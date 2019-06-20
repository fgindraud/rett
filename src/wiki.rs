use hyper::rt::{Future, Stream};
use hyper::service::service_fn_ok;
use hyper::{Body, Method, Request, Response, Server, StatusCode};
use signal_hook::{self, iterator::Signals};
use tokio::runtime::current_thread;

use std::borrow::Cow;
use std::cell::RefCell;
use std::path::Path;
use std::rc::Rc;

use horrorshow::{self, Render, RenderOnce, Template};

use relations::{Atom, Database, Element, Index, Ref, Relation};
use utils::remove_prefix;

/******************************************************************************
 * Wiki runtime system.
 * Based on hyper/tokio, but uses the single threaded tokio runtime.
 */

/// Wiki web interface state.
struct State {
    database: RefCell<Database>,
}

/// Interface for an URL endpoint.
trait EndPoint<'r>
where
    Self: Sized + 'r,
{
    fn url(&self) -> String;
    fn from_request(request: &'r Request<Body>) -> Result<Self, FromRequestError>;
    fn generate_response(self, state: &State) -> Response<Body>;
}

/// Error code used for routing. BadRequest is used to stop matching with an error.
enum FromRequestError {
    NoMatch,
    BadRequest(Box<std::error::Error>),
}
/// Convenience conversion which allows to use '?' in from_request() implementations.
impl<E: Into<Box<std::error::Error>>> From<E> for FromRequestError {
    fn from(e: E) -> Self {
        FromRequestError::BadRequest(e.into())
    }
}

/// Entry point, run the wiki server.
pub fn run(addr: &str, database_file: &Path) {
    let addr = addr.parse().expect("Address::parse");

    let database = super::read_database_from_file(database_file);
    let state = Rc::new(State {
        database: RefCell::new(database),
    });

    let create_service = || {
        let state = state.clone();
        service_fn_ok(move |request| {
            // Move cloned rc ref in this scope.
            let handlers = [
                end_point_handler::<DisplayElement>,
                end_point_handler::<StaticAsset>, //
            ];
            process_request(&request, &state, handlers.iter())
        })
    };
    let server = Server::bind(&addr)
        .executor(current_thread::TaskExecutor::current())
        .serve(create_service);

    let shutdown_signal = Signals::new(&[signal_hook::SIGTERM, signal_hook::SIGINT])
        .expect("Signal handler setup")
        .into_async()
        .expect("Signal handler to async")
        .into_future();

    let wiki = server.with_graceful_shutdown(shutdown_signal.map(|_| ()));
    current_thread::block_on_all(wiki).expect("Runtime error");
    super::write_database_to_file(database_file, &state.database.borrow());
    eprintln!("Database saved to {}", database_file.display());
}

fn end_point_handler<'r, E: EndPoint<'r>>(
    request: &'r Request<Body>,
    state: &State,
) -> Result<Response<Body>, FromRequestError> {
    E::from_request(request).map(|e| e.generate_response(state))
}

/// Apply the first matching handler, or generate an error reponse (400 or 404).
fn process_request<'r, 's, I>(
    request: &'r Request<Body>,
    state: &'s State,
    handlers: I,
) -> Response<Body>
where
    I: Iterator,
    <I as Iterator>::Item:
        Fn(&'r Request<Body>, &'s State) -> Result<Response<Body>, FromRequestError>,
{
    for handler in handlers {
        match handler(request, state) {
            Ok(response) => return response,
            Err(FromRequestError::NoMatch) => (),
            Err(FromRequestError::BadRequest(e)) => {
                return Response::builder()
                    .status(StatusCode::BAD_REQUEST)
                    .body(Body::from(e.to_string()))
                    .unwrap();
            }
        }
    }
    Response::builder()
        .status(StatusCode::NOT_FOUND)
        .body(Body::empty())
        .unwrap()
}

/******************************************************************************
 * Wiki page definitions.
 */

/* Design:
 *
 * In //:
 * - if stop signal, gracefully shutdown + save db
 * - if request, build page
 *
 * -> need some router-like small tool, see RegexSet
 *
 * Pages:
 * - display for any index
 * - atom creation
 * - abstract creation
 * - link creation:
 *   - buttons to start creating a link from/to a normal display page.
 *   - add get-type params to represent partial state (?link_to=x&...)
 *   - cancel + build button if all requirements are filled
 *
 * Removal TODO
 */

/// Display an element of the relation graph.
struct DisplayElement {
    index: Index,
    // Temporary selection for link creation
    link_from: Option<Index>,
    link_to: Option<Index>,
    link_tag: Option<Index>,
}
impl<'r> EndPoint<'r> for DisplayElement {
    fn url(&self) -> String {
        let mut b = uri::PathQueryBuilder::new(format!("/element/{}", self.index));
        b.optional_entry("link_from", self.link_from);
        b.optional_entry("link_to", self.link_to);
        b.optional_entry("link_tag", self.link_tag);
        b.build()
    }
    fn from_request(r: &'r Request<Body>) -> Result<Self, FromRequestError> {
        match (r.method(), remove_prefix(r.uri().path(), "/element/")) {
            (&Method::GET, Some(index)) => {
                let query = uri::decode_optional_query_entries(r.uri().query())?;
                Ok(DisplayElement {
                    index: index.parse()?,
                    link_from: query.get("link_from").map(|s| s.parse()).transpose()?,
                    link_to: query.get("link_to").map(|s| s.parse()).transpose()?,
                    link_tag: query.get("link_tag").map(|s| s.parse()).transpose()?,
                })
            }
            _ => Err(FromRequestError::NoMatch),
        }
    }
    fn generate_response(self, state: &State) -> Response<Body> {
        match state.database.borrow().element(self.index) {
            Ok(element) => Response::builder()
                .status(StatusCode::OK)
                .body(Body::from(display_element_page_content(element)))
                .unwrap(),
            Err(_) => Response::builder()
                .status(StatusCode::NOT_FOUND)
                .body(Body::empty())
                .unwrap(),
        }
    }
}
fn display_element_page_content(element: Ref<Element>) -> String {
    let nav = html! {};
    let name = match element.value() {
        Element::Abstract => format!("Abstract {}", element.index()),
        Element::Atom(a) => match a {
            Atom::Text(s) => format!("Atom: \"{}\"", s),
        },
        Element::Relation(r) => match r.complement {
            Some(c) => format!("Relation: {} {} {}", r.subject, r.descriptor, c),
            None => format!("Relation {} {}", r.subject, r.descriptor),
        },
    };
    let content = html! {
        h1(class=css_class_name(element)) : &name;
        h2 : "Subject of";
        ul {
            @ for relation in element.subject_of().iter() {
                li : relation_link(relation);
            }
        }
        h2 : "Descriptor of";
        ul {
            @ for relation in element.descriptor_of().iter() {
                li : relation_link(relation);
            }
        }
        h2 : "Complement of";
        ul {
            @ for relation in element.complement_of().iter() {
                li : relation_link(relation);
            }
        }
    };
    compose_wiki_page(&name, nav, content)
}

fn relation_link(relation: Ref<Relation>) -> impl Render {
    relation.index()
}
fn css_class_name(element: Ref<Element>) -> &'static str {
    match element.value() {
        Element::Abstract => "abstract",
        Element::Atom(_) => "atom",
        Element::Relation(_) => "relation",
    }
}

/// Generated wiki page final assembly. Adds the navigation bar, overall html structure.
fn compose_wiki_page<T, N, C>(title: T, additional_nav_links: N, content: C) -> String
where
    T: RenderOnce,
    N: RenderOnce,
    C: RenderOnce,
{
    let template = html! {
        : horrorshow::helper::doctype::HTML;
        html {
            head {
                meta(charset="UTF-8");
                link(rel="stylesheet", type="text/css", href=StaticAsset::from("style.css").url());
                meta(name="viewport", content="width=device-width, initial-scale=1.0");
                title : title;
            }
            body {
                nav {
                    a(href="/") : "Home";
                    a(href="/create/atom", class="atom") : "Atom";
                    a(href="/create/abstract", class="abstract") : "Abstract";
                    : additional_nav_links;
                    a(href="/all") : "All";
                    // TODO other
                }
                main {
                    : content;
                }
                script(src=StaticAsset::from("client.js").url()) {}
            }
        }
    };
    template.into_string().unwrap()
}

/******************************************************************************
 * Wiki static files.
 * Do not depend on page generation.
 * Static files are stored externally for development (easier to edit).
 * Their content is statically embedded to remove file dependencies at runtime.
 */

/// Static files.
struct StaticAsset<'r> {
    path: Cow<'r, str>,
}
impl<'r, T: Into<Cow<'r, str>>> From<T> for StaticAsset<'r> {
    fn from(v: T) -> Self {
        StaticAsset { path: v.into() }
    }
}
impl<'r> EndPoint<'r> for StaticAsset<'r> {
    fn url(&self) -> String {
        format!("/static/{}", self.path)
    }
    fn from_request(r: &'r Request<Body>) -> Result<Self, FromRequestError> {
        match (r.method(), remove_prefix(r.uri().path(), "/static/")) {
            (&Method::GET, Some(path)) => Ok(path.into()),
            _ => Err(FromRequestError::NoMatch),
        }
    }
    fn generate_response(self, _state: &State) -> Response<Body> {
        match ASSETS.iter().find(|asset| asset.path == self.path) {
            Some(asset) => Response::builder()
                .status(StatusCode::OK)
                .header("Content-Type", asset.mime)
                .header("Cache-Control", "public, max-age=3600") // Allow cache for 1h
                .body(Body::from(asset.content))
                .unwrap(),
            None => Response::builder()
                .status(StatusCode::NOT_FOUND)
                .body(Body::empty())
                .unwrap(),
        }
    }
}
struct AssetDefinition {
    path: &'static str,
    mime: &'static str,
    content: &'static str,
}
const ASSETS: [AssetDefinition; 2] = [
    AssetDefinition {
        path: "style.css",
        mime: "text/css; charset=utf8",
        content: include_str!("wiki_assets/style.css"),
    },
    AssetDefinition {
        path: "client.js",
        mime: "application/javascript",
        content: include_str!("wiki_assets/client.js"),
    },
];

/******************************************************************************
 * Wiki runtime utils.
 */

/// Uri related utilities
mod uri {
    use percent_encoding::{percent_decode, utf8_percent_encode, PercentEncode, QUERY_ENCODE_SET};
    use relations;
    use std::borrow::Cow;
    use std::fmt::{self, Write};
    use utils::Map;

    #[derive(Debug)]
    pub enum ParsingError {
        Utf8,
        Structure,
    }
    impl fmt::Display for ParsingError {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self {
                ParsingError::Utf8 => "query is not valid utf8".fmt(f),
                ParsingError::Structure => "bad query structure".fmt(f),
            }
        }
    }
    impl std::error::Error for ParsingError {}

    /// Convertible to something printable in a query string
    pub trait QueryFormat {
        // This trait is an optimization over encode(to_string(T)) for some T.
        type Output: fmt::Display;
        fn to_query_format(&self) -> Self::Output;
    }
    impl QueryFormat for relations::Index {
        type Output = relations::Index;
        fn to_query_format(&self) -> Self::Output {
            *self // Integers do not need URL-encoding
        }
    }
    impl<'a> QueryFormat for &'a str {
        type Output = PercentEncode<'a, QUERY_ENCODE_SET>;
        fn to_query_format(&self) -> Self::Output {
            utf8_percent_encode(self, QUERY_ENCODE_SET) // Text needs URL-encoding
        }
    }

    /// Build the Path and Query path of an Uri incrementally with a builder pattern.
    pub struct PathQueryBuilder {
        path_and_query: String,
        query_prefix_char: char,
    }
    impl PathQueryBuilder {
        pub fn new(path: String) -> Self {
            PathQueryBuilder {
                path_and_query: path,
                query_prefix_char: '?',
            }
        }
        pub fn build(self) -> String {
            self.path_and_query
        }
        pub fn entry<K, V>(&mut self, key: K, value: V)
        where
            K: QueryFormat,
            V: QueryFormat,
        {
            write!(
                &mut self.path_and_query,
                "{}{}={}",
                self.query_prefix_char,
                key.to_query_format(),
                value.to_query_format()
            )
            .unwrap();
            self.query_prefix_char = '&' // Entries are ?first=v&second=v&...
        }
        pub fn optional_entry<K, V>(&mut self, key: K, value: Option<V>)
        where
            K: QueryFormat,
            V: QueryFormat,
        {
            if let Some(v) = value {
                self.entry(key, v)
            }
        }
    }

    /// Create a Map: str -> str from a query string.
    pub fn decode_query_entries<'a>(
        raw_query: &'a str,
    ) -> Result<Map<Cow<'a, str>, Cow<'a, str>>, ParsingError> {
        raw_query
            .split('&')
            .map(|entry| {
                let mut it = entry.split('=');
                let fields = [it.next(), it.next(), it.next()];
                match fields {
                    [Some(key), Some(value), None] => {
                        let decode = |s| {
                            percent_decode(s)
                                .decode_utf8()
                                .map_err(|_| ParsingError::Utf8)
                        };
                        Ok((decode(key.as_bytes())?, decode(value.as_bytes())?))
                    }
                    _ => Err(ParsingError::Structure),
                }
            })
            .collect()
    }
    /// Create a Map: str -> str from a query string, or default with an empty map.
    pub fn decode_optional_query_entries<'a>(
        raw_query: Option<&'a str>,
    ) -> Result<Map<Cow<'a, str>, Cow<'a, str>>, ParsingError> {
        match raw_query {
            Some(s) => decode_query_entries(s),
            None => Ok(Map::new()),
        }
    }
}
