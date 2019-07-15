use hyper::rt::{Future, Stream};
use hyper::service::service_fn;
use hyper::{Body, Method, Request, Response, Server, StatusCode};
use signal_hook::{self, iterator::Signals};
use tokio::runtime::current_thread;

use std::borrow::Cow;
use std::cell::RefCell;
use std::path::Path;
use std::rc::Rc;

use horrorshow::{self, Render, RenderOnce, Template};

use self::web::{EndPoint, FromRequestError, FromRequestOk};
use relations::{Atom, Database, Element, ElementRef, Index, Ref, Relation};
use utils::{remove_prefix, Map};

/******************************************************************************
 * Wiki runtime system.
 * Based on hyper/tokio, but uses the single threaded tokio runtime.
 */

/// Wiki web interface state.
struct State {
    database: RefCell<Database>,
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
        service_fn(move |request| {
            // Move cloned rc ref in this scope.
            let handlers = [
                web::end_point_handler::<ListAllElements>,
                web::end_point_handler::<DisplayElement>,
                web::end_point_handler::<CreateAtom>,
                web::end_point_handler::<StaticAsset>,
            ];
            web::handle_request(request, state.clone(), handlers.iter())
        })
    };
    let server = Server::bind(&addr)
        .executor(current_thread::TaskExecutor::current())
        .serve(create_service);

    let shutdown_signal = Signals::new(&[signal_hook::SIGTERM, signal_hook::SIGINT])
        .expect("Signal handler setup")
        .into_async() // Stream of signals
        .expect("Signal handler to async")
        .into_future(); // Only look at first signal

    let wiki = server.with_graceful_shutdown(shutdown_signal.map(|_| ()));
    current_thread::block_on_all(wiki).expect("Runtime error");
    super::write_database_to_file(database_file, &state.database.borrow());
    eprintln!("Database saved to {}", database_file.display());
}

/******************************************************************************
 * Wiki page definitions.
 */

/* Design:
 *
 * Link creation:
 * buttons to start creating a link from/to a normal display page.
 * cancel + build button if all requirements are filled
 *
 * Abstract creation: Same with optional name field.
 *
 * Removal TODO
 */

#[derive(Clone)]
struct EditState {
    // Relation
    subject: Option<Index>,
    descriptor: Option<Index>,
    complement: Option<Index>,
}
impl web::QueryFormat for EditState {
    fn to_query(&self, builder: &mut web::PathQueryBuilder) {
        builder.optional_entry("subject", self.subject);
        builder.optional_entry("descriptor", self.descriptor);
        builder.optional_entry("complement", self.complement);
    }
}
impl EditState {
    fn empty() -> Self {
        EditState {
            subject: None,
            descriptor: None,
            complement: None,
        }
    }
    fn from_query<'a>(
        query: &Map<Cow<'a, str>, Cow<'a, str>>,
    ) -> Result<Self, std::num::ParseIntError> {
        Ok(EditState {
            subject: query.get("subject").map(|s| s.parse()).transpose()?,
            descriptor: query.get("descriptor").map(|s| s.parse()).transpose()?,
            complement: query.get("complement").map(|s| s.parse()).transpose()?,
        })
    }
}

/// Display an element of the relation graph.
struct DisplayElement {
    index: Index,
    edit_state: EditState,
}
impl DisplayElement {
    fn new(index: Index, edit_state: &EditState) -> Self {
        DisplayElement {
            index: index,
            edit_state: edit_state.clone(),
        }
    }
}
impl EndPoint for DisplayElement {
    type State = State;
    fn url(&self) -> String {
        web::to_path_and_query(format!("/element/{}", self.index), &self.edit_state)
    }
    fn from_request(r: Request<Body>) -> Result<FromRequestOk<Self>, FromRequestError> {
        match (r.method(), remove_prefix(r.uri().path(), "/element/")) {
            (&Method::GET, Some(index)) => {
                let query = web::decode_optional_query_entries(r.uri().query())?;
                Ok(FromRequestOk::Value(DisplayElement {
                    index: index.parse()?,
                    edit_state: EditState::from_query(&query)?,
                }))
            }
            _ => Err(FromRequestError::NoMatch(r)),
        }
    }
    fn generate_response(self, state: &State) -> Response<Body> {
        match state.database.borrow().element(self.index) {
            Ok(element) => {
                web::response_ok(display_element_page_content(element, &self.edit_state))
            }
            Err(_) => web::response_empty_404(),
        }
    }
}
fn display_element_page_content(element: Ref<Element>, edit_state: &EditState) -> String {
    let _nav = html! {};
    let name = element_name(element);
    let content = html! {
        h1(class=css_class_name(element)) : &name;
        ul {
            @ for relation in element.subject_of().iter() {
                li : relation_link(relation, edit_state);
            }
            @ for relation in element.descriptor_of().iter() {
                li : relation_link(relation, edit_state);
            }
            @ for relation in element.complement_of().iter() {
                li : relation_link(relation, edit_state);
            }
        }
    };
    compose_wiki_page(&name, content, edit_state)
}

/// List all elements
struct ListAllElements {
    edit_state: EditState,
}
impl EndPoint for ListAllElements {
    type State = State;
    fn url(&self) -> String {
        web::to_path_and_query("/all", &self.edit_state)
    }
    fn from_request(r: Request<Body>) -> Result<FromRequestOk<Self>, FromRequestError> {
        match (r.method(), r.uri().path()) {
            (&Method::GET, "/all") => {
                let query = web::decode_optional_query_entries(r.uri().query())?;
                Ok(FromRequestOk::Value(ListAllElements {
                    edit_state: EditState::from_query(&query)?,
                }))
            }
            _ => Err(FromRequestError::NoMatch(r)),
        }
    }
    fn generate_response(self, state: &State) -> Response<Body> {
        let database = &state.database.borrow();
        let edit_state = &self.edit_state;
        let content = html! {
            @ for element in database.iter() {
                p {
                    a(href=DisplayElement::new(element.index(), edit_state).url(), class=css_class_name(element)) {
                        : element.index();
                        : " ";
                        : element_name(element);
                    }
                }
            }
        };
        let page = compose_wiki_page(lang::ALL_ELEMENTS, content, edit_state);
        web::response_ok(page)
    }
}

/// Create an atom.
enum CreateAtom {
    Get { edit_state: EditState },
    Post,
}
impl EndPoint for CreateAtom {
    type State = State;
    fn url(&self) -> String {
        String::from("/create/atom")
    }
    fn from_request(r: Request<Body>) -> Result<FromRequestOk<Self>, FromRequestError> {
        match (r.method(), r.uri().path()) {
            (&Method::GET, "/create/atom") => {
                let query = web::decode_optional_query_entries(r.uri().query())?;
                Ok(FromRequestOk::Value(CreateAtom::Get {
                    edit_state: EditState::from_query(&query)?,
                }))
            }
            (&Method::POST, "/create/atom") => {
                eprintln!("CreateAtomPost: {:?}", r); //FIXME body is a future::Stream... cannot match it there
                Ok(FromRequestOk::Value(CreateAtom::Post))
            }
            _ => Err(FromRequestError::NoMatch(r)),
        }
    }
    fn generate_response(self, _state: &State) -> Response<Body> {
        match self {
            CreateAtom::Get { edit_state } => {
                let content = html! {
                    form(method="post") {
                        : "Value:";
                        input(type="text", name="text");
                        input(type="submit", value="Create");
                    }
                };
                let page = compose_wiki_page(lang::CREATE_ATOM, content, &edit_state);
                web::response_ok(page)
            }
            CreateAtom::Post => unimplemented!(),
        }
    }
}

//FIXME text use lang
fn relation_link<'a>(relation: Ref<'a, Relation>, edit_state: &'a EditState) -> impl Render + 'a {
    owned_html! {
        a(href=DisplayElement::new(relation.index(),edit_state).url(), class="relation") : format!("Element {}", relation.index());
    }
}
fn element_name(element: Ref<Element>) -> String {
    match element.cases() {
        ElementRef::Atom(r) => match r.value() {
            Atom::Text(s) => s.clone(),
        },
        ElementRef::Abstract(r) => {
            if let Some(is_named_atom_index) =
                element.database().index_of_text_atom(lang::NAMED_ATOM)
            {
                let name = r.subject_of().iter().find_map(|r| {
                    if r.descriptor().index() == is_named_atom_index {
                        r.complement()
                    } else {
                        None
                    }
                });
                if let Some(name) = name {
                    return element_name(name);
                }
            }
            format!("Abstrait_{}", element.index())
        }
        ElementRef::Relation(r) => {
            // TODO naming of (s,d,c) without relation recursion
            let r = r.value();
            match r.complement {
                Some(c) => format!("Relation: {} {} {}", r.subject, r.descriptor, c),
                None => format!("Relation {} {}", r.subject, r.descriptor),
            }
        }
    }
}
fn css_class_name(element: Ref<Element>) -> &'static str {
    match element.value() {
        Element::Abstract => "abstract",
        Element::Atom(_) => "atom",
        Element::Relation(_) => "relation",
    }
}

/// Generated wiki page final assembly. Adds the navigation bar, overall html structure.
fn compose_wiki_page<T, C>(title: T, content: C, edit_state: &EditState) -> String
where
    T: RenderOnce,
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
                    a(href=ListAllElements{edit_state: edit_state.clone()}.url()) : lang::ALL_ELEMENTS;
                    a(href=CreateAtom::Get{edit_state: edit_state.clone()}.url(), class="atom") : lang::CREATE_ATOM;
                    a(href="/create/abstract", class="abstract") : lang::CREATE_ABSTRACT;
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
struct StaticAsset {
    path: String,
}
impl<T: Into<String>> From<T> for StaticAsset {
    fn from(v: T) -> Self {
        StaticAsset { path: v.into() }
    }
}
impl EndPoint for StaticAsset {
    type State = State;
    fn url(&self) -> String {
        format!("/static/{}", self.path)
    }
    fn from_request(r: Request<Body>) -> Result<FromRequestOk<Self>, FromRequestError> {
        match (r.method(), remove_prefix(r.uri().path(), "/static/")) {
            (&Method::GET, Some(path)) => Ok(FromRequestOk::Value(StaticAsset::from(path))),
            _ => Err(FromRequestError::NoMatch(r)),
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
            None => web::response_empty_404(),
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
 * Language.
 */
mod lang {
    pub const ALL_ELEMENTS: &'static str = "Éléments";
    pub const CREATE_ATOM: &'static str = "Atome...";
    pub const CREATE_ABSTRACT: &'static str = "Abstrait...";
    pub const NAMED_ATOM: &'static str = "est nommé";
}

/******************************************************************************
 * Wiki web related utils.
 */

/// Web related utilities
mod web {
    use std::borrow::Cow;
    use utils::Map;

    use hyper::rt::Future;
    use hyper::{Body, Request, Response, StatusCode};
    use percent_encoding::{percent_decode, utf8_percent_encode, QUERY_ENCODE_SET};
    use std::fmt::{self, Write};
    use std::rc::Rc;
    use tokio::prelude::future;

    pub type BoxedFuture<T> = Box<dyn Future<Item = T, Error = hyper::Error>>;

    /// Interface for an URL endpoint.
    pub trait EndPoint: Sized {
        type State: ?Sized;
        fn url(&self) -> String;
        fn from_request(request: Request<Body>) -> Result<FromRequestOk<Self>, FromRequestError>;
        fn generate_response(self, state: &Self::State) -> Response<Body>;
    }

    /// Result of from_request: either a direct value, or a future.
    pub enum FromRequestOk<T> {
        Value(T),
        Future(BoxedFuture<T>),
    }
    /// Error code used for routing.
    /// NoMatch returns the request so that it can be used by other handlers.
    /// BadRequest is used to stop matching with an error.
    pub enum FromRequestError {
        NoMatch(Request<Body>),
        BadRequest,
    }
    /// Convenience conversion which allows to use '?' in from_request() implementations.
    impl<E: std::error::Error> From<E> for FromRequestError {
        fn from(_e: E) -> Self {
            FromRequestError::BadRequest
        }
    }

    pub fn end_point_handler<E: EndPoint + 'static>(
        request: Request<Body>,
        state: Rc<E::State>,
    ) -> Result<BoxedFuture<Response<Body>>, FromRequestError> {
        E::from_request(request).map(move |ok_value| {
            let response_future: BoxedFuture<Response<Body>> = match ok_value {
                FromRequestOk::Value(v) => {
                    Box::new(future::ok(v.generate_response(state.as_ref())))
                }
                FromRequestOk::Future(f) => Box::new(f.then(move |end_point_value| {
                    match end_point_value {
                        Ok(e) => Ok(e.generate_response(state.as_ref())),
                        Err(_) => Ok(Response::builder()
                            .status(StatusCode::BAD_REQUEST)
                            .body(Body::empty())
                            .unwrap()),
                    }
                })),
            };
            response_future
        })
    }

    /// Apply the first matching handler, or generate an error reponse (400 or 404).
    pub fn handle_request<S, I>(
        request: Request<Body>,
        state: Rc<S>,
        handlers: I,
    ) -> BoxedFuture<Response<Body>>
    where
        I: Iterator,
        <I as Iterator>::Item:
            Fn(Request<Body>, Rc<S>) -> Result<BoxedFuture<Response<Body>>, FromRequestError>,
    {
        let tried_all_handlers = handlers.fold(
            Err(FromRequestError::NoMatch(request)),
            |a, handler| match a {
                Err(FromRequestError::NoMatch(r)) => handler(r, state.clone()),
                a => a,
            },
        );
        match tried_all_handlers {
            Ok(response) => response,
            Err(e) => Box::new(future::ok(match e {
                FromRequestError::NoMatch(_) => response_empty_404(),
                FromRequestError::BadRequest => Response::builder()
                    .status(StatusCode::BAD_REQUEST)
                    .body(Body::empty())
                    .unwrap(),
            })),
        }
    }

    /// Create an ok response with a body.
    pub fn response_ok<B: Into<Body>>(body: B) -> Response<Body> {
        Response::builder()
            .status(StatusCode::OK)
            .body(body.into())
            .unwrap()
    }
    /// Create an empty 404 response.
    pub fn response_empty_404() -> Response<Body> {
        Response::builder()
            .status(StatusCode::NOT_FOUND)
            .body(Body::empty())
            .unwrap()
    }

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

    /// Object can be represented as a query string.
    pub trait QueryFormat: Sized {
        fn to_query(&self, query: &mut PathQueryBuilder);
    }

    pub fn to_path_and_query<P: Into<String>, Q: QueryFormat>(path: P, query: &Q) -> String {
        let mut builder = PathQueryBuilder::new(path.into());
        query.to_query(&mut builder);
        builder.build()
    }

    /// T as a url encoded representation.
    pub struct QueryFormattedValue<T>(T);
    impl fmt::Display for QueryFormattedValue<usize> {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            self.0.fmt(f)
        }
    }
    impl<'a> fmt::Display for QueryFormattedValue<&'a str> {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            utf8_percent_encode(self.0, QUERY_ENCODE_SET).fmt(f)
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
            QueryFormattedValue<K>: fmt::Display,
            QueryFormattedValue<V>: fmt::Display,
        {
            write!(
                &mut self.path_and_query,
                "{}{}={}",
                self.query_prefix_char,
                QueryFormattedValue(key),
                QueryFormattedValue(value)
            )
            .unwrap();
            self.query_prefix_char = '&' // Entries are ?first=v&second=v&...
        }
        pub fn optional_entry<K, V>(&mut self, key: K, value: Option<V>)
        where
            QueryFormattedValue<K>: fmt::Display,
            QueryFormattedValue<V>: fmt::Display,
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
