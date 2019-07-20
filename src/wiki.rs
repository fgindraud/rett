use hyper::rt::{Future, Stream};
use hyper::service::service_fn;
use hyper::{Body, Method, Request, Response, Server, StatusCode};
use signal_hook::{self, iterator::Signals};
use tokio::runtime::current_thread;

use std::cell::RefCell;
use std::path::Path;
use std::rc::Rc;

use horrorshow::{self, RenderOnce, Template};

use self::web::{remove_prefix, EndPoint, FromRequestError, FromRequestOk};
use relations::{Atom, Database, Element, ElementRef, Index, Ref, Relation};

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
                web::end_point_handler::<Homepage>,
                web::end_point_handler::<ListAllElements>,
                web::end_point_handler::<DisplayElement>,
                web::end_point_handler::<CreateAtom>,
                web::end_point_handler::<CreateAbstract>,
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

/* Link creation:
 * buttons to start creating a link from/to a normal display page.
 * cancel + build button if all requirements are filled
 *
 * Removal TODO
 */

// TODO copy for convenience with url generation. should be reworked one day
#[derive(Clone, Copy)]
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
    fn from_query(entries: &web::UrlDecodedEntries) -> Result<Self, web::Error> {
        let parse_index = |s: &str| s.parse::<usize>().map_err(|_| web::Error::BadRequest);
        Ok(EditState {
            subject: entries.get("subject").map(parse_index).transpose()?,
            descriptor: entries.get("descriptor").map(parse_index).transpose()?,
            complement: entries.get("complement").map(parse_index).transpose()?,
        })
    }
}

/// Display an element of the relation graph.
struct DisplayElement {
    index: Index,
    edit_state: EditState,
}
impl EndPoint for DisplayElement {
    type State = State;
    fn url(&self) -> String {
        web::to_path_and_query(format!("/element/{}", self.index), &self.edit_state)
    }
    fn from_request(r: Request<Body>) -> Result<FromRequestOk<Self>, FromRequestError> {
        match (r.method(), remove_prefix(r.uri().path(), "/element/")) {
            (&Method::GET, Some(index)) => Ok(FromRequestOk::Value(DisplayElement {
                index: index.parse().map_err(|_| web::Error::BadRequest)?,
                edit_state: web::from_query(r.uri().query())?,
            })),
            _ => Err(FromRequestError::NoMatch(r)),
        }
    }
    fn generate_response(self, state: &State) -> Response<Body> {
        match state.database.borrow().element(self.index) {
            Ok(element) => {
                web::response_html(display_element_page_content(element, self.edit_state))
            }
            Err(_) => web::response_empty_404(),
        }
    }
}
fn display_element_page_content(element: Ref<Element>, edit_state: EditState) -> String {
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
    let nav = navigation_links(edit_state, Some(element.index()));
    compose_wiki_page(&name, content, nav)
}

/// Homepage : links to selected elements.
struct Homepage {
    edit_state: EditState,
}
impl EndPoint for Homepage {
    type State = State;
    fn url(&self) -> String {
        web::to_path_and_query("/", &self.edit_state)
    }
    fn from_request(r: Request<Body>) -> Result<FromRequestOk<Self>, FromRequestError> {
        match (r.method(), r.uri().path()) {
            (&Method::GET, "/") => Ok(FromRequestOk::Value(Homepage {
                edit_state: web::from_query(r.uri().query())?,
            })),
            _ => Err(FromRequestError::NoMatch(r)),
        }
    }
    fn generate_response(self, state: &State) -> Response<Body> {
        let database = &state.database.borrow();
        let edit_state = self.edit_state;
        let content = html! {
            h1 : lang::HOMEPAGE;
            @ if let Some(wiki_homepage) = database.get_text_atom("_wiki_homepage") {
                ul {
                    @ for tagged in wiki_homepage.descriptor_of().iter().map(|tag_relation| tag_relation.subject()) {
                        li {
                            a(href=DisplayElement{ index: tagged.index(), edit_state }.url(), class=css_class_name(tagged)) {
                                : tagged.index();
                                : " ";
                                : element_name(tagged);
                            }
                        }
                    }
                }
            }
            form(method="post", action=CreateAtom::Get{ edit_state }.url(), class="hbox") {
                p(class="fill_box") : lang::HOMEPAGE_HELP;
                button(type="submit") : "_wiki_homepage";
                input(type="hidden", name="text", value="_wiki_homepage");
            }
        };
        let nav = navigation_links(self.edit_state, None);
        let page = compose_wiki_page(lang::HOMEPAGE, content, nav);
        web::response_html(page)
    }
}

/// List all elements.
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
            (&Method::GET, "/all") => Ok(FromRequestOk::Value(ListAllElements {
                edit_state: web::from_query(r.uri().query())?,
            })),
            _ => Err(FromRequestError::NoMatch(r)),
        }
    }
    fn generate_response(self, state: &State) -> Response<Body> {
        let database = &state.database.borrow();
        let edit_state = self.edit_state;
        let content = html! {
            h1 : lang::ALL_ELEMENTS_TITLE;
            @ for element in database.iter() {
                p {
                    a(href=DisplayElement{ index: element.index(), edit_state }.url(), class=css_class_name(element)) {
                        : element.index();
                        : " ";
                        : element_name(element);
                    }
                }
            }
        };
        let nav = navigation_links(edit_state, None);
        let page = compose_wiki_page(lang::ALL_ELEMENTS_TITLE, content, nav);
        web::response_html(page)
    }
}

/// Create an atom.
enum CreateAtom {
    Get { edit_state: EditState },
    Post { text: String, edit_state: EditState },
}
impl EndPoint for CreateAtom {
    //TODO implement preview button: fuzzy search current text content and propose similar atoms
    type State = State;
    fn url(&self) -> String {
        //FIXME EndPoint trait works great for GET but not for POST...
        let edit_state = match self {
            CreateAtom::Get { edit_state } => edit_state,
            CreateAtom::Post { edit_state, .. } => edit_state,
        };
        web::to_path_and_query("/create/atom", edit_state)
    }
    fn from_request(r: Request<Body>) -> Result<FromRequestOk<Self>, FromRequestError> {
        match (r.method(), r.uri().path()) {
            (&Method::GET, "/create/atom") => Ok(FromRequestOk::Value(CreateAtom::Get {
                edit_state: web::from_query(r.uri().query())?,
            })),
            (&Method::POST, "/create/atom") => {
                let edit_state = web::from_query(r.uri().query())?;
                web::with_post_entries(r, move |entries| {
                    let text = entries.get("text").ok_or(web::Error::BadRequest)?;
                    let text = text.to_string();
                    Ok(CreateAtom::Post { text, edit_state })
                })
            }
            _ => Err(FromRequestError::NoMatch(r)),
        }
    }
    fn generate_response(self, state: &State) -> Response<Body> {
        match self {
            CreateAtom::Get { edit_state } => {
                let content = html! {
                    h1(class="atom") : lang::CREATE_ATOM_TITLE;
                    form(method="post", class="vbox") {
                        input(type="text", name="text", required, placeholder=lang::ATOM_TEXT);
                        div(class="hbox") {
                            button(type="submit", name="preview") : lang::PREVIEW_BUTTON;
                            button(type="submit", name="create") : lang::COMMIT_BUTTON;
                        }
                    }
                };
                let nav = navigation_links(edit_state, None);
                let page = compose_wiki_page(lang::CREATE_ATOM_TITLE, content, nav);
                web::response_html(page)
            }
            CreateAtom::Post { text, edit_state } => {
                let index = state.database.borrow_mut().insert_atom(Atom::from(text));
                let uri = DisplayElement { index, edit_state }.url();
                web::response_redirection(&uri)
            }
        }
    }
}

/// Create an atom.
enum CreateAbstract {
    Get {
        edit_state: EditState,
    },
    Post {
        name: Option<String>,
        edit_state: EditState,
    },
}
impl EndPoint for CreateAbstract {
    type State = State;
    fn url(&self) -> String {
        let edit_state = match self {
            CreateAbstract::Get { edit_state } => edit_state,
            CreateAbstract::Post { edit_state, .. } => edit_state,
        };
        web::to_path_and_query("/create/abstract", edit_state)
    }
    fn from_request(r: Request<Body>) -> Result<FromRequestOk<Self>, FromRequestError> {
        match (r.method(), r.uri().path()) {
            (&Method::GET, "/create/abstract") => Ok(FromRequestOk::Value(CreateAbstract::Get {
                edit_state: web::from_query(r.uri().query())?,
            })),
            (&Method::POST, "/create/abstract") => {
                let edit_state = web::from_query(r.uri().query())?;
                web::with_post_entries(r, move |entries| {
                    let name = entries.get("name").ok_or(web::Error::BadRequest)?;
                    let name = match name {
                        "" => None,
                        _ => Some(name.to_string()),
                    };
                    Ok(CreateAbstract::Post { name, edit_state })
                })
            }
            _ => Err(FromRequestError::NoMatch(r)),
        }
    }
    fn generate_response(self, state: &State) -> Response<Body> {
        match self {
            CreateAbstract::Get { edit_state } => {
                let content = html! {
                    h1(class="atom") : lang::CREATE_ABSTRACT_TITLE;
                    form(method="post", class="vbox") {
                        input(type="text", name="name", placeholder=lang::CREATE_ABSTRACT_NAME_PLACEHOLDER);
                        div(class="hbox") {
                            button(type="submit", name="preview") : lang::PREVIEW_BUTTON;
                            button(type="submit", name="create") : lang::COMMIT_BUTTON;
                        }
                    }
                };
                let nav = navigation_links(edit_state, None);
                let page = compose_wiki_page(lang::CREATE_ABSTRACT_TITLE, content, nav);
                web::response_html(page)
            }
            CreateAbstract::Post { name, edit_state } => {
                let database = &mut state.database.borrow_mut();
                let index = database.create_abstract_element();
                if let Some(name) = name {
                    let name_element = database.insert_atom(Atom::from(name));
                    let is_named_atom = database.insert_atom(Atom::from(lang::NAMED_ATOM));
                    let _naming_relation = database.insert_relation(Relation {
                        subject: index,
                        descriptor: is_named_atom,
                        complement: Some(name_element),
                    });
                }
                let uri = DisplayElement { index, edit_state }.url();
                web::response_redirection(&uri)
            }
        }
    }
}

//FIXME text use lang
fn relation_link<'a>(relation: Ref<'a, Relation>, edit_state: EditState) -> impl RenderOnce + 'a {
    owned_html! {
        a(href=DisplayElement{ index: relation.index(), edit_state }.url(), class="relation") : format!("Element {}", relation.index());
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

fn navigation_links<'a>(edit_state: EditState, current_element: Option<Index>) -> impl RenderOnce {
    owned_html! {
        a(href=Homepage{ edit_state }.url()) : lang::HOMEPAGE;
        a(href=ListAllElements{ edit_state }.url()) : lang::ALL_ELEMENTS_NAV;
        a(href=CreateAtom::Get{ edit_state }.url(), class="atom") : lang::CREATE_ATOM_NAV;
        a(href=CreateAbstract::Get{ edit_state }.url(), class="abstract") : lang::CREATE_ABSTRACT_NAV;
    }
}

/// Generated wiki page final assembly. Adds the navigation bar, overall html structure.
fn compose_wiki_page<T, C, N>(title: T, content: C, navigation_links: N) -> String
where
    T: RenderOnce,
    C: RenderOnce,
    N: RenderOnce,
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
                nav : navigation_links;
                main : content;
                script(src=StaticAsset::from("client.js").url()) {}
            }
        }
    };
    template.into_string().unwrap()
}

/******************************************************************************
 * Language.
 */
mod lang {
    pub const COMMIT_BUTTON: &'static str = "Valider";
    pub const PREVIEW_BUTTON: &'static str = "Prévisualiser";

    pub const HOMEPAGE: &'static str = "Accueil";
    pub const HOMEPAGE_HELP: &'static str =
        "Pour lister un élément sur cette page, il doit être taggé par _wiki_homepage.";

    pub const ALL_ELEMENTS_NAV: &'static str = "Éléments";
    pub const ALL_ELEMENTS_TITLE: &'static str = "Liste des éléments";

    pub const CREATE_ATOM_NAV: &'static str = "Atome...";
    pub const CREATE_ATOM_TITLE: &'static str = "Ajouter un atome...";
    pub const ATOM_TEXT: &'static str = "Texte";

    pub const CREATE_ABSTRACT_NAV: &'static str = "Abstrait...";
    pub const CREATE_ABSTRACT_TITLE: &'static str = "Ajouter un élément abstrait...";
    pub const CREATE_ABSTRACT_NAME_PLACEHOLDER: &'static str = "Nom optionel";

    pub const NAMED_ATOM: &'static str = "est nommé";
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
                .header(hyper::header::CONTENT_TYPE, asset.mime)
                .header(hyper::header::CACHE_CONTROL, "public, max-age=3600") // Allow cache for 1h
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
 * Wiki web related utils.
 */

/// Web related utilities
mod web {
    use hyper::header;
    use hyper::rt::{Future, Stream};
    use hyper::{Body, Request, Response, StatusCode};
    use percent_encoding::{percent_decode, utf8_percent_encode, QUERY_ENCODE_SET};
    use std::borrow::{Borrow, Cow};
    use std::fmt::{self, Write};
    use std::iter::FromIterator;
    use std::rc::Rc;
    use std::str;
    use tokio::prelude::future;

    #[derive(Debug)]
    pub enum Error {
        BadRequest,
        Internal,
    }
    impl fmt::Display for Error {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            "error".fmt(f)
        }
    }
    impl std::error::Error for Error {}
    impl From<hyper::Error> for Error {
        fn from(_: hyper::Error) -> Self {
            Error::Internal
        }
    }
    impl From<Error> for StatusCode {
        fn from(e: Error) -> StatusCode {
            match e {
                Error::BadRequest => StatusCode::BAD_REQUEST,
                Error::Internal => StatusCode::INTERNAL_SERVER_ERROR,
            }
        }
    }

    /******************************************************************************
     * Route selection.
     */
    pub type BoxedFuture<T> = Box<dyn Future<Item = T, Error = Error>>;

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
    /// Error stops the routing.
    pub enum FromRequestError {
        NoMatch(Request<Body>),
        Error(Error),
    }
    impl From<Error> for FromRequestError {
        fn from(e: Error) -> Self {
            FromRequestError::Error(e)
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
                        Err(e) => Ok(Response::builder()
                            .status(StatusCode::from(e))
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
                FromRequestError::Error(e) => Response::builder()
                    .status(StatusCode::from(e))
                    .body(Body::empty())
                    .unwrap(),
            })),
        }
    }

    /******************************************************************************
     * Short utility functions.
     */

    /// Create an ok response with a body.
    pub fn response_html<B: Into<Body>>(body: B) -> Response<Body> {
        Response::builder()
            .status(StatusCode::OK)
            .header(header::CONTENT_TYPE, "text/html")
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
    /// Create a redirection.
    pub fn response_redirection(uri: &str) -> Response<Body> {
        Response::builder()
            .status(StatusCode::SEE_OTHER)
            .header(header::LOCATION, uri)
            .body(Body::empty())
            .unwrap()
    }

    /// Object can be represented as a query string.
    pub trait QueryFormat: Sized {
        fn to_query(&self, query: &mut PathQueryBuilder);
        fn from_query(entries: &UrlDecodedEntries) -> Result<Self, Error>;
    }

    pub fn to_path_and_query<P: Into<String>, Q: QueryFormat>(path: P, q: &Q) -> String {
        let mut builder = PathQueryBuilder::new(path.into());
        q.to_query(&mut builder);
        builder.build()
    }

    pub fn from_query<Q: QueryFormat>(query: Option<&str>) -> Result<Q, Error> {
        let entries = match query {
            Some(q) => UrlDecodedEntries::decode(q.as_bytes())?,
            None => UrlDecodedEntries::new(),
        };
        Q::from_query(&entries)
    }

    pub fn with_post_entries<E, F>(
        request: Request<Body>,
        f: F,
    ) -> Result<FromRequestOk<E>, FromRequestError>
    where
        E: EndPoint + 'static,
        F: FnOnce(UrlDecodedEntries) -> Result<E, Error> + 'static,
    {
        match request.headers().get(header::CONTENT_TYPE) {
            Some(t) if t == "application/x-www-form-urlencoded" => {
                Ok(FromRequestOk::Future(Box::new(
                    request
                        .into_body()
                        .concat2()
                        .map_err(|_| Error::Internal)
                        .and_then(move |body| {
                            let entries = UrlDecodedEntries::decode(body.as_ref())?;
                            f(entries)
                        }),
                )))
            }
            _ => Err(FromRequestError::Error(Error::BadRequest)),
        }
    }

    /// Remove prefix and return tail of string if successful
    pub fn remove_prefix<'a>(s: &'a str, prefix: &str) -> Option<&'a str> {
        match s.get(..prefix.len()) {
            Some(p) if p == prefix => Some(&s[prefix.len()..]),
            _ => None,
        }
    }

    /******************************************************************************
     * Query writing tools.
     */

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

    /******************************************************************************
     * Url Decoding tools
     */

    /// Associative map based on a sorted vector.
    #[derive(Debug, PartialEq, Eq)]
    pub struct UrlDecodedEntries<'r> {
        inner: Vec<(Cow<'r, str>, Cow<'r, str>)>,
    }
    impl<'r> UrlDecodedEntries<'r> {
        /// With no entries
        pub fn new() -> Self {
            UrlDecodedEntries { inner: Vec::new() }
        }
        /// Decode entries from raw input.
        pub fn decode(input: &'r [u8]) -> Result<Self, Error> {
            input
                .split(|b| *b == b'&')
                .map(|entry| {
                    let mut it = entry.split(|b| *b == b'=');
                    let fields = [it.next(), it.next(), it.next()];
                    match fields {
                        [Some(key), Some(value), None] => {
                            let decode = |s| decode_url(s).map_err(|_| Error::BadRequest);
                            Ok((decode(key)?, decode(value)?))
                        }
                        _ => Err(Error::BadRequest),
                    }
                })
                .collect()
        }
        /// Access entries by name.
        pub fn get<Q>(&self, k: &Q) -> Option<&str>
        where
            Cow<'r, str>: Borrow<Q>,
            Q: Ord + ?Sized,
        {
            self.inner
                .binary_search_by_key(&k, |p| p.0.borrow())
                .map(|index| self.inner[index].1.as_ref())
                .ok()
        }
    }
    impl<'r> FromIterator<(Cow<'r, str>, Cow<'r, str>)> for UrlDecodedEntries<'r> {
        fn from_iter<T>(iter: T) -> Self
        where
            T: IntoIterator<Item = (Cow<'r, str>, Cow<'r, str>)>,
        {
            let mut v = Vec::from_iter(iter);
            v.sort_unstable_by(|lhs, rhs| lhs.0.cmp(&rhs.0));
            UrlDecodedEntries { inner: v }
        }
    }

    fn replace_plus_with_space<'a>(input: &'a [u8]) -> Cow<'a, [u8]> {
        match input.iter().position(|b| *b == b'+') {
            None => Cow::Borrowed(input),
            Some(position) => {
                let mut owned = input.to_owned();
                owned[position] = b' ';
                for b in &mut owned[position + 1..] {
                    if *b == b'+' {
                        *b = b' ';
                    }
                }
                Cow::Owned(owned)
            }
        }
    }
    fn from_utf8<'a>(input: Cow<'a, [u8]>) -> Result<Cow<'a, str>, str::Utf8Error> {
        match input {
            Cow::Borrowed(bytes) => str::from_utf8(bytes).map(|s| Cow::Borrowed(s)),
            Cow::Owned(bytes) => match String::from_utf8(bytes) {
                Ok(s) => Ok(Cow::Owned(s)),
                Err(e) => Err(e.utf8_error()),
            },
        }
    }
    fn decode_url<'a>(input: &'a [u8]) -> Result<Cow<'a, str>, str::Utf8Error> {
        let replaced = replace_plus_with_space(input);
        let decoded = match percent_decode(&replaced).if_any() {
            Some(vec) => Cow::Owned(vec),
            None => replaced,
        };
        from_utf8(decoded)
    }
}
