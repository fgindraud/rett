use hyper::rt::Future;
use hyper::service::service_fn_ok;
use hyper::{Body, Method, Request, Response, Server, StatusCode};
use tokio::runtime::current_thread;

use std::cell::RefCell;
use std::io;
use std::path::Path;
use std::rc::Rc;

use horrorshow::{self, Render, RenderOnce, Template};

use relations;
use utils;

type DatabaseLock = RefCell<relations::Database>;

/// Interface for a wiki page.
trait Page
where
    Self: Sized,
{
    fn to_url(&self) -> String;
    fn from_request(request: Request<Body>) -> Result<Self, PageFromRequestError>;
    fn generate_response(self, db: &DatabaseLock) -> Response<Body>;
}

/// Error code used for routing.
enum PageFromRequestError {
    NoMatch(Request<Body>),
    BadRequest(Box<std::error::Error>),
}
/// Convenience conversion which allows to use '?' in from_request() implementations.
impl<E: Into<Box<std::error::Error>>> From<E> for PageFromRequestError {
    fn from(e: E) -> Self {
        PageFromRequestError::BadRequest(e.into())
    }
}

/// Entry point, run the wiki server.
pub fn run(addr: &str, database_file: &Path) {
    let addr = addr.parse().expect("Address::parse");

    let database = ::read_database_from_file(database_file);
    let database = Rc::new(RefCell::new(database));

    let create_service = || {
        let database = database.clone();
        service_fn_ok(move |request| {
            // Move cloned rc ref in this scope.
            eprintln!("Request: {:?}", request);
            let response = process_request(request, &database, PAGES.iter());
            eprintln!("Response: {:?}", response);
            response
        })
    };
    let server = Server::bind(&addr)
        .executor(current_thread::TaskExecutor::current())
        .serve(create_service)
        .map_err(|e| panic!("Server error: {}", e));

    current_thread::block_on_all(server).expect("Failed");
}

/// All page types must be registered here.
const PAGES: [PageHandlerFn; 1] = [page_handler::<DisplayElement>];
type PageHandlerFn =
    fn(Request<Body>, &DatabaseLock) -> Result<Response<Body>, PageFromRequestError>;

fn page_handler<P: Page>(
    request: Request<Body>,
    db: &DatabaseLock,
) -> Result<Response<Body>, PageFromRequestError> {
    P::from_request(request).map(|p| p.generate_response(db))
}

/// Apply the first matching handler, or generate an error reponse (400 or 404).
fn process_request<I>(request: Request<Body>, db: &DatabaseLock, handlers: I) -> Response<Body>
where
    I: Iterator,
    <I as Iterator>::Item:
        Fn(Request<Body>, &DatabaseLock) -> Result<Response<Body>, PageFromRequestError>,
{
    // Apply handlers until match or BadRequest.
    let final_state = handlers.fold(
        Err(PageFromRequestError::NoMatch(request)),
        |state, handler| match state {
            Err(PageFromRequestError::NoMatch(request)) => handler(request, db),
            state => state,
        },
    );
    // Select final response
    match final_state {
        Ok(response) => response,
        Err(PageFromRequestError::NoMatch(_)) => Response::builder()
            .status(StatusCode::NOT_FOUND)
            .body(Body::empty())
            .unwrap(),
        Err(PageFromRequestError::BadRequest(e)) => Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(e.to_string()))
            .unwrap(),
    }
}

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

/// Display an element.
struct DisplayElement {
    index: relations::Index,
    // Temporary selection for link creation
    link_from: Option<relations::Index>,
    link_to: Option<relations::Index>,
    link_tag: Option<relations::Index>,
}
impl Page for DisplayElement {
    fn to_url(&self) -> String {
        let mut b = uri::PathQueryBuilder::new(format!("/element/{}", self.index));
        b.optional_entry("link_from", self.link_from);
        b.optional_entry("link_to", self.link_to);
        b.optional_entry("link_tag", self.link_tag);
        b.build()
    }
    fn from_request(request: Request<Body>) -> Result<Self, PageFromRequestError> {
        if let (&Method::GET, Some(s)) = (
            request.method(),
            utils::remove_prefix(request.uri().path(), "/element/"),
        ) {
            let query = uri::decode_optional_query_entries(request.uri().query())?;
            return Ok(DisplayElement {
                index: s.parse()?,
                link_from: query.get("link_from").map(|s| s.parse()).transpose()?,
                link_to: query.get("link_to").map(|s| s.parse()).transpose()?,
                link_tag: query.get("link_tag").map(|s| s.parse()).transpose()?,
            });
        }
        Err(PageFromRequestError::NoMatch(request))
    }
    fn generate_response(self, _db: &DatabaseLock) -> Response<Body> {
        //TODO continue here
        Response::builder()
            .status(StatusCode::NOT_FOUND)
            .body(Body::empty())
            .unwrap()
    }
}

/// Uri related utilities
mod uri {
    use percent_encoding::{percent_decode, utf8_percent_encode, PercentEncode, QUERY_ENCODE_SET};
    use relations;
    use std::borrow::Cow;
    use std::fmt::{self, Write};
    use utils::Map;

    /// Convertible to something printable in a query string
    pub trait ToQueryDisplayable {
        // This trait is an optimization over encode(to_string(T)) for some T.
        type Output: fmt::Display;
        fn to_query_displayable(&self) -> Self::Output;
    }
    impl ToQueryDisplayable for relations::Index {
        type Output = relations::Index;
        fn to_query_displayable(&self) -> Self::Output {
            *self // Integers do not need URL-encoding
        }
    }
    impl<'a> ToQueryDisplayable for &'a str {
        type Output = PercentEncode<'a, QUERY_ENCODE_SET>;
        fn to_query_displayable(&self) -> Self::Output {
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
            K: ToQueryDisplayable,
            V: ToQueryDisplayable,
        {
            write!(
                &mut self.path_and_query,
                "{}{}={}",
                self.query_prefix_char,
                key.to_query_displayable(),
                value.to_query_displayable()
            )
            .unwrap();
            self.query_prefix_char = '&' // Entries are ?first=v&second=v&...
        }
        pub fn optional_entry<K, V>(&mut self, key: K, value: Option<V>)
        where
            K: ToQueryDisplayable,
            V: ToQueryDisplayable,
        {
            if let Some(v) = value {
                self.entry(key, v)
            }
        }
    }

    #[derive(Debug)]
    pub enum QueryDecodingError {
        Utf8,
        Structure,
    }
    impl fmt::Display for QueryDecodingError {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self {
                QueryDecodingError::Utf8 => "query is not valid utf8".fmt(f),
                QueryDecodingError::Structure => "bad query structure".fmt(f),
            }
        }
    }
    impl std::error::Error for QueryDecodingError {}

    pub fn decode_query_entries<'a>(
        raw_query: &'a str,
    ) -> Result<Map<Cow<'a, str>, Cow<'a, str>>, QueryDecodingError> {
        raw_query
            .split('&')
            .map(|entry| {
                let mut it = entry.split('=');
                let fields = [it.next(), it.next(), it.next()];
                match fields {
                    [Some(key), Some(value), None] => {
                        let decode = |s| percent_decode(s).decode_utf8();
                        match (decode(key.as_bytes()), decode(value.as_bytes())) {
                            (Ok(key), Ok(value)) => Ok((key, value)),
                            _ => Err(QueryDecodingError::Utf8),
                        }
                    }
                    _ => Err(QueryDecodingError::Structure),
                }
            })
            .collect()
    }
    pub fn decode_optional_query_entries<'a>(
        raw_query: Option<&'a str>,
    ) -> Result<Map<Cow<'a, str>, Cow<'a, str>>, QueryDecodingError> {
        match raw_query {
            Some(s) => decode_query_entries(s),
            None => Ok(Map::new()),
        }
    }
}
