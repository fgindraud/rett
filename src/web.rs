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
    //TODO reintroduce if useful: fn url(&self) -> String;
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
            FromRequestOk::Value(v) => Box::new(future::ok(v.generate_response(state.as_ref()))),
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
        Some(t) if t == "application/x-www-form-urlencoded" => Ok(FromRequestOk::Future(Box::new(
            request
                .into_body()
                .concat2()
                .map_err(|_| Error::Internal)
                .and_then(move |body| {
                    let entries = UrlDecodedEntries::decode(body.as_ref())?;
                    f(entries)
                }),
        ))),
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