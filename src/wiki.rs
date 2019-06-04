use hyper::rt::Future;
use hyper::service::service_fn_ok;
use hyper::{Body, Request, Response, Server};
use tokio::runtime::current_thread;

use std::cell::RefCell;
use std::path::Path;

use horrorshow::{self, Render, RenderOnce, Template};
use relations;

pub fn run(addr: &str, database_file: &Path) {
    let addr = addr.parse().expect("Address::parse");

    let database = ::read_database_from_file(database_file);

    // TODO use hyper send_file example to re-add static files.
    // Routing must be done on req.uri().(method, path).
    // Use a manual small parser lib ?
    // Introduce a ElementDisplayUrl with a parse method ?

    let new_service = || service_fn_ok(|_req| Response::new(Body::from("Blah")));

    let server = Server::bind(&addr)
        .executor(current_thread::TaskExecutor::current())
        .serve(new_service)
        .map_err(|e| panic!("Server error: {}", e));

    current_thread::block_on_all(server).expect("Failed")
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

struct State {
    database: RefCell<relations::Database>,
}

trait Page
where
    Self: Sized,
{
    fn to_url(&self) -> String;
    //FIXME fn from_request(request: &Request<()>) -> Option<Self>;
    //FIXME fn generate_page(&self, state: &State) -> Response<()>;
}

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
        if let Some(i) = self.link_from {
            b.entry("link_from", i)
        }
        if let Some(i) = self.link_to {
            b.entry("link_to", i)
        }
        if let Some(i) = self.link_tag {
            b.entry("link_tag", i)
        }
        b.build()
    }
}

/// Uri related utilities
mod uri {
    use percent_encoding::{utf8_percent_encode, QUERY_ENCODE_SET};
    use relations;
    use std::fmt::{self, Write};

    /// T value in a Query Uri encoding. Has fmt::Display impls for supported types.
    struct QueryFormat<T>(T);
    impl fmt::Display for QueryFormat<relations::Index> {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            self.0.fmt(f) // Formatting an int does not require encoding
        }
    }
    impl<'a> fmt::Display for QueryFormat<&'a str> {
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
            QueryFormat<K>: fmt::Display,
            QueryFormat<V>: fmt::Display,
        {
            write!(
                &mut self.path_and_query,
                "{}{}={}",
                self.query_prefix_char,
                QueryFormat(key),
                QueryFormat(value)
            )
            .unwrap();
            self.query_prefix_char = '&' // Entries are ?first=v&second=v&...
        }
    }
}

mod router {
    // TODO think more about design there
    use hyper::Method;
    use hyper::Uri;

    pub trait FromUri<R> {
        fn from_uri(uri: &Uri) -> Option<R>
        where
            Self: Sized;
    }

    pub struct Router<R> {
        routes: Vec<Box<FromUri<R>>>,
    }

    impl<R> Router<R> {
        pub fn new() -> Self {
            Router { routes: Vec::new() }
        }
    }

    // URLs are percent_encoded.
    // Use simple split on hyper::Uri::path, then use
}
