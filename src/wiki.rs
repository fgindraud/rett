use hyper::rt::Future;
use hyper::service::service_fn_ok;
use hyper::{Body, Request, Response, Server};
use std::path::Path;
use tokio::runtime::current_thread;

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
