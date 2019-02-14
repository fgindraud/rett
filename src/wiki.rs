use hyper::rt::Future;
use hyper::service::service_fn_ok;
use hyper::{Body, Response, Server};
use std::path::Path;
use tokio::runtime::current_thread;

pub fn run(addr: &str, database_file: &Path) {
    let addr = addr.parse().expect("Address::parse");

    let executor = current_thread::TaskExecutor::current();
    let mut runtime = current_thread::Runtime::new().expect("Runtime::new");

    let build_new_service = || {
        service_fn_ok(|_req| {
            //
            Response::new(Body::from("Blah"))
        })
    };

    let server = Server::bind(&addr)
        .executor(executor)
        .serve(build_new_service)
        .map_err(|e| panic!("Server error: {}", e));

    runtime.spawn(server);
    runtime.run().expect("Runtime::run")
}
