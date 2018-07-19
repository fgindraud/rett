use super::graph::{Graph, Index, ObjectRef};
use super::horrorshow::helper::doctype;
use super::read_graph_from_file;
use super::rouille;
use rouille::Response;
use std::path::Path;

fn page_for_object<'a>(object: ObjectRef<'a>, _graph: &'a Graph) -> Response {
    let title = format!("Object {}", object.index());
    let details = format!("{:?}", *object);
    Response::html(format!(
        "{}",
        html! {
            : doctype::HTML;
            html {
                head {
                    title : &title
                }
                body {
                    h1 : &title;
                    p : &details;
                }
            }
        }
    ))
}

fn page_for_index(index: Index, graph: &Graph) -> Response {
    match graph.get_object(index) {
        Some(object) => page_for_object(object, graph),
        None => Response::empty_404(),
    }
}

pub fn run(addr: &str, db_file: &Path) -> ! {
    use std::sync::RwLock;
    let graph = read_graph_from_file(db_file);
    let graph = RwLock::new(graph);
    eprintln!("Wiki starting on {}", addr,);

    rouille::start_server(addr, move |request| {
        router!(request,
            (GET) (/id/{id: Index}) => {
                page_for_index(id, &graph.read().unwrap())
            },
            _ => {
                Response::empty_404()
            }
            )
    })
}
