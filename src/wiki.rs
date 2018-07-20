use super::graph::{Graph, Index, Object, ObjectRef};
use super::horrorshow::helper::doctype;
use super::read_graph_from_file;
use super::rouille;
use rouille::Response;
use std::path::Path;

fn title_for_object<'a>(object: ObjectRef<'a>, _graph: &'a Graph) -> String {
    match *object {
        Object::Atom(ref a) => format!("{}", a),
        Object::Link(ref l) => format!("{} → {}", l.from, l.to),
        Object::Abstract => format!("Object {}", object.index()),
    }
}

fn page_listing(graph: &Graph) -> Response {
    Response::html(format!(
        "{}",
        html! {
            : doctype::HTML;
            head {
                title : "Object list";
            }
            body {
                h1 : "Atoms";
                ul {
                    @ for object in graph.objects().filter(|o| o.is_atom()) {
                        li {
                            a(href=format!("/id/{}", object.index())) : title_for_object(object, graph);
                        }
                    }
                }
                h1 : "Abstract";
                ul {
                    @ for object in graph.objects().filter(|o| o.is_abstract()) {
                        li {
                            a(href=format!("/id/{}", object.index())) : title_for_object(object, graph);
                        }
                    }
                }
                h1 : "Links";
                ul {
                    @ for object in graph.objects().filter(|o| o.is_link()) {
                        li {
                            a(href=format!("/id/{}", object.index())) : title_for_object(object, graph);
                        }
                    }
                }
            }
        }
    ))
}

fn page_for_object<'a>(object: ObjectRef<'a>, graph: &'a Graph) -> Response {
    let title = title_for_object(object, graph);
    let details = format!("{:?}", *object);
    Response::html(format!(
        "{}",
        html! {
            : doctype::HTML;
            html {
                head {
                    title : &title;
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
            (GET) ["/"] => {
                page_listing(&graph.read().unwrap())
            },
            (GET) ["/id/{id}", id: Index] => {
                page_for_index(id, &graph.read().unwrap())
            },
            _ => {
                Response::empty_404()
            }
        )
    })
}
