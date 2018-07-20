use super::graph::{Graph, Index, Object, ObjectRef};
use super::horrorshow::{helper::doctype, Render};
use super::read_graph_from_file;
use super::rouille;
use rouille::Response;
use std::path::Path;

trait ToUrl {
    fn to_url(&self) -> String;
}
impl ToUrl for Index {
    fn to_url(&self) -> String {
        format!("/id/{}", *self)
    }
}

fn title_for_object<'a>(object: ObjectRef<'a>, _graph: &'a Graph) -> String {
    match *object {
        Object::Atom(ref a) => format!("{}", a),
        Object::Link(ref l) => format!("{} → {}", l.from, l.to),
        Object::Abstract => format!("Object {}", object.index()),
    }
}

fn page_all_objects(graph: &Graph) -> Response {
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
                            a(href=object.index().to_url()) : title_for_object(object, graph);
                        }
                    }
                }
                h1 : "Abstract";
                ul {
                    @ for object in graph.objects().filter(|o| o.is_abstract()) {
                        li {
                            a(href=object.index().to_url()) : title_for_object(object, graph);
                        }
                    }
                }
                h1 : "Links";
                ul {
                    @ for object in graph.objects().filter(|o| o.is_link()) {
                        li {
                            a(href=object.index().to_url()) : title_for_object(object, graph);
                        }
                    }
                }
            }
        }
    ))
}

fn page_for_object<'a>(object: ObjectRef<'a>, graph: &'a Graph) -> Response {
    let title = title_for_object(object, graph);
    let details: Box<Render> = match *object {
        Object::Atom(ref a) => box_html! { : format!("{}", a); },
        Object::Link(ref l) => box_html! {
            ul {
                li {
                    : "From ";
                    a(href=l.from.to_url()) : title_for_object(graph.object(l.from), graph);
                }
                li {
                    : "To ";
                    a(href=l.to.to_url()) : title_for_object(graph.object(l.to), graph);
                }
            }
        },
        Object::Abstract => box_html! { : "Abstract"; },
    };
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
                    h2 : "Linked from";
                    ul {
                        @ for object in object.in_links().iter().map(|i| graph.object(*i)) {
                            li {
                                a(href=object.index().to_url()) : title_for_object(object, graph);
                            }
                        }
                    }
                    h2 : "Linked to";
                    ul {
                        @ for object in object.out_links().iter().map(|i| graph.object(*i)) {
                            li {
                                a(href=object.index().to_url()) : title_for_object(object, graph);
                            }
                        }
                    }
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
            (GET) ["/all"] => {
                page_all_objects(&graph.read().unwrap())
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
