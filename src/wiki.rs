use super::graph::{Graph, Index, Object, ObjectRef};
use super::horrorshow;
use super::read_graph_from_file;
use super::rouille;
use horrorshow::{Render, RenderOnce, Template};
use rouille::Response;
use std::path::Path;

pub fn run(addr: &str, db_file: &Path) -> ! {
    use std::sync::RwLock;
    let graph = read_graph_from_file(db_file);
    let graph = RwLock::new(graph);
    eprintln!("Wiki starting on {}", addr,);

    rouille::start_server(addr, move |request| {
        router!(request,
            (GET) ["/"] => {
                main_page(&graph.read().unwrap())
            },
            (GET) ["/all"] => {
                page_all_objects(&graph.read().unwrap())
            },
            (GET) ["/id/{id}", id: Index] => {
                page_for_index(id, &graph.read().unwrap())
            },
            (GET) ["/{asset}", asset: String] => {
                send_asset(&asset)
            },
            _ => {
                Response::empty_404()
            }
        )
    })
}

trait ToUrl {
    fn to_url(&self) -> String;
}
impl ToUrl for Index {
    fn to_url(&self) -> String {
        format!("/id/{}", *self)
    }
}

fn wiki_page<T, C>(title: T, content: C) -> Response
where
    T: RenderOnce,
    C: RenderOnce,
{
    let template = html! {
        : horrorshow::helper::doctype::HTML;
        html {
            head {
                link(rel="stylesheet", type="text/css", href="/style.css");
                title : title;
            }
            body {
                nav {
                    a(href="/") : "Main page"; br;
                    a(href="/all") : "All objects"; br;
                    a(href="/doc.html") : "Help"; br;
                }
                : content;
            }
        }
    };
    Response::html(template.into_string().unwrap())
}

fn title_for_object<'a>(object: ObjectRef<'a>, _graph: &'a Graph) -> String {
    match *object {
        Object::Atom(ref a) => a.to_string(),
        Object::Link(ref l) => format!("{} → {}", l.from, l.to),
        Object::Abstract => format!("Object {}", object.index()),
    }
}

fn main_page(graph: &Graph) -> Response {
    // TODO redirect to page pointed by _wiki_main
    wiki_page("Main page", html! { p : "TODO"; })
}

fn page_all_objects(graph: &Graph) -> Response {
    wiki_page(
        "Object list",
        html! {
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
        },
    )
}

fn page_for_object<'a>(object: ObjectRef<'a>, graph: &'a Graph) -> Response {
    // TODO use aside tag for edit box ?
    let title = title_for_object(object, graph);
    let details: Box<Render> = match *object {
        Object::Atom(ref a) => box_html! { : a.to_string(); },
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
    wiki_page(
        &title,
        html! {
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
        },
    )
}

fn page_for_index(index: Index, graph: &Graph) -> Response {
    match graph.get_object(index) {
        Some(object) => page_for_object(object, graph),
        None => Response::empty_404(),
    }
}

/* Wiki external files.
 * Static files are much easier to edit as standalone.
 * Use rust_embed to embed them in the binary (on release mode only).
 */
#[derive(RustEmbed)]
#[folder = "wiki/"]
struct Asset;

fn send_asset(path: &str) -> Response {
    if let Some(asset) = Asset::get(path) {
        let content_type = match path {
            path if path.ends_with(".css") => "text/css",
            path if path.ends_with(".html") => "text/html",
            _ => "application/octet-stream",
        };
        Response::from_data(content_type, asset).with_public_cache(3600)
    } else {
        Response::empty_404()
    }
}
