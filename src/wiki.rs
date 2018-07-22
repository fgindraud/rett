use super::graph::{Atom, Graph, Index, Object, ObjectRef};
use super::horrorshow;
use super::rouille;
use super::{read_graph_from_file, write_graph_to_file};
use horrorshow::{Render, RenderOnce, Template};
use rouille::{Request, Response};
use std::ops::{Deref, DerefMut};
use std::path::{Path, PathBuf};
use std::sync::{RwLock, RwLockReadGuard, RwLockWriteGuard};

/******************************************************************************
 * Database concurrent access and writing logic.
 */
struct Database {
    file: PathBuf,
    graph: RwLock<Graph>,
}
impl Database {
    /// Initialize graph from file content.
    fn from_file(file: &Path) -> Self {
        Database {
            file: file.to_owned(),
            graph: RwLock::new(read_graph_from_file(file)),
        }
    }
    /// Get read only access to the database.
    fn access(&self) -> RwLockReadGuard<Graph> {
        self.graph.read().unwrap()
    }
    /// Get write access to the database ; writes database to disk when lock is released.
    fn modify<'a>(&'a self) -> DatabaseWriteLock<'a> {
        DatabaseWriteLock {
            file: &self.file,
            lock: self.graph.write().unwrap(),
        }
    }
}
struct DatabaseWriteLock<'a> {
    file: &'a Path,
    lock: RwLockWriteGuard<'a, Graph>,
}
impl<'a> Deref for DatabaseWriteLock<'a> {
    type Target = Graph;
    fn deref(&self) -> &Graph {
        &self.lock
    }
}
impl<'a> DerefMut for DatabaseWriteLock<'a> {
    fn deref_mut(&mut self) -> &mut Graph {
        &mut self.lock
    }
}
impl<'a> Drop for DatabaseWriteLock<'a> {
    fn drop(&mut self) {
        write_graph_to_file(self.file, &self.lock)
    }
}

/******************************************************************************
 * HTTP server.
 */
pub fn run(addr: &str, file: &Path) -> ! {
    let db = Database::from_file(file);
    eprintln!("Wiki starting on {}", addr);

    rouille::start_server(addr, move |request| {
        router!(request,
            (GET) ["/"] => {
                main_page(&db.access())
            },
            (GET) ["/all"] => {
                page_all_objects(&db.access())
            },
            (GET) ["/id/{id}", id: Index] => {
                match db.access ().get_object(id) {
                    Some(object) => page_for_object(object),
                    None => Response::empty_404(),
                }
            },
            (GET) ["/create/atom"] => {
                page_create_atom()
            },
            (POST) ["/create/atom"] => {
                post_create_atom(request, &mut db.modify())
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

/******************************************************************************
 * Wiki page generation.
 */
fn wiki_page<T, C>(title: T, content: C) -> Response
where
    T: RenderOnce,
    C: RenderOnce,
{
    let navigation = [
        ("Home", "/"),
        ("All", "/all"),
        ("New atom", "/create/atom"),
        ("New abstract", "/create/abstract"),
        ("Help", "/doc.html"),
    ];
    let template = html! {
        : horrorshow::helper::doctype::HTML;
        html {
            head {
                link(rel="stylesheet", type="text/css", href="/style.css");
                title : title;
            }
            body {
                nav {
                    ul {
                        @ for (name, url) in navigation.iter() {
                            li {
                                a(href=url) : name;
                            }
                        }
                    }
                }
                : content;
            }
        }
    };
    Response::html(template.into_string().unwrap())
}

fn title_for_object<'a>(object: ObjectRef<'a>) -> String {
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
                        a(href=object.index().to_url()) : title_for_object(object);
                    }
                }
            }
            h1 : "Abstract";
            ul {
                @ for object in graph.objects().filter(|o| o.is_abstract()) {
                    li {
                        a(href=object.index().to_url()) : title_for_object(object);
                    }
                }
            }
            h1 : "Links";
            ul {
                @ for object in graph.objects().filter(|o| o.is_link()) {
                    li {
                        a(href=object.index().to_url()) : title_for_object(object);
                    }
                }
            }
        },
    )
}

fn page_for_object<'a>(object: ObjectRef<'a>) -> Response {
    // TODO use aside tag for edit box ?
    let graph = object.graph();
    let title = title_for_object(object);
    let details: Box<Render> = match *object {
        Object::Atom(ref a) => box_html! { : a.to_string(); },
        Object::Link(ref l) => box_html! {
            ul {
                li {
                    : "From ";
                    a(href=l.from.to_url()) : title_for_object(graph.object(l.from));
                }
                li {
                    : "To ";
                    a(href=l.to.to_url()) : title_for_object(graph.object(l.to));
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
                        a(href=object.index().to_url()) : title_for_object(object);
                    }
                }
            }
            h2 : "Linked to";
            ul {
                @ for object in object.out_links().iter().map(|i| graph.object(*i)) {
                    li {
                        a(href=object.index().to_url()) : title_for_object(object);
                    }
                }
            }
        },
    )
}

fn page_create_atom() -> Response {
    wiki_page(
        "Create atom",
        html!{
            form(method="post") {
                : "Value:";
                input(type="text", name="text");
                input(type="submit", value="Create");
            }
        },
    )
}
fn post_create_atom(request: &Request, graph: &mut Graph) -> Response {
    let form_data = try_or_400!(post_input!(request, { text: String }));
    let text = form_data.text.trim();
    let index = graph.use_atom(Atom::text(text));
    Response::redirect_303(index.to_url())
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
