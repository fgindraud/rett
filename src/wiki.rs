/******************************************************************************
 * Database concurrent access and writing logic.
 * In its own module to scope the use;
 */
mod database {
    use graph::Graph;
    use read_graph_from_file;
    use std::ops::{Deref, DerefMut};
    use std::path::{Path, PathBuf};
    use std::sync::{RwLock, RwLockReadGuard, RwLockWriteGuard};
    use write_graph_to_file;

    pub struct Database {
        file: PathBuf,
        graph: RwLock<Graph>,
    }
    impl Database {
        /// Initialize graph from file content.
        pub fn from_file(file: &Path) -> Self {
            Database {
                file: file.to_owned(),
                graph: RwLock::new(read_graph_from_file(file)),
            }
        }
        /// Get read only access to the database.
        pub fn access(&self) -> RwLockReadGuard<Graph> {
            self.graph.read().unwrap()
        }
        /// Get write access to the database ; writes database to disk when lock is released.
        pub fn modify<'a>(&'a self) -> DatabaseWriteLock<'a> {
            DatabaseWriteLock {
                file: &self.file,
                lock: self.graph.write().unwrap(),
            }
        }
    }
    pub struct DatabaseWriteLock<'a> {
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
}

use graph::{Atom, Graph, Index, Link, Object, ObjectRef};
use horrorshow::{self, Render, RenderOnce, Template};
use rouille::{self, Request, Response};
use std::path::Path;

enum LinkSide {
    From,
    To,
}

/******************************************************************************
 * HTTP server.
 */
pub fn run(addr: &str, file: &Path, nb_threads: usize) -> ! {
    let db = database::Database::from_file(file);
    eprintln!("Wiki starting on {}", addr);
    rouille::start_server_with_pool(addr, Some(nb_threads), move |request| {
        router!(request,
            (GET) ["/"] => { main_page(&db.access()) },
            (GET) ["/all"] => { page_all_objects(&db.access()) },
            (GET) ["/id/{id}", id: Index] => {
                match db.access ().get_object(id) {
                    Some(object) => page_for_object(object),
                    None => Response::empty_404(),
                }
            },
            (GET) ["/create/atom"] => { page_create_atom() },
            (POST) ["/create/atom"] => { post_create_atom(request, &mut db.modify()) },
            (GET) ["/create/abstract"] => { page_create_abstract() },
            (POST) ["/create/abstract"] => { post_create_abstract(request, &mut db.modify()) },
            (GET) ["/create/link/from/{id}", id: Index] => {
                match db.access ().get_object(id) {
                    Some(object) => page_create_link(object, LinkSide::From),
                    None => Response::empty_404(),
                }
            },
            (GET) ["/create/link/to/{id}", id: Index] => {
                match db.access ().get_object(id) {
                    Some(object) => page_create_link(object, LinkSide::To),
                    None => Response::empty_404(),
                }
            },
            (POST) ["/create/link"] => { post_create_link(request, &mut db.modify()) },
            (GET) ["/static/{asset}", asset: String] => { send_asset(&asset) },
            _ => { Response::empty_404() }
        )
    })
}

/******************************************************************************
 * Wiki page generation.
 * TODO page_object : remove
 * TODO page_orphan : not linked from _wiki_main
 * TODO provide suggestions for atoms (fuzzy seach in atom list)
 * TODO improve node selection system
 */
trait ToUrl {
    fn to_url(&self) -> String;
}
impl ToUrl for Index {
    fn to_url(&self) -> String {
        format!("/id/{}", *self)
    }
}

fn title_for_object<'a>(object: ObjectRef<'a>) -> String {
    match *object {
        Object::Atom(ref a) => a.to_string(),
        Object::Link(ref l) => format!("{} → {}", l.from, l.to),
        Object::Abstract => format!("Object {}", object.index()),
    }
}

// TODO add field for additional nav elements(link to/from, remove)
fn wiki_page<T, C>(title: T, content: C) -> Response
where
    T: RenderOnce,
    C: RenderOnce,
{
    let navigation = [
        ("Home", "/"),
        ("All", "/all"),
        ("Atom", "/create/atom"),
        ("Abstract", "/create/abstract"),
        ("Help", "/static/doc.html"), // TODO generated to have menu, or special page
    ];
    let template = html! {
        : horrorshow::helper::doctype::HTML;
        html {
            head {
                link(rel="stylesheet", type="text/css", href="/static/style.css");
                meta(name="viewport", content="width=device-width, initial-scale=1.0");
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
                main {
                    : content;
                }
            }
        }
    };
    Response::html(template.into_string().unwrap())
}

fn main_page(graph: &Graph) -> Response {
    if let Some(wiki_main_index) = graph.get_atom(&Atom::text("_wiki_main")) {
        let wiki_main = graph.object(wiki_main_index);
        if let Some(&out_link_id) = wiki_main.out_links().first() {
            let out_link = graph.object(out_link_id);
            if let Object::Link(ref l) = *graph.object(out_link_id) {
                return Response::redirect_303(l.to.to_url());
            }
        }
    }
    wiki_page(
        "Main page",
        html! {
            p : "Create a link from \"_wiki_main\" to a graph object to define it as the home page.";
        },
    )
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
            p {
                a(href=format!("/create/link/to/{}", object.index())) : "Add";
            }
            ul {
                @ for object in object.in_links().iter().map(|i| graph.object(*i)) {
                    li {
                        a(href=object.index().to_url()) : title_for_object(object);
                    }
                }
            }
            h2 : "Links to";
            p {
                a(href=format!("/create/link/from/{}", object.index())) : "Add";
            }
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

fn page_create_abstract() -> Response {
    wiki_page(
        "Create abstract",
        html!{
            form(method="post") {
                : "Optional name:";
                input(type="text", name="name");
                input(type="submit", value="Create");
            }
        },
    )
}
fn post_create_abstract(request: &Request, graph: &mut Graph) -> Response {
    let form_data = try_or_400!(post_input!(request, { name: String }));
    let name = form_data.name.trim();
    let abstract_index = graph.create_abstract();
    if name != "" {
        let atom = graph.use_atom(Atom::text(name));
        graph.use_link(Link::new(atom, abstract_index));
    }
    Response::redirect_303(abstract_index.to_url())
}

fn page_create_link<'a>(object: ObjectRef<'a>, link_side: LinkSide) -> Response {
    let defined_link_side_text = match link_side {
        LinkSide::From => "from",
        LinkSide::To => "to",
    };
    let undefined_link_side_text = match link_side {
        LinkSide::From => "to",
        LinkSide::To => "from",
    };
    let graph = object.graph();
    wiki_page(
        format!(
            "Create link {} {}",
            defined_link_side_text,
            title_for_object(object)
        ),
        html!{
            form(method="post", action="/create/link") {
                input(type="hidden", name=defined_link_side_text, value=object.index());
                @ for object in graph.objects() {
                    input(type="radio", name=undefined_link_side_text, value=object.index()) {
                        : title_for_object(object);
                    }
                    br;
                }
                input(type="submit", value="Create");
            }
        },
    )
}
fn post_create_link(request: &Request, graph: &mut Graph) -> Response {
    let l = try_or_400!(post_input!(request, { from: Index, to: Index }));
    if let (Some(_), Some(_)) = (graph.get_object(l.from), graph.get_object(l.to)) {
        // Indexes are valid TODO validate in graph
        let index = graph.use_link(Link::new(l.from, l.to));
        Response::redirect_303(index.to_url())
    } else {
        Response::empty_400()
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
            path if path.ends_with(".js") => "application/javascript",
            path if path.ends_with(".ico") => "image/vnd.microsoft.icon",
            _ => "application/octet-stream",
        };
        Response::from_data(content_type, asset) //FIXME.with_public_cache(3600)
    } else {
        Response::empty_404()
    }
}
