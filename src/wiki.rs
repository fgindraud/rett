/******************************************************************************
 * Database concurrent access and writing logic.
 * In its own module to scope the use;
 */
mod state {
    use read_database_from_file;
    use relations::Database;
    use std::ops::{Deref, DerefMut};
    use std::path::{Path, PathBuf};
    use std::sync::{RwLock, RwLockReadGuard, RwLockWriteGuard};
    use write_database_to_file;

    pub struct State {
        file: PathBuf,
        database: RwLock<Database>,
    }
    impl State {
        /// Initialize graph from file content.
        pub fn from_file(file: &Path) -> Self {
            State {
                file: file.to_owned(),
                database: RwLock::new(read_database_from_file(file)),
            }
        }
        /// Get read only access to the database.
        pub fn access(&self) -> RwLockReadGuard<Database> {
            self.database.read().unwrap()
        }
        /// Get write access to the database ; writes database to disk when lock is released.
        pub fn modify<'a>(&'a self) -> DatabaseWriteLock<'a> {
            DatabaseWriteLock {
                file: &self.file,
                lock: self.database.write().unwrap(),
            }
        }
    }
    pub struct DatabaseWriteLock<'a> {
        file: &'a Path,
        lock: RwLockWriteGuard<'a, Database>,
    }
    impl<'a> Deref for DatabaseWriteLock<'a> {
        type Target = Database;
        fn deref(&self) -> &Database {
            &self.lock
        }
    }
    impl<'a> DerefMut for DatabaseWriteLock<'a> {
        fn deref_mut(&mut self) -> &mut Database {
            &mut self.lock
        }
    }
    impl<'a> Drop for DatabaseWriteLock<'a> {
        fn drop(&mut self) {
            write_database_to_file(self.file, &self.lock)
        }
    }
}

use horrorshow::{self, Render, RenderOnce, Template};
use relations::{Abstract, Atom, Element, Index, Ref, Relation};
use rouille::{self, Request, Response};
use std::fmt;
use std::path::Path;

/******************************************************************************
 * HTTP server.
 */
pub fn run(addr: &str, file: &Path) -> ! {
    let nb_threads = 2;
    let db = state::State::from_file(file);
    eprintln!("Wiki starting on {}", addr);
    rouille::start_server_with_pool(addr, Some(nb_threads), move |request| {
        router!(request,
                    // Main page and special pages
        //            (GET) ["/"] => { main_page(&db.access()) },
        //            (GET) ["/all"] => { page_all_objects(&db.access()) },
                    // Elements by id
                    (GET) ["/element/{id}", id: usize] => {
                        match db.access().element(id as Index) {
                            Ok(r) => display_page(r),
                            _ => Response::empty_404()
                        }
                    },
                    // Create elements (raw)
        //            (GET) ["/create/atom"] => { page_create_atom() },
        //            (POST) ["/create/atom"] => { post_create_atom(request, &mut db.modify()) },
        //            (GET) ["/create/abstract"] => { page_create_abstract() },
        //            (POST) ["/create/abstract"] => { post_create_abstract(request, &mut db.modify()) },
        //            (GET) ["/create/link/from/{id}", id: Index] => {
        //                match db.access ().get_object(id) {
        //                    Ok(object) => page_create_link(object, LinkSide::From),
        //                    _ => Response::empty_400(),
        //                }
        //            },
        //            (GET) ["/create/link/to/{id}", id: Index] => {
        //                match db.access ().get_object(id) {
        //                    Ok(object) => page_create_link(object, LinkSide::To),
        //                    _ => Response::empty_400(),
        //                }
        //            },
        //            (POST) ["/create/link"] => { post_create_link(request, &mut db.modify()) },
        //            // Contextual actions from overlays
        //            (POST) ["/edit/description"] => { post_edit_description(request, &mut db.modify()) },
        //            (POST) ["/remove"] => { post_remove(request, &mut db.modify()) },
                    // Assets
                    (GET) ["/static/{asset}", asset: String] => { send_asset(&asset) },
                    _ => { Response::empty_404() }
                )
    })
}

/******************************************************************************
 * Wiki page generation.
 *
 * TODO page_orphan : not linked from _wiki_main
 * TODO provide suggestions for atoms (fuzzy seach in atom list)
 * TODO improve node selection system
 */

// Display page is the page showing information, read only.
struct DisplayPageUrl(Index);
impl fmt::Display for DisplayPageUrl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "/element/{}", self.0)
    }
}

fn display_page<'a>(r: Ref<'a, Element>) -> Response {
    wiki_page("Title", html!{}, html!{})
}

//// Elements associated to a type of objects are tagged with HTML classes. Get class name.
//fn object_html_class<'a>(object: ObjectRef<'a>) -> &'static str {
//    match *object {
//        Object::Atom(_) => "atom",
//        Object::Link(_) => "link",
//        Object::Abstract => "abstract",
//    }
//}
//
//fn object_name<'a>(object: ObjectRef<'a>) -> String {
//    match *object {
//        Object::Atom(ref a) => a.to_string(),
//        Object::Link(ref l) => format!("{} → {}", l.from, l.to),
//        Object::Abstract => {
//            let has_name_parent = |l: ObjectRef<'a>| match *l.as_link().unwrap().from {
//                Object::Atom(ref a) => match *a {
//                    Atom::Text(ref t) if t == "name" => true,
//                    _ => false,
//                },
//                _ => false,
//            };
//            let is_naming_link = |l: &ObjectRef<'a>| l.in_links().into_iter().any(has_name_parent);
//            if let Some(naming_link) = object.in_links().into_iter().find(is_naming_link) {
//                object_name(naming_link.as_link().unwrap().from)
//            } else {
//                format!("Object {}", object.index())
//            }
//        }
//    }
//}
//
//fn object_link<'a>(object: ObjectRef<'a>) -> Box<Render> {
//    let url = object_url(object.index());
//    let class = object_html_class(object);
//    let name = object_name(object);
//    box_html! {
//        a(href=&url, class=&class) : &name;
//    }
//}
//
//fn page_for_object<'a>(object: ObjectRef<'a>) -> Response {
//    let graph = object.graph();
//    let name = object_name(object);
//
//    let nav = html! {
//        a(href=format!("/create/link/to/{}", object.index()), class="link") : "Link to";
//        a(href=format!("/create/link/from/{}", object.index()), class="link") : "Link from";
//        a(id="edit_description_start") : "Description";
//        @ if object.in_links().is_empty() && object.out_links().is_empty() {
//            a(id="remove_start") : "Remove";
//        }
//    };
//    let description = html! {
//        @ for paragraph in object.description().split("\n\n") {
//            p(class="description") : paragraph;
//        }
//    };
//    let overlays = html! {
//        div(id="edit_description_overlay", class="overlay") {
//            form(class="vbox", method="post", action="/edit/description") {
//                input(type="hidden", name="index", value=object.index());
//                textarea(id="edit_description_content", name="content", autofocus, placeholder="Description...") : object.description();
//                div(class="hbox") {
//                    a(class="button", id="edit_description_cancel") : "Cancel";
//                    input(class="button", type="submit", value="Edit");
//                }
//            }
//        }
//        div(id="remove_overlay", class="overlay") {
//            form(class="vbox", method="post", action="/remove") {
//                input(type="hidden", name="index", value=object.index());
//                h1 : format!("Remove {} ?", name);
//                div(class="hbox") {
//                    a(class="button", id="remove_cancel") : "Cancel";
//                    input(class="button", type="submit", value="Remove");
//                }
//            }
//        }
//    };
//    let content = html! {
//        h1(class=object_html_class(object)) : &name;
//        : description;
//        @ if let Object::Link(ref l) = *object {
//            ul {
//                li {
//                    : "From ";
//                    : object_link(graph.object(l.from));
//                }
//                li {
//                    : "To ";
//                    : object_link(graph.object(l.to));
//                }
//            }
//        }
//        h2 : "Linked from";
//        ul {
//            @ for object in object.in_links() {
//                li : object_link(object);
//            }
//        }
//        h2 : "Links to";
//        ul {
//            @ for object in object.out_links() {
//                li : object_link(object);
//            }
//        }
//        : overlays;
//    };
//    wiki_page(&name, nav, content)
//}
//
//fn main_page(graph: &Graph) -> Response {
//    if let Some(wiki_main) = graph.get_atom(&Atom::text("_wiki_main")) {
//        if let Some(out_link) = wiki_main.out_links().first() {
//            if let Object::Link(ref l) = *out_link {
//                return Response::redirect_303(object_url(l.to));
//            }
//        }
//    }
//    let content = html! {
//        p : "Create a link from \"_wiki_main\" to a graph object to define it as the home page.";
//    };
//    wiki_page("Main page", html!{}, content)
//}
//
//fn page_all_objects(graph: &Graph) -> Response {
//    let content = html! {
//        h1(class="atom") : "Atoms";
//        ul {
//            @ for object in graph.objects().filter(|o| o.is_atom()) {
//                li : object_link(object);
//            }
//        }
//        h1(class="abstract") : "Abstract";
//        ul {
//            @ for object in graph.objects().filter(|o| o.is_abstract()) {
//                li : object_link(object);
//            }
//        }
//        h1(class="link") : "Links";
//        ul {
//            @ for object in graph.objects().filter(|o| o.is_link()) {
//                li : object_link(object);
//            }
//        }
//    };
//    wiki_page("Object list", html!{}, content)
//}
//
//fn page_create_atom() -> Response {
//    let content = html! {
//        form(method="post") {
//            : "Value:";
//            input(type="text", name="text");
//            input(type="submit", value="Create");
//        }
//    };
//    wiki_page("Create atom", html!{}, content)
//}
//fn post_create_atom(request: &Request, graph: &mut Graph) -> Response {
//    let form_data = try_or_400!(post_input!(request, { text: String }));
//    let text = form_data.text.trim();
//    let index = graph.use_atom(Atom::text(text));
//    Response::redirect_303(object_url(index))
//}
//
//fn page_create_abstract() -> Response {
//    let content = html! {
//        form(method="post") {
//            : "Optional name:";
//            input(type="text", name="name");
//            input(type="submit", value="Create");
//        }
//    };
//    wiki_page("Create abstract", html!{}, content)
//}
//fn post_create_abstract(request: &Request, graph: &mut Graph) -> Response {
//    let form_data = try_or_400!(post_input!(request, { name: String }));
//    let name = form_data.name.trim();
//    let abstract_index = graph.create_abstract();
//    if name != "" {
//        let atom = graph.use_atom(Atom::text(name));
//        let _ = try_or_400!(graph.use_link(Link::new(atom, abstract_index)));
//    }
//    Response::redirect_303(object_url(abstract_index))
//}
//
//fn page_create_link<'a>(object: ObjectRef<'a>, link_side: LinkSide) -> Response {
//    let defined_link_side_text = match link_side {
//        LinkSide::From => "from",
//        LinkSide::To => "to",
//    };
//    let undefined_link_side_text = match link_side {
//        LinkSide::From => "to",
//        LinkSide::To => "from",
//    };
//    let graph = object.graph();
//    let name = object_name(object);
//    let class = object_html_class(object);
//    let url = object_url(object.index());
//
//    let title = format!("Create link {} {}", defined_link_side_text, name);
//    let content = html! {
//        p {
//            : format!("Create link {} ", defined_link_side_text);
//            a(href=url, class=class) : name;
//        }
//        form(method="post", action="/create/link") {
//            input(type="hidden", name=defined_link_side_text, value=object.index());
//            @ for object in graph.objects() {
//                input(type="radio", name=undefined_link_side_text, value=object.index()) {
//                    : object_name(object);
//                }
//                br;
//            }
//            input(type="submit", value="Create");
//        }
//    };
//    wiki_page(title, html!{}, content)
//}
//fn post_create_link(request: &Request, graph: &mut Graph) -> Response {
//    let l = try_or_400!(post_input!(request, { from: Index, to: Index }));
//    let index = try_or_400!(graph.use_link(Link::new(l.from, l.to)));
//    Response::redirect_303(object_url(index))
//}
//
//fn post_edit_description(request: &Request, graph: &mut Graph) -> Response {
//    let mut form_data = try_or_400!(post_input!(request, { index: Index, content: String}));
//    form_data.content.retain(|c| c != '\r');
//    let _ = try_or_400!(graph.set_description(form_data.index, form_data.content));
//    Response::redirect_303(object_url(form_data.index))
//}
//fn post_remove(request: &Request, graph: &mut Graph) -> Response {
//    let form_data = try_or_400!(post_input!(request, { index: Index }));
//    let _ = try_or_400!(graph.remove_object(form_data.index));
//    Response::redirect_303("/")
//}

// Page template
fn wiki_page<T, N, C>(title: T, additional_nav_links: N, content: C) -> Response
where
    T: RenderOnce,
    N: RenderOnce,
    C: RenderOnce,
{
    let template = html! {
        : horrorshow::helper::doctype::HTML;
        html {
            head {
                link(rel="stylesheet", type="text/css", href="/static/style.css");
                script(src="/static/jquery.js") {}
                meta(name="viewport", content="width=device-width, initial-scale=1.0");
                title : title;
            }
            body {
                nav {
                    a(href="/") : "Home";
                    a(href="/create/atom", class="atom") : "Atom";
                    a(href="/create/abstract", class="abstract") : "Abstract";
                    : additional_nav_links;
                    a(href="/all") : "All";
                    a(href="/static/doc.html") : "Doc";
                    // TODO other
                }
                main {
                    : content;
                }
                script(src="/static/client.js") {}
            }
        }
    };
    Response::html(template.into_string().unwrap())
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
            path if path.ends_with(".pdf") => "application/pdf",
            path if path.ends_with(".ico") => "image/vnd.microsoft.icon",
            _ => "application/octet-stream",
        };
        Response::from_data(content_type, asset) //FIXME.with_public_cache(3600)
    } else {
        Response::empty_404()
    }
}
