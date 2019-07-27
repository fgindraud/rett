use hyper::service::service_fn;
use hyper::{Body, Method, Request, Response, Server, StatusCode};
use maud::{html, Markup, PreEscaped};
use signal_hook::{self, iterator::Signals};
use tokio::prelude::{Future, Stream};
use tokio::runtime::current_thread;
use tokio::timer;

use std::cell;
use std::fs;
use std::net::SocketAddr;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::time::Duration;

use relations::{read_database_from_file, write_database_to_file};
use relations::{Abstract, Atom, Database, Element, ElementRef, Index, Ref, Relation};
use utils::remove_prefix;

/// Mini web framework.
mod web;
use self::web::{EndPoint, FromRequestError, FromRequestOk};

/******************************************************************************
 * Wiki runtime system.
 * Based on hyper/tokio, but uses the single threaded tokio runtime.
 */

/// Entry point, run the wiki server.
pub fn run(
    addr: &SocketAddr,
    database_file: &Path,
    backup_file: &Path,
    autosave_interval: Duration,
) -> Result<(), String> {
    let state = Rc::new(State::from_file(database_file, backup_file)?);

    let create_service = || {
        let state = state.clone();
        service_fn(move |request| {
            // Move cloned rc ref in this scope.
            let handlers = [
                web::end_point_handler::<DisplayElement>,
                web::end_point_handler::<Homepage>,
                web::end_point_handler::<ListAllElements>,
                web::end_point_handler::<SearchAtom>,
                web::end_point_handler::<CreateAtom>,
                web::end_point_handler::<CreateAbstract>,
                web::end_point_handler::<CreateRelation>,
                web::end_point_handler::<RemoveElement>,
                web::end_point_handler::<StaticAsset>,
            ];
            web::handle_request(request, state.clone(), handlers.iter())
        })
    };
    let server = Server::bind(&addr)
        .executor(current_thread::TaskExecutor::current())
        .serve(create_service);
    let shutdown_signal = Signals::new(&[signal_hook::SIGTERM, signal_hook::SIGINT])
        .map_err(|e| e.to_string())?
        .into_async() // Stream of signals
        .map_err(|e| e.to_string())?
        .into_future(); // Only look at first signal
    let wiki = server
        .with_graceful_shutdown(shutdown_signal.map(|_| ()))
        .map_err(|e| e.to_string());

    let database_autosave = timer::Interval::new_interval(autosave_interval)
        .map_err(|e| e.to_string())
        .for_each({
            let state = state.clone();
            move |_instant| state.write_to_file()
        });

    // Launch both autosave and wiki, stop whenever one terminates.
    // select() return the other future, throw it away to let the runtime stop.
    let which_terminates_first = Future::select(wiki, database_autosave).then(|f| match f {
        Ok((v, _)) => Ok(v),
        Err((e, _)) => Err(e),
    });
    current_thread::block_on_all(which_terminates_first)?;
    state.write_to_file()?;
    Ok(())
}

/// Wiki web interface state.
struct State {
    mutable: cell::RefCell<InnerMutableState>,
    database_file: PathBuf,
    backup_file: PathBuf,
}
struct InnerMutableState {
    database: Database,
    modified_since_last_write: bool,
}
impl State {
    fn from_file(database_file: &Path, backup_file: &Path) -> Result<Self, String> {
        let init_database = match read_database_from_file(database_file) {
            Ok(database) => database,
            Err(e) => {
                eprintln!("[warning] {}", e);
                eprintln!("[database] Starting with empty database");
                let db = Database::new();
                // Write empty database so that autosave process does not fail
                write_database_to_file(database_file, &db)?;
                db
            }
        };
        Ok(State {
            mutable: cell::RefCell::new(InnerMutableState {
                database: init_database,
                modified_since_last_write: false,
            }),
            database_file: database_file.to_owned(),
            backup_file: backup_file.to_owned(),
        })
    }
    fn write_to_file(&self) -> Result<(), String> {
        let inner = &mut self.mutable.borrow_mut();
        if inner.modified_since_last_write {
            inner.modified_since_last_write = false;
            fs::rename(&self.database_file, &self.backup_file)
                .map_err(|e| format!("Cannot move backup: {}", e))?;
            write_database_to_file(&self.database_file, &inner.database)?
        }
        Ok(())
    }
    fn get(&self) -> cell::Ref<Database> {
        cell::Ref::map(self.mutable.borrow(), |s| &s.database)
    }
    fn get_mut(&self) -> cell::RefMut<Database> {
        let mut inner = self.mutable.borrow_mut();
        inner.modified_since_last_write = true;
        cell::RefMut::map(inner, |s| &mut s.database)
    }
}

/******************************************************************************
 * Wiki page definitions.
 */

#[derive(Clone, Default)]
struct EditState {
    // Relation
    subject: Option<Index>,
    descriptor: Option<Index>,
    complement: Option<Index>,
}
impl web::QueryFormat for EditState {
    fn to_query(&self, builder: &mut web::PathQueryBuilder) {
        builder.optional_entry("subject", self.subject);
        builder.optional_entry("descriptor", self.descriptor);
        builder.optional_entry("complement", self.complement);
    }
    fn from_query(entries: &web::UrlDecodedEntries) -> Result<Self, web::Error> {
        Ok(EditState {
            subject: parse_optional_index(entries.get("subject"))?,
            descriptor: parse_optional_index(entries.get("descriptor"))?,
            complement: parse_optional_index(entries.get("complement"))?,
        })
    }
}

/// Display an element of the relation graph.
struct DisplayElement {
    index: Index,
    edit_state: EditState,
}
impl DisplayElement {
    fn url(index: Index, edit_state: &EditState) -> String {
        web::to_path_and_query(format!("/element/{}", index), edit_state)
    }
}
impl EndPoint for DisplayElement {
    type State = State;
    fn from_request(r: Request<Body>) -> Result<FromRequestOk<Self>, FromRequestError> {
        match (r.method(), remove_prefix(r.uri().path(), "/element/")) {
            (&Method::GET, Some(index)) => Ok(FromRequestOk::Value(DisplayElement {
                index: parse_index(index)?,
                edit_state: web::from_query(r.uri().query())?,
            })),
            _ => Err(FromRequestError::NoMatch(r)),
        }
    }
    fn generate_response(self, state: &State) -> Response<Body> {
        match state.get().element(self.index) {
            Ok(element) => web::response_html(display_element_page(element, &self.edit_state)),
            Err(_) => web::response_empty_404(),
        }
    }
}
fn display_element_page(element: Ref<Element>, edit_state: &EditState) -> String {
    let basic_name = html! {
        (match element.value() {
            Element::Abstract => lang::ABSTRACT,
            Element::Atom(_) => lang::ATOM,
            Element::Relation(_) => lang::RELATION,
        }) "#" (element.index())
    };
    let name = element_name(element, 1);
    let title = html! { (basic_name) " - " (name) };
    let descriptions = {
        let mut v: Vec<_> =
            Iterator::chain(element.subject_of().iter(), element.complement_of().iter()).collect();
        v.sort_by_key(|r: &Ref<Relation>| {
            // Group descriptions by descriptor.
            // For the same descriptor, put element on top.
            // This relies on None < Some(_)
            (
                r.descriptor().index(),
                Some(r.subject().index()).filter(|&i| i != element.index()),
            )
        });
        v
    };
    let descriptor_of = element.descriptor_of();
    let relation_component_row = |r: Ref<Relation>| -> Markup {
        html! {
            tr {
                td { a.relation href=(DisplayElement::url(r.index(), edit_state)) { "#" (r.index()) } }
                td {
                    (element_link(r.subject(), edit_state)) " " (element_link(r.descriptor(), edit_state))
                    @if let Some(complement) = r.complement() { " " (element_link(complement, edit_state)) }
                }
            }
        }
    };
    let content = html! {
        h1 class=(css_class_name(element)) { (name) }
        p {
            (basic_name)
            @match element.cases() {
                ElementRef::Abstract(r) => @if let Some(r) = naming_atom(r) { ": " (atom_link(r, edit_state)) },
                ElementRef::Atom(r) => ": " (atom_name(r)),
                ElementRef::Relation(r) => {
                    br;
                    (element_link(r.subject(), edit_state)) " " (element_link(r.descriptor(), edit_state))
                    @if let Some(complement) = r.complement() { " " (element_link(complement, edit_state)) }
                }
            }
            @if descriptions.len() > 0 {
                table {
                    @for d in descriptions { (relation_component_row(d)) }
                }
            }
            @if descriptor_of.len() > 0 {
                p { (lang::DISPLAY_DESCRIBES) ":" }
                table {
                    @for d in descriptor_of.iter() { (relation_component_row(d)) }
                }
            }
        }
    };
    let nav = navigation_links(edit_state, Some(element));
    compose_wiki_page(title, content, nav)
}

/// Homepage : links to selected elements.
struct Homepage {
    edit_state: EditState,
}
impl Homepage {
    fn url(edit_state: &EditState) -> String {
        web::to_path_and_query("/", edit_state)
    }
}
impl EndPoint for Homepage {
    type State = State;
    fn from_request(r: Request<Body>) -> Result<FromRequestOk<Self>, FromRequestError> {
        match (r.method(), r.uri().path()) {
            (&Method::GET, "/") => Ok(FromRequestOk::Value(Homepage {
                edit_state: web::from_query(r.uri().query())?,
            })),
            _ => Err(FromRequestError::NoMatch(r)),
        }
    }
    fn generate_response(self, state: &State) -> Response<Body> {
        let database = state.get();
        let content = html! {
            h1 { (lang::HOMEPAGE) }
            @if let Some(wiki_homepage) = database.get_text_atom("_wiki_homepage") {
                ul {
                    @for tagged in wiki_homepage.descriptor_of().iter().map(|tag_relation| tag_relation.subject()) {
                        li { (element_link(tagged, &self.edit_state)) }
                    }
                }
            }
            form.hbox method="post" action=(CreateAtom::url(&self.edit_state)) {
                label for="wiki_homepage" { (lang::HOMEPAGE_HELP) }
                button#wiki_homepage { "_wiki_homepage" }
                input type="hidden" name="text" value="_wiki_homepage";
            }
        };
        let nav = navigation_links(&self.edit_state, None);
        let page = compose_wiki_page(lang::HOMEPAGE, content, nav);
        web::response_html(page)
    }
}

/// List all elements.
struct ListAllElements {
    edit_state: EditState,
}
impl ListAllElements {
    fn url(edit_state: &EditState) -> String {
        web::to_path_and_query("/all", edit_state)
    }
}
impl EndPoint for ListAllElements {
    type State = State;
    fn from_request(r: Request<Body>) -> Result<FromRequestOk<Self>, FromRequestError> {
        match (r.method(), r.uri().path()) {
            (&Method::GET, "/all") => Ok(FromRequestOk::Value(ListAllElements {
                edit_state: web::from_query(r.uri().query())?,
            })),
            _ => Err(FromRequestError::NoMatch(r)),
        }
    }
    fn generate_response(self, state: &State) -> Response<Body> {
        let database = state.get();
        let content = html! {
            h1 { (lang::ALL_ELEMENTS_TITLE) }
            ul {
                @for element in database.iter() {
                    li { (element_link(element, &self.edit_state)) }
                }
            }
        };
        let nav = navigation_links(&self.edit_state, None);
        let page = compose_wiki_page(lang::ALL_ELEMENTS_TITLE, content, nav);
        web::response_html(page)
    }
}

/// Search by name in the list of atoms.
struct SearchAtom {
    pattern: Option<String>,
    edit_state: EditState,
}
impl SearchAtom {
    fn url(edit_state: &EditState) -> String {
        web::to_path_and_query("/search/atom", edit_state)
    }
}
impl EndPoint for SearchAtom {
    type State = State;
    fn from_request(r: Request<Body>) -> Result<FromRequestOk<Self>, FromRequestError> {
        match (r.method(), r.uri().path()) {
            (&Method::GET, "/search/atom") => Ok(FromRequestOk::Value(SearchAtom {
                pattern: None,
                edit_state: web::from_query(r.uri().query())?,
            })),
            (&Method::POST, "/search/atom") => {
                let edit_state = web::from_query(r.uri().query())?;
                web::with_post_entries(r, move |entries| {
                    let pattern = entries.get("pattern").ok_or(web::Error::BadRequest)?;
                    Ok(SearchAtom {
                        pattern: Some(pattern.to_string()),
                        edit_state,
                    })
                })
            }
            _ => Err(FromRequestError::NoMatch(r)),
        }
    }
    fn generate_response(self, state: &State) -> Response<Body> {
        let content = html! {
            h1.atom { (lang::SEARCH_ATOM_TITLE) }
            form.vbox method="post" action=(SearchAtom::url(&self.edit_state)) {
                input type="text" name="pattern" required? placeholder=(lang::ATOM_TEXT)
                    value=(match self.pattern.as_ref() {
                        Some(s) => s.as_str(),
                        None => "",
                    });
                button { (lang::COMMIT_BUTTON) }
            }
            @if let Some(pattern) = self.pattern {
                @let database = state.get();
                @let results = database.text_atom_fuzzy_matches(&pattern);
                table {
                    @for (atom, score) in results.iter().take(40) {
                        tr {
                            td { (score) }
                            td { (atom_link(atom, &self.edit_state)) }
                        }
                    }
                }
            }
        };
        let nav = navigation_links(&self.edit_state, None);
        let page = compose_wiki_page(lang::SEARCH_ATOM_TITLE, content, nav);
        web::response_html(page)
    }
}

/// Create an atom.
enum CreateAtom {
    Get { edit_state: EditState },
    Post { text: String, edit_state: EditState },
}
impl CreateAtom {
    fn url(edit_state: &EditState) -> String {
        web::to_path_and_query("/create/atom", edit_state)
    }
}
impl EndPoint for CreateAtom {
    //TODO implement preview button: fuzzy search current text content and propose similar atoms
    type State = State;
    fn from_request(r: Request<Body>) -> Result<FromRequestOk<Self>, FromRequestError> {
        match (r.method(), r.uri().path()) {
            (&Method::GET, "/create/atom") => Ok(FromRequestOk::Value(CreateAtom::Get {
                edit_state: web::from_query(r.uri().query())?,
            })),
            (&Method::POST, "/create/atom") => {
                let edit_state = web::from_query(r.uri().query())?;
                web::with_post_entries(r, move |entries| {
                    let text = entries.get("text").ok_or(web::Error::BadRequest)?;
                    let text = text.to_string();
                    Ok(CreateAtom::Post { text, edit_state })
                })
            }
            _ => Err(FromRequestError::NoMatch(r)),
        }
    }
    fn generate_response(self, state: &State) -> Response<Body> {
        match self {
            CreateAtom::Get { edit_state } => {
                let content = html! {
                    h1.atom { (lang::CREATE_ATOM_TITLE) }
                    form.vbox method="post" action=(CreateAtom::url(&edit_state)) {
                        input type="text" name="text" required? placeholder=(lang::ATOM_TEXT);
                        div.hbox {
//                            button formmethod="get" { (lang::PREVIEW_BUTTON) }
                            button { (lang::COMMIT_BUTTON) }
                        }
                    }
                };
                let nav = navigation_links(&edit_state, None);
                let page = compose_wiki_page(lang::CREATE_ATOM_TITLE, content, nav);
                web::response_html(page)
            }
            CreateAtom::Post { text, edit_state } => {
                let index = state.get_mut().insert_atom(Atom::from(text));
                web::response_redirection(&DisplayElement::url(index, &edit_state))
            }
        }
    }
}

/// Create an atom.
enum CreateAbstract {
    Get {
        edit_state: EditState,
    },
    Post {
        name: Option<String>,
        edit_state: EditState,
    },
}
impl CreateAbstract {
    fn url(edit_state: &EditState) -> String {
        web::to_path_and_query("/create/abstract", edit_state)
    }
}
impl EndPoint for CreateAbstract {
    type State = State;
    fn from_request(r: Request<Body>) -> Result<FromRequestOk<Self>, FromRequestError> {
        match (r.method(), r.uri().path()) {
            (&Method::GET, "/create/abstract") => Ok(FromRequestOk::Value(CreateAbstract::Get {
                edit_state: web::from_query(r.uri().query())?,
            })),
            (&Method::POST, "/create/abstract") => {
                let edit_state = web::from_query(r.uri().query())?;
                web::with_post_entries(r, move |entries| {
                    let name = entries.get("name").ok_or(web::Error::BadRequest)?;
                    let name = match name {
                        "" => None,
                        _ => Some(name.to_string()),
                    };
                    Ok(CreateAbstract::Post { name, edit_state })
                })
            }
            _ => Err(FromRequestError::NoMatch(r)),
        }
    }
    fn generate_response(self, state: &State) -> Response<Body> {
        match self {
            CreateAbstract::Get { edit_state } => {
                let content = html! {
                    h1.abstract { (lang::CREATE_ABSTRACT_TITLE) }
                    form.vbox method="post" action=(CreateAbstract::url(&edit_state)) {
                        input type="text" name="name" placeholder=(lang::CREATE_ABSTRACT_NAME_PLACEHOLDER);
                        div.hbox {
//                            button name="preview" formmethod="get" { (lang::PREVIEW_BUTTON) }
                            button { (lang::COMMIT_BUTTON) }
                        }
                    }
                };
                let nav = navigation_links(&edit_state, None);
                let page = compose_wiki_page(lang::CREATE_ABSTRACT_TITLE, content, nav);
                web::response_html(page)
            }
            CreateAbstract::Post { name, edit_state } => {
                let database = &mut state.get_mut();
                let index = database.create_abstract_element();
                if let Some(name) = name {
                    let name_element = database.insert_atom(Atom::from(name));
                    let is_named_atom = database.insert_atom(Atom::from(lang::NAMED_ATOM));
                    let _naming_relation = database.insert_relation(Relation {
                        subject: index,
                        descriptor: is_named_atom,
                        complement: Some(name_element),
                    });
                }
                web::response_redirection(&DisplayElement::url(index, &edit_state))
            }
        }
    }
}

/// Create a Relation.
enum CreateRelation {
    Get {
        edit_state: EditState,
    },
    Post {
        relation: Relation,
        edit_state: EditState,
    },
}
impl CreateRelation {
    fn url(edit_state: &EditState) -> String {
        web::to_path_and_query("/create/relation", edit_state)
    }
}
impl EndPoint for CreateRelation {
    type State = State;
    fn from_request(r: Request<Body>) -> Result<FromRequestOk<Self>, FromRequestError> {
        match (r.method(), r.uri().path()) {
            (&Method::GET, "/create/relation") => Ok(FromRequestOk::Value(CreateRelation::Get {
                edit_state: web::from_query(r.uri().query())?,
            })),
            (&Method::POST, "/create/relation") => {
                let edit_state = web::from_query(r.uri().query())?;
                web::with_post_entries(r, move |entries| {
                    // Missing fields implies not using the form, fail with bad request.
                    let relation = Relation {
                        subject: parse_required_index(entries.get("subject"))?,
                        descriptor: parse_required_index(entries.get("descriptor"))?,
                        complement: parse_optional_index(entries.get("complement"))?,
                    };
                    Ok(CreateRelation::Post {
                        relation,
                        edit_state,
                    })
                })
            }
            _ => Err(FromRequestError::NoMatch(r)),
        }
    }
    fn generate_response(self, state: &State) -> Response<Body> {
        match self {
            CreateRelation::Get { edit_state } => {
                let database = state.get();
                let enable_form = {
                    let valid_or =
                        |i: Option<Index>, d| i.map_or(d, |i| database.element(i).is_ok());
                    valid_or(edit_state.subject, false)
                        && valid_or(edit_state.descriptor, false)
                        && valid_or(edit_state.complement, true)
                };
                let field_preview = |name: PreEscaped<&str>,
                                     index: Option<Index>,
                                     allow_missing: bool|
                 -> Markup {
                    html! {
                        tr {
                            td { (name) }
                            @match index {
                                None => @match allow_missing {
                                    true => td;,
                                    false => td.error { (lang::CREATE_RELATION_MISSING) },
                                },
                                Some(index) => @match database.element(index) {
                                    Ok(element) => td { (element_link(element, &edit_state)) },
                                    Err(_) => td.error { (lang::INVALID_ELEMENT_INDEX) ": " (index) },
                                }
                            }
                        }
                    }
                };
                let content = html! {
                    h1.relation { (lang::CREATE_RELATION_TITLE) }
                    form.vbox method="post" action=(CreateRelation::url(&edit_state)) {
                        table {
                            (field_preview(lang::RELATION_SUBJECT, edit_state.subject, false))
                            (field_preview(lang::RELATION_DESCRIPTOR, edit_state.descriptor, false))
                            (field_preview(lang::RELATION_COMPLEMENT, edit_state.complement, true))
                        }
                        @if let Some(subject) = edit_state.subject {
                            input type="hidden" name="subject" value=(subject);
                        }
                        @if let Some(descriptor) = edit_state.descriptor {
                            input type="hidden" name="descriptor" value=(descriptor);
                        }
                        @if let Some(complement) = edit_state.complement {
                            input type="hidden" name="complement" value=(complement);
                        }
                        button disabled?[!enable_form] { (lang::COMMIT_BUTTON) }
                    }
                };
                let nav = navigation_links(&edit_state, None);
                let page = compose_wiki_page(lang::CREATE_RELATION_TITLE, content, nav);
                web::response_html(page)
            }
            CreateRelation::Post {
                relation,
                edit_state,
            } => {
                let insertion = state.get_mut().insert_relation(relation);
                web::response_redirection(&match insertion {
                    Ok(index) => DisplayElement::url(index, &EditState::default()),
                    Err(_) => CreateRelation::url(&edit_state), // Allow retrying
                })
            }
        }
    }
}

// Modes {
// - one element (must be orphaned), page is preview, default
// - recursive (no requirements): preview with list, revert if list changed
// - recursive + orphans ?
struct RemoveElement {
    index: Index,
    edit_state: EditState,
}
impl RemoveElement {
    fn url(index: Index, edit_state: &EditState) -> String {
        web::to_path_and_query(format!("/remove/{}", index), edit_state)
    }
}
impl EndPoint for RemoveElement {
    type State = State;
    fn from_request(r: Request<Body>) -> Result<FromRequestOk<Self>, FromRequestError> {
        match (r.method(), remove_prefix(r.uri().path(), "/remove/")) {
            (&Method::GET, Some(index)) => Ok(FromRequestOk::Value(RemoveElement {
                index: parse_index(index)?,
                edit_state: web::from_query(r.uri().query())?,
            })),
            _ => Err(FromRequestError::NoMatch(r)),
        }
    }
    fn generate_response(self, state: &State) -> Response<Body> {
        web::response_empty_404() //TODO
    }
}

///////////////////////////////////////////////////////////////////////////////
/// Language specific configuration.
/// Contains text constants.
mod lang {
    use maud::PreEscaped;
    type ConstStr = PreEscaped<&'static str>;

    pub const NAMED_ATOM: &'static str = "est nommé";

    pub const COMMIT_BUTTON: ConstStr = PreEscaped("Valider");
    pub const PREVIEW_BUTTON: ConstStr = PreEscaped("Prévisualiser");
    pub const INVALID_ELEMENT_INDEX: ConstStr = PreEscaped("Index invalide");

    pub const RELATION: ConstStr = PreEscaped("Relation");
    pub const ATOM: ConstStr = PreEscaped("Atome");
    pub const ABSTRACT: ConstStr = PreEscaped("Abstrait");
    pub const DISPLAY_DESCRIBES: ConstStr = PreEscaped("Décrit");

    pub const HOMEPAGE: ConstStr = PreEscaped("Accueil");
    pub const HOMEPAGE_HELP: ConstStr =
        PreEscaped("Pour lister un élément sur cette page, il doit être taggé par _wiki_homepage.");

    pub const ALL_ELEMENTS_NAV: ConstStr = PreEscaped("Éléments");
    pub const ALL_ELEMENTS_TITLE: ConstStr = PreEscaped("Liste des éléments");

    pub const SEARCH_ATOM_NAV: ConstStr = PreEscaped("Chercher");
    pub const SEARCH_ATOM_TITLE: ConstStr = PreEscaped("Recherche par texte");

    pub const ATOM_TEXT: ConstStr = PreEscaped("Texte");
    pub const CREATE_ATOM_NAV: ConstStr = PreEscaped("Atome...");
    pub const CREATE_ATOM_TITLE: ConstStr = PreEscaped("Ajouter un atome...");

    pub const CREATE_ABSTRACT_NAV: ConstStr = PreEscaped("Abstrait...");
    pub const CREATE_ABSTRACT_TITLE: ConstStr = PreEscaped("Ajouter un élément abstrait...");
    pub const CREATE_ABSTRACT_NAME_PLACEHOLDER: ConstStr = PreEscaped("Nom optionel");

    pub const RELATION_SUBJECT: ConstStr = PreEscaped("Sujet");
    pub const RELATION_DESCRIPTOR: ConstStr = PreEscaped("Verbe");
    pub const RELATION_COMPLEMENT: ConstStr = PreEscaped("Objet");
    pub const CREATE_RELATION_NAV: ConstStr = PreEscaped("Relation...");
    pub const CREATE_RELATION_TITLE: ConstStr = PreEscaped("Ajouter une relation...");
    pub const CREATE_RELATION_MISSING: ConstStr = PreEscaped("Champ manquant !");

    pub const REMOVE_ELEMENT_NAV: ConstStr = PreEscaped("Supprimer");
}

fn css_class_name(element: Ref<Element>) -> &'static str {
    match element.value() {
        Element::Abstract => "abstract",
        Element::Atom(_) => "atom",
        Element::Relation(_) => "relation",
    }
}

/// Atom default representation: with its text.
fn atom_name(r: Ref<Atom>) -> Markup {
    match r.value() {
        Atom::Text(s) => html! { (s) },
    }
}
/// Abstract default representation: find a naming atom, or use index.
fn abstract_name(r: Ref<Abstract>) -> Markup {
    match naming_atom(r) {
        Some(r) => atom_name(r),
        None => html! { (lang::ABSTRACT) "#" (r.index()) },
    }
}
fn naming_atom(r: Ref<Abstract>) -> Option<Ref<Atom>> {
    r.database()
        .index_of_text_atom(lang::NAMED_ATOM)
        .and_then(|is_named| {
            r.subject_of().iter().find_map(|r| {
                // Search for first naming relation, restricted to atom names
                if r.descriptor().index() == is_named {
                    r.complement().and_then(|r| match r.cases() {
                        ElementRef::Atom(r) => Some(r),
                        _ => None,
                    })
                } else {
                    None
                }
            })
        })
}
/// Relation representation: index, or components recursively.
fn relation_name(r: Ref<Relation>, depth: u64) -> Markup {
    if depth > 0 {
        let element_name_nested = |r: Ref<Element>| -> Markup {
            match r.cases() {
                ElementRef::Atom(r) => atom_name(r),
                ElementRef::Abstract(r) => abstract_name(r),
                ElementRef::Relation(r) => {
                    let depth = depth - 1;
                    if depth > 0 {
                        html! { "(" (relation_name(r, depth)) ")" }
                    } else {
                        relation_name(r, depth)
                    }
                }
            }
        };
        html! {
            (element_name_nested(r.subject())) " " (element_name_nested(r.descriptor()))
            @if let Some(complement) = r.complement() { " " (element_name_nested(complement)) }
        }
    } else {
        html! { (lang::RELATION) "#" (r.index()) }
    }
}
fn element_name(r: Ref<Element>, depth: u64) -> Markup {
    match r.cases() {
        ElementRef::Atom(r) => atom_name(r),
        ElementRef::Abstract(r) => abstract_name(r),
        ElementRef::Relation(r) => relation_name(r, depth),
    }
}

fn atom_link(r: Ref<Atom>, edit_state: &EditState) -> Markup {
    html! {
        a.atom href=(DisplayElement::url(r.index(), edit_state)) { (atom_name(r)) }
    }
}
fn abstract_link(r: Ref<Abstract>, edit_state: &EditState) -> Markup {
    html! {
        a.abstract href=(DisplayElement::url(r.index(), edit_state)) { (abstract_name(r)) }
    }
}
fn relation_link(r: Ref<Relation>, edit_state: &EditState) -> Markup {
    html! {
        a.relation href=(DisplayElement::url(r.index(), edit_state)) { (relation_name(r, 1)) }
    }
}
fn element_link(r: Ref<Element>, edit_state: &EditState) -> Markup {
    match r.cases() {
        ElementRef::Atom(r) => atom_link(r, edit_state),
        ElementRef::Abstract(r) => abstract_link(r, edit_state),
        ElementRef::Relation(r) => relation_link(r, edit_state),
    }
}

/// Generates sequence of navigation links depending on state.
fn navigation_links(edit_state: &EditState, displayed: Option<Ref<Element>>) -> Markup {
    let displayed = displayed.map(|r| r.index());
    html! {
        a href=(Homepage::url(edit_state)) { (lang::HOMEPAGE) }
        a href=(ListAllElements::url(edit_state)) { (lang::ALL_ELEMENTS_NAV) }
        a.atom href=(SearchAtom::url(edit_state)) { (lang::SEARCH_ATOM_NAV) }
        a.atom href=(CreateAtom::url(edit_state)) { (lang::CREATE_ATOM_NAV) }
        a.abstract href=(CreateAbstract::url(edit_state)) { (lang::CREATE_ABSTRACT_NAV) }
        (selection_nav_link(lang::RELATION_SUBJECT, displayed, edit_state, |e| e.subject, |e,subject| EditState{ subject, ..*e }))
        (selection_nav_link(lang::RELATION_DESCRIPTOR, displayed, edit_state, |e| e.descriptor, |e,descriptor| EditState{ descriptor, ..*e }))
        (selection_nav_link(lang::RELATION_COMPLEMENT, displayed, edit_state, |e| e.complement, |e,complement| EditState{ complement, ..*e }))
        a.relation href=(CreateRelation::url(edit_state)) { (lang::CREATE_RELATION_NAV) }
        @if let Some(index) = displayed {
            a href=(RemoveElement::url(index, edit_state)) { (lang::REMOVE_ELEMENT_NAV) }
        }
    }
}
fn selection_nav_link<IFV, WFV>(
    field_text: PreEscaped<&str>,
    displayed: Option<Index>,
    edit_state: &EditState,
    init_field_value: IFV,
    with_field_value: WFV,
) -> Markup
where
    IFV: FnOnce(&EditState) -> Option<Index>,
    WFV: FnOnce(&EditState, Option<Index>) -> EditState,
{
    html! {
        @match (init_field_value(edit_state), displayed) {
            (None, None) => {},
            (Some(selected), Some(displayed)) => @if selected == displayed {
                a.relation href=(DisplayElement::url(displayed, &with_field_value(edit_state, None))) {
                    "-" (field_text) " #" (selected)
                }
            } @else {
                a.relation href=(DisplayElement::url(displayed, &with_field_value(edit_state, Some(displayed)))) {
                    "=" (field_text) " #" (selected)
                }
            },
            (None, Some(displayed)) => {
                a.relation href=(DisplayElement::url(displayed, &with_field_value(edit_state, Some(displayed)))) {
                    "+" (field_text)
                }
            },
            (Some(selected), None) => {
                a.relation href=(DisplayElement::url(selected, edit_state)) {
                    (field_text) " #" (selected)
                }
            }
        }
    }
}

/// Generated wiki page final assembly. Adds the navigation bar, overall html structure.
fn compose_wiki_page<T: AsRef<str>>(
    title: PreEscaped<T>,
    content: Markup,
    navigation_links: Markup,
) -> String {
    let template = html! {
        (maud::DOCTYPE)
        html {
            head {
                meta charset="UTF-8";
                link rel="stylesheet" type="text/css" href=(StaticAsset::url("style.css"));
                meta name="viewport" content="width=device-width, initial-scale=1.0";
                title { (title) };
            }
            body {
                nav { (navigation_links) }
                main { (content) }
                script src=(StaticAsset::url("client.js"));
            }
        }
    };
    template.into_string()
}

fn parse_index(s: &str) -> Result<Index, web::Error> {
    s.parse().map_err(|_| web::Error::BadRequest)
}
fn parse_optional_index(s: Option<&str>) -> Result<Option<Index>, web::Error> {
    s.map(parse_index).transpose()
}
fn parse_required_index(s: Option<&str>) -> Result<Index, web::Error> {
    s.map_or(Err(web::Error::BadRequest), parse_index)
}

///////////////////////////////////////////////////////////////////////////////
/// Wiki static files.
/// Do not depend on page generation.
/// Static files are stored externally for development (easier to edit).
/// Their content is statically embedded to remove file dependencies at runtime.
struct StaticAsset {
    path: String,
}
impl StaticAsset {
    fn url(path: &str) -> String {
        format!("/static/{}", path)
    }
}
impl EndPoint for StaticAsset {
    type State = State;
    fn from_request(r: Request<Body>) -> Result<FromRequestOk<Self>, FromRequestError> {
        match (r.method(), remove_prefix(r.uri().path(), "/static/")) {
            (&Method::GET, Some(path)) => Ok(FromRequestOk::Value(StaticAsset {
                path: path.to_string(),
            })),
            _ => Err(FromRequestError::NoMatch(r)),
        }
    }
    fn generate_response(self, _state: &State) -> Response<Body> {
        match ASSETS.iter().find(|asset| asset.path == self.path) {
            Some(asset) => Response::builder()
                .status(StatusCode::OK)
                .header(hyper::header::CONTENT_TYPE, asset.mime)
                .header(hyper::header::CACHE_CONTROL, "public, max-age=3600") // Allow cache for 1h
                .body(Body::from(asset.content))
                .unwrap(),
            None => web::response_empty_404(),
        }
    }
}
struct AssetDefinition<'a> {
    path: &'a str,
    mime: &'a str,
    content: &'a str,
}
const ASSETS: [AssetDefinition; 2] = [
    AssetDefinition {
        path: "style.css",
        mime: "text/css; charset=utf8",
        content: include_str!("assets/style.css"),
    },
    AssetDefinition {
        path: "client.js",
        mime: "application/javascript",
        content: include_str!("assets/client.js"),
    },
];
