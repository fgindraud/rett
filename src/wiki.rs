use hyper::rt::{Future, Stream};
use hyper::service::service_fn;
use hyper::{Body, Method, Request, Response, Server, StatusCode};
use maud::{html, Markup, PreEscaped};
use signal_hook::{self, iterator::Signals};
use tokio::runtime::current_thread;

use std::cell::RefCell;
use std::path::Path;
use std::rc::Rc;

use relations::{Atom, Database, Element, ElementRef, Index, Ref, Relation};
use web::{self, remove_prefix, EndPoint, FromRequestError, FromRequestOk};

/******************************************************************************
 * Wiki runtime system.
 * Based on hyper/tokio, but uses the single threaded tokio runtime.
 */

/// Wiki web interface state.
struct State {
    database: RefCell<Database>,
}

/// Entry point, run the wiki server.
pub fn run(addr: &str, database_file: &Path) {
    let addr = addr.parse().expect("Address::parse");

    let database = super::read_database_from_file(database_file);
    let state = Rc::new(State {
        database: RefCell::new(database),
    });

    let create_service = || {
        let state = state.clone();
        service_fn(move |request| {
            // Move cloned rc ref in this scope.
            let handlers = [
                web::end_point_handler::<Homepage>,
                web::end_point_handler::<ListAllElements>,
                web::end_point_handler::<DisplayElement>,
                web::end_point_handler::<CreateAtom>,
                web::end_point_handler::<CreateAbstract>,
                web::end_point_handler::<CreateRelation>,
                web::end_point_handler::<StaticAsset>,
            ];
            web::handle_request(request, state.clone(), handlers.iter())
        })
    };
    let server = Server::bind(&addr)
        .executor(current_thread::TaskExecutor::current())
        .serve(create_service);

    let shutdown_signal = Signals::new(&[signal_hook::SIGTERM, signal_hook::SIGINT])
        .expect("Signal handler setup")
        .into_async() // Stream of signals
        .expect("Signal handler to async")
        .into_future(); // Only look at first signal

    let wiki = server.with_graceful_shutdown(shutdown_signal.map(|_| ()));
    current_thread::block_on_all(wiki).expect("Runtime error");
    super::write_database_to_file(database_file, &state.database.borrow());
    eprintln!("Database saved to {}", database_file.display());
}

/******************************************************************************
 * Wiki page definitions.
 */

/* Link creation:
 * buttons to start creating a link from/to a normal display page.
 * cancel + build button if all requirements are filled
 *
 * Removal TODO
 */

// TODO copy for convenience with url generation. should be reworked one day
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
        match state.database.borrow().element(self.index) {
            Ok(element) => {
                web::response_html(display_element_page_content(element, &self.edit_state))
            }
            Err(_) => web::response_empty_404(),
        }
    }
}
fn display_element_page_content(element: Ref<Element>, edit_state: &EditState) -> String {
    let name = html! { (element_name(element)) };
    let content = html! {
        h1 class=(css_class_name(element)) { (name) }
        ul {
            @ for relation in element.subject_of().iter() {
                li { (relation_link(relation, edit_state)) }
            }
            @ for relation in element.descriptor_of().iter() {
                li { (relation_link(relation, edit_state)) }
            }
            @ for relation in element.complement_of().iter() {
                li { (relation_link(relation, edit_state)) }
            }
        }
    };
    let nav = navigation_links(edit_state, Some(element.index()));
    compose_wiki_page(name, content, nav)
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
        let database = &state.database.borrow();
        let content = html! {
            h1 { (lang::HOMEPAGE) }
            @ if let Some(wiki_homepage) = database.get_text_atom("_wiki_homepage") {
                ul {
                    @ for tagged in wiki_homepage.descriptor_of().iter().map(|tag_relation| tag_relation.subject()) {
                        li {
                            a href=(DisplayElement::url(tagged.index(), &self.edit_state)) class=(css_class_name(tagged)) {
                                (tagged.index()) " " (element_name(tagged))
                            }
                        }
                    }
                }
            }
            form method="post" action=(CreateAtom::url(&self.edit_state)) class="hbox" {
                label for="wiki_homepage" { (lang::HOMEPAGE_HELP) }
                button id="wiki_homepage" { "_wiki_homepage" }
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
        let database = &state.database.borrow();
        let content = html! {
            h1 { (lang::ALL_ELEMENTS_TITLE) }
            @ for element in database.iter() {
                p {
                    a href=(DisplayElement::url(element.index(), &self.edit_state)) class=(css_class_name(element)) {
                        (element.index()) " " (element_name(element))
                    }
                }
            }
        };
        let nav = navigation_links(&self.edit_state, None);
        let page = compose_wiki_page(lang::ALL_ELEMENTS_TITLE, content, nav);
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
                    h1 class="atom" { (lang::CREATE_ATOM_TITLE) }
                    form method="post" action=(CreateAtom::url(&edit_state)) class="vbox" {
                        input type="text" name="text" required? placeholder=(lang::ATOM_TEXT);
                        div class="hbox" {
                            button formmethod="get" { (lang::PREVIEW_BUTTON) }
                            button { (lang::COMMIT_BUTTON) }
                        }
                    }
                };
                let nav = navigation_links(&edit_state, None);
                let page = compose_wiki_page(lang::CREATE_ATOM_TITLE, content, nav);
                web::response_html(page)
            }
            CreateAtom::Post { text, edit_state } => {
                let index = state.database.borrow_mut().insert_atom(Atom::from(text));
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
                    h1 class="abstract" { (lang::CREATE_ABSTRACT_TITLE) }
                    form method="post" action=(CreateAbstract::url(&edit_state)) class="vbox" {
                        input type="text" name="name" placeholder=(lang::CREATE_ABSTRACT_NAME_PLACEHOLDER);
                        div class="hbox" {
                            button name="preview" formmethod="get" { (lang::PREVIEW_BUTTON) }
                            button { (lang::COMMIT_BUTTON) }
                        }
                    }
                };
                let nav = navigation_links(&edit_state, None);
                let page = compose_wiki_page(lang::CREATE_ABSTRACT_TITLE, content, nav);
                web::response_html(page)
            }
            CreateAbstract::Post { name, edit_state } => {
                let database = &mut state.database.borrow_mut();
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
                let state = &state.database.borrow();
                let enable_form = edit_state.subject.is_some() && edit_state.descriptor.is_some();
                let field_preview = |name: PreEscaped<&str>,
                                     index: Option<Index>,
                                     allow_missing: bool|
                 -> Markup {
                    html! {
                        tr {
                            td { (name) }
                            @match index {
                                None => @match allow_missing {
                                    true => { td; },
                                    false => { td class="error" { (lang::CREATE_RELATION_MISSING) } },
                                },
                                Some(index) => @match state.element(index) {
                                    Ok(element) => { td { (element_link(element, &edit_state)) } },
                                    Err(_) => { td class="error" { (lang::INVALID_ELEMENT_INDEX) ": " (index) } }
                                }
                            }
                        }
                    }
                };
                let content = html! {
                    h1 class="relation" { (lang::CREATE_RELATION_TITLE) }
                    form method="post" action=(CreateRelation::url(&edit_state)) class="vbox" {
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
                let insertion = state.database.borrow_mut().insert_relation(relation);
                web::response_redirection(&match insertion {
                    Ok(index) => DisplayElement::url(index, &EditState::default()),
                    Err(_) => CreateRelation::url(&edit_state), // Allow retrying
                })
            }
        }
    }
}

//FIXME text use lang
fn relation_link(relation: Ref<Relation>, edit_state: &EditState) -> Markup {
    html! {
        a href=(DisplayElement::url(relation.index(), edit_state)) class="relation" {
            "Element " (relation.index())
        }
    }
}
fn element_link(element: Ref<Element>, edit_state: &EditState) -> Markup {
    html! {
        a href=(DisplayElement::url(element.index(), edit_state)) class=(css_class_name(element)) {
            (element_name(element))
        }
    }
}
fn element_name(element: Ref<Element>) -> String {
    match element.cases() {
        ElementRef::Atom(r) => match r.value() {
            Atom::Text(s) => s.clone(),
        },
        ElementRef::Abstract(r) => {
            if let Some(is_named_atom_index) =
                element.database().index_of_text_atom(lang::NAMED_ATOM)
            {
                let name = r.subject_of().iter().find_map(|r| {
                    if r.descriptor().index() == is_named_atom_index {
                        r.complement()
                    } else {
                        None
                    }
                });
                if let Some(name) = name {
                    return element_name(name);
                }
            }
            format!("Abstrait_{}", element.index())
        }
        ElementRef::Relation(r) => {
            // TODO naming of (s,d,c) without relation recursion
            let r = r.value();
            match r.complement {
                Some(c) => format!("Relation: {} {} {}", r.subject, r.descriptor, c),
                None => format!("Relation {} {}", r.subject, r.descriptor),
            }
        }
    }
}
fn css_class_name(element: Ref<Element>) -> &'static str {
    match element.value() {
        Element::Abstract => "abstract",
        Element::Atom(_) => "atom",
        Element::Relation(_) => "relation",
    }
}

///////////////////////////////////////////////////////////////////////////////
/// Language specific configuration.
/// Contains text constants.
mod lang {
    use maud::PreEscaped;
    type ConstStr = PreEscaped<&'static str>;

    pub const COMMIT_BUTTON: ConstStr = PreEscaped("Valider");
    pub const PREVIEW_BUTTON: ConstStr = PreEscaped("Prévisualiser");
    pub const INVALID_ELEMENT_INDEX: ConstStr = PreEscaped("Index d'élément invalide");

    pub const HOMEPAGE: ConstStr = PreEscaped("Accueil");
    pub const HOMEPAGE_HELP: ConstStr =
        PreEscaped("Pour lister un élément sur cette page, il doit être taggé par _wiki_homepage.");

    pub const ALL_ELEMENTS_NAV: ConstStr = PreEscaped("Éléments");
    pub const ALL_ELEMENTS_TITLE: ConstStr = PreEscaped("Liste des éléments");

    pub const CREATE_ATOM_NAV: ConstStr = PreEscaped("Atome...");
    pub const CREATE_ATOM_TITLE: ConstStr = PreEscaped("Ajouter un atome...");
    pub const ATOM_TEXT: ConstStr = PreEscaped("Texte");

    pub const CREATE_ABSTRACT_NAV: ConstStr = PreEscaped("Abstrait...");
    pub const CREATE_ABSTRACT_TITLE: ConstStr = PreEscaped("Ajouter un élément abstrait...");
    pub const CREATE_ABSTRACT_NAME_PLACEHOLDER: ConstStr = PreEscaped("Nom optionel");

    pub const RELATION_SUBJECT: ConstStr = PreEscaped("Sujet");
    pub const RELATION_DESCRIPTOR: ConstStr = PreEscaped("Verbe");
    pub const RELATION_COMPLEMENT: ConstStr = PreEscaped("Objet");
    pub const CREATE_RELATION_NAV: ConstStr = PreEscaped("Relation...");
    pub const CREATE_RELATION_TITLE: ConstStr = PreEscaped("Ajouter une relation...");
    pub const CREATE_RELATION_MISSING: ConstStr = PreEscaped("Champ manquant !");

    pub const NAMED_ATOM: &'static str = "est nommé";
}

/// Generates sequence of navigation links depending on state.
fn navigation_links(edit_state: &EditState, displayed: Option<Index>) -> Markup {
    html! {
        a href=(Homepage::url(edit_state)) { (lang::HOMEPAGE) }
        a href=(ListAllElements::url(edit_state)) { (lang::ALL_ELEMENTS_NAV) }
        a href=(CreateAtom::url(edit_state)) class="atom" { (lang::CREATE_ATOM_NAV) }
        a href=(CreateAbstract::url(edit_state)) class="abstract" { (lang::CREATE_ABSTRACT_NAV) }
        (selection_nav_link(lang::RELATION_SUBJECT, displayed, edit_state, |e| e.subject, |e,subject| EditState{ subject, ..*e }))
        (selection_nav_link(lang::RELATION_DESCRIPTOR, displayed, edit_state, |e| e.descriptor, |e,descriptor| EditState{ descriptor, ..*e }))
        (selection_nav_link(lang::RELATION_COMPLEMENT, displayed, edit_state, |e| e.complement, |e,complement| EditState{ complement, ..*e }))
        a href=(CreateRelation::url(edit_state)) class="relation" { (lang::CREATE_RELATION_NAV) }
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
            (None, Some(displayed)) => {
                a href=(DisplayElement::url(displayed, &with_field_value(edit_state, Some(displayed)))) class="relation" {
                    "+ " (field_text)
                }
            },
            (Some(selected), Some(displayed)) if selected == displayed => {
                a href=(DisplayElement::url(displayed, &with_field_value(edit_state, None))) class="relation" {
                    "- " (field_text) ": " (displayed)
                }
            }
            (Some(selected), _) => {
                a href=(DisplayElement::url(selected, edit_state)) class="relation" {
                    (field_text) ": " (selected)
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
        content: include_str!("wiki_assets/style.css"),
    },
    AssetDefinition {
        path: "client.js",
        mime: "application/javascript",
        content: include_str!("wiki_assets/client.js"),
    },
];
