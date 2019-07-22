

fn object_name<'a>(object: ObjectRef<'a>) -> String {
    match *object {
        Object::Atom(ref a) => a.to_string(),
        Object::Link(ref l) => format!("{} → {}", l.from, l.to),
        Object::Abstract => {
            let has_name_parent = |l: ObjectRef<'a>| match *l.as_link().unwrap().from {
                Object::Atom(ref a) => match *a {
                    Atom::Text(ref t) if t == "name" => true,
                    _ => false,
                },
                _ => false,
            };
            let is_naming_link = |l: &ObjectRef<'a>| l.in_links().into_iter().any(has_name_parent);
            if let Some(naming_link) = object.in_links().into_iter().find(is_naming_link) {
                object_name(naming_link.as_link().unwrap().from)
            } else {
                format!("Object {}", object.index())
            }
        }
    }
}

fn object_link<'a>(object: ObjectRef<'a>) -> Box<Render> {
    let url = object_url(object.index());
    let class = object_html_class(object);
    let name = object_name(object);
    box_html! {
        a(href=&url, class=&class) : &name;
    }
}

fn page_for_object<'a>(object: ObjectRef<'a>) -> Response {
    let graph = object.graph();
    let name = object_name(object);

    let nav = html! {
        a(href=format!("/create/link/to/{}", object.index()), class="link") : "Link to";
        a(href=format!("/create/link/from/{}", object.index()), class="link") : "Link from";
        a(id="edit_description_start") : "Description";
        @ if object.in_links().is_empty() && object.out_links().is_empty() {
            a(id="remove_start") : "Remove";
        }
    };
    let description = html! {
        @ for paragraph in object.description().split("\n\n") {
            p(class="description") : paragraph;
        }
    };
    let overlays = html! {
        div(id="edit_description_overlay", class="overlay") {
            form(class="vbox", method="post", action="/edit/description") {
                input(type="hidden", name="index", value=object.index());
                textarea(id="edit_description_content", name="content", autofocus, placeholder="Description...") : object.description();
                div(class="hbox") {
                    a(class="button", id="edit_description_cancel") : "Cancel";
                    input(class="button", type="submit", value="Edit");
                }
            }
        }
        div(id="remove_overlay", class="overlay") {
            form(class="vbox", method="post", action="/remove") {
                input(type="hidden", name="index", value=object.index());
                h1 : format!("Remove {} ?", name);
                div(class="hbox") {
                    a(class="button", id="remove_cancel") : "Cancel";
                    input(class="button", type="submit", value="Remove");
                }
            }
        }
    };
    let content = html! {
        h1(class=object_html_class(object)) : &name;
        : description;
        @ if let Object::Link(ref l) = *object {
            ul {
                li {
                    : "From ";
                    : object_link(graph.object(l.from));
                }
                li {
                    : "To ";
                    : object_link(graph.object(l.to));
                }
            }
        }
        : overlays;
    };
    wiki_page(&name, nav, content)
}

fn post_remove(request: &Request, graph: &mut Graph) -> Response {
    let form_data = try_or_400!(post_input!(request, { index: Index }));
    let _ = try_or_400!(graph.remove_object(form_data.index));
    Response::redirect_303("/")
}