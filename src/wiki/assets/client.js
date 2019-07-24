function hide(el) { el.style.display = 'none'; }
function show(el) { el.style.display = ''; }

function add_click_listener_by_id(id, fn) { document.getElementById(id).addEventListener('click', fn); }

var edit_description_overlay = document.getElementById('edit_description_overlay');
if (edit_description_overlay) {
	hide(edit_description_overlay);
	add_click_listener_by_id('edit_description_start', function () { show(edit_description_overlay); });
	add_click_listener_by_id('edit_description_cancel', function () { hide(edit_description_overlay); });
}

var remove_overlay = document.getElementById('remove_overlay');
if (remove_overlay) {
	hide(remove_overlay);
	add_click_listener_by_id('remove_start', function () { show(remove_overlay); });
	add_click_listener_by_id('remove_cancel', function () { hide(remove_overlay); });
}
