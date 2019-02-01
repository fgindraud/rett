var edit_description_overlay = $('#edit_description_overlay');
if (edit_description_overlay) {
	edit_description_overlay.hide();
	$('#edit_description_start').click(function () { edit_description_overlay.show(); });
	$('#edit_description_cancel').click(function () { edit_description_overlay.hide(); });
}

var remove_overlay = $('#remove_overlay');
if (remove_overlay) {
	remove_overlay.hide();
	$('#remove_start').click(function () { remove_overlay.show(); });
	$('#remove_cancel').click(function () { remove_overlay.hide(); });
}
