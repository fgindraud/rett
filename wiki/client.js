/******************************************************************************
 * Popup for edit description overlay.
 */
var edit_description_overlay = $('#edit_description_overlay');
if (edit_description_overlay) {
	edit_description_overlay.hide();
	$('#edit_description_start').click(function () {
		edit_description_overlay.show();
	});
	$('#edit_description_cancel').click(function () {
		edit_description_overlay.hide();
	});
}

/*
$('form').submit (function (event) {
	var message = {
		nickname: input_nickname.val(),
		content: input_content.val()
	};
	input_content.val('');

	$.ajax({
		type: "POST",
		data: message,
		error: function (xhr, status, error) {
			error_message("Failed to send message. Server may be down.");
		}
	});
	event.preventDefault();
});*/
