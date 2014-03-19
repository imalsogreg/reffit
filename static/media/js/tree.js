/* Taken from http://jsfiddle.net/mehmetatas/fXzHS/2/  */
/* http://stackoverflow.com/questions/11167628/trees-in-twitter-bootstrap */
/* By Mehmet Atas */


$(function () {
    $('.tree li').hide();
    $('.tree li:first').show();
    $('.tree .btn-add').on('click', function(e) {
	var fieldTagsContents = $('#fieldTagsHidden').val();
	var fieldTagsView     = $('#fieldTagsView').val();
	var thisTag           = $(this).attr("name");
	var thisTagView       = $(this).attr("shortName");
	if (fieldTagsView != "")
	{
	    thisTag = ", " + thisTag;
	    thisTagView = ", " + thisTagView;
	}
	$('#fieldTagsHidden').val(fieldTagsContents + thisTag);
	$('#fieldTagsView').val(fieldTagsView + thisTagView);  /* TODO just take last name */

	e.stopPropagation();
    });
    $('.tree li').on('click', function (e) {
        var children = $(this).find('> ul > li');
        if (children.is(":visible")) children.hide('fast');
        else children.show('fast');
        e.stopPropagation();
    });
    $('.btn-clear').on('click', function(e) {
	$('#fieldTagsHidden').val("");
	$('#fieldTagsView').val("");
	e.stopPropagation();
	e.preventDefault();
    });

});
