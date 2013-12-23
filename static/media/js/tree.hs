/* Taken from http://jsfiddle.net/mehmetatas/fXzHS/2/  */
/* http://stackoverflow.com/questions/11167628/trees-in-twitter-bootstrap */
/* By Mehmet Atas */


$(function () {
    $('.tree li').hide();
    $('.tree li:first').show();
    $('.tree li').on('click', function (e) {
        var children = $(this).find('> ul > li');
        if (children.is(":visible")) children.hide('fast');
        else children.show('fast');
        e.stopPropagation();
    });
});