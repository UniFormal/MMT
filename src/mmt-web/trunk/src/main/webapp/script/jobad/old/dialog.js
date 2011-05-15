/**
 * utility functions to display a dialog
 * @author Florian Rabe, based on previous code by Jana Giceva and Catalin David
 */

/**
 * populateDialog - function that fills in a dialog box with the results of the definition lookup service
 *
 * @param where : string denoting the id name of the dialog box that needs to be filled with the result data
 * @param innerHTML : string denoting the result of the user request to be put in the tooltip dialog box
 */
function populateDialog(where, content, title, pos){
    var dia = $('#' + where);
    dia.dialog('option', 'title', title);
    dia[0].replaceChild(content.firstChild, dia[0].firstChild);
    if (!pos) dia.dialog('option', 'position', [xpos, ypos]);
    dia.dialog('open');
}

function init(){
	//create and initialize the dialog
	var div = document.createElement('div');
	div.setAttribute("id", "latin-dialog");
	document.body.appendChild(div);
	var span = document.createElement('span');
	div.appendChild(span)
	$('#latin-dialog').dialog({ autoOpen: false});
}

$(document).ready(init);