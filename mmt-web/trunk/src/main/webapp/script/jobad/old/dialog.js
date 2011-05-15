/**
 * addElement - function that adds an html child to a specific element, specifying the element
 * type, id, title attribute and the inner html content.
 *
 * @param elementID : string value of the ID of the "parent" element
 * @param newElement : string value of the child element tag name
 * @param idAttrVal : string value for the id attribute of the child element
 * @param titelAttrVal : string value for the title attibute of the child element
 * @param onMouseHover : string value for the action that is to be performed when cursor is to hover
 * over the child element
 * @param htmlContent : sting value for the content of the child element to be created
 */
function addElement(elementID, newElement, idAttrVal, titleAttrVal, onMouseHover, htmlContent){
    var parentEl = document.getElementById(elementID);
    var newEl = document.createElement(newElement);
    newEl.setAttribute('id', idAttrVal);
    if (onMouseHover !== '') {
        newEl.setAttribute('onmouseover', onMouseHover);
    }
    else {
        newEl.setAttribute('title', titleAttrVal);
    }
    newEl.innerHTML = htmlContent;
    parentEl.appendChild(newEl);
}

/**
 * removeElement - function that removes an html element
 *
 * @param elementID : string value of the ID of the element to be removed
 */
function removeElement(elementID){
    var elementEl = document.getElementById(elementID);
	if(elementEl)
    	elementEl.parentNode.removeChild(elementEl);
}

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

/**
 * clearDialog - function that removes the content of the dialog box
 */
function clearDialog(){
    removeElement('_jobad_content');
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