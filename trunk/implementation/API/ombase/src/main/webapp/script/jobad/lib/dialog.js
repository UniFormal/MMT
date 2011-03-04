/**
 * populateDialog - function that fills in a dialog box with the results of the definition lookup service
 *
 * @param where : string denoting the id name of the dialog box that needs to be filled with the result data
 * @param innerHTML : string denoting the result of the user request to be put in the tooltip dialog box
 */
function populateDialog(where, innerHTML, title, pos){
    if (populated) { //populated
        clearDialog();
    }
    var prefix = "#";
    var selector = prefix.concat(where);
    $(selector).dialog('option', 'title', title);
    addElement(where, 'div', '_jobad_content', 'click to get more info', '',
    /* XML-compliant implementations like the one of TNTBase
     * prefix their text/xml response body with an XML declaration,
     * but we have to remove this; otherwise innerHTML, which is
     * used by addElement, will not work. */
    innerHTML.replace(/^<\?xml[^?]+\?>/, ''));    
    populated = true;
    if (!pos) 
        $(selector).dialog('option', 'position', [xpos, ypos]);
    $(selector).dialog('open');
}

/**
 * clearDialog - function that removes the content of the tooltip dialog box
 */
function clearDialog(){
    removeElement('_jobad_content');
    populated = false;
}
