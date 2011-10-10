/**
 * JOBAD - definitionLookup.js module
 * @author: Catalin David
 * @created: 24 Feb 2010
 * @description: provides error reporting support for a document
 *
 */
//depends on: parallelMarkup.js, ajax.js

var report = clone(Service);
var populated = false;

report.contextMenuEntries = function(target){
    return [['Report an issue', 'report_expand()']];
};

function get_XPath(elt){
    var path = '';
    for (; elt && elt.nodeType == 1; elt = elt.parentNode) {
        var idx = $(elt.parentNode).children(elt.tagName).index(elt) + 1;
        idx > 1 ? (idx = '[' + idx + ']') : (idx = '');
        path = '/' + elt.tagName.toLowerCase() + idx + path;
    }
    return path;
}

/**
 * expand - function that performs the definition lookup service. It extracts the neccessary
 * data from within the html document and establishes the connection to the service host via
 * the ajax module helping functions.
 *
 * @throws an alert in case the function is invoked and the service is unavailable
 */
function report_expand(){
    var object = focus;
    if (populated) { //populated
        clearDialog();
    }
    var selector = '_jobad_dialog';
    //add the textbox and the button
    var lbl1 = "<label for='reportArea' >Bug text</label>"
    var txt = "<textarea id='reportArea' cols='25' rows='5' />";
    var br = "<br/><br/>";
    var lbl2 = "<label for='email' >E-mail address</label>"
    var from = "<input type='text' id='email' />";
    var btn = "<button id='reportButton' type='button' onclick=\"doPost('" + get_XPath(object) + "');\" > Submit </button>";
    populateDialog(selector, lbl1 + txt + br + lbl2 + from + br + btn, 'Report a bug');
}

function doPost(xpathStr){//xpath
    var txt = document.getElementById('reportArea');
    var msgStr = txt.value;//message
    txt = document.getElementById('email');
    var from = txt.value;
    var urlStr = location.href; //url of the document
    //send the data
    $.post("http://127.0.0.1/testphp.php", {
        'url': urlStr,
        'xpath': xpathStr,
        'message': msgStr,
        'from': from
    });
    //and close the window
    $('#_jobad_dialog').dialog('close');
}



//Register yourself with the Service class
for (x in loadedModules) {
    if (loadedModules[x][0] == "report") {
        console.log("Successfully registered report");
        loadedModules[x][2] = report;
        break;
    }
}

//Finally, do init
$.getScript(gContext + '/mathml.js', function(){
    $.getScript(gContext + '/ajax.js', function(){
        $.getScript(gContext + '/parallelMarkup.js', function(){
            $.getScript(gContext + '/html.js', function(){
                $.getScript(gContext + '/dialog.js', function(){
                    report.init();
                    $('#_jobad_dialog').dialog({
                        autoOpen: false
                    });
                })
            })
        })
    })
});


