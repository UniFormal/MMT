/**
 * JOBAD - definitionLookup.js module
 * @author: Catalin David
 * @created: 24 Feb 2010
 * @description: provides definition lookup support for a document
 *
 */
//depends on: parallelMarkup.js, ajax.js

var defLookup = clone(Service);
var populated = false;
// getting the url address for the definition lookup service host
var ServingOMDoc = gHost;
defLookup.contextMenuEntries = function(target){
    var object = target;
	if (checkMathMLEncapsulation('math', target)) {
		var contentElement = getContentElement(object);
		if (contentElement !== undefined && parentOrSelfSymbol(contentElement) !== undefined) {
			return [['Look-up Definition', 'expand()']];
		}
		else {
			return null;
		}
	}
	else return null;
};

/**
 * returns the current object, if it is an OMS, otherwise returns its parent
 * element operator if that is an OMS, otherwise return “undefined”.
 *
 * @param object : the element that needs to be checked
 * @returns : the element, if it is an OMS, otherwise its parent operator if
 * that is an OMS, otherwise undefined
 */
var parentOrSelfSymbol = function(object){
    var result;
    if (getTagName(object) == "OMS") {
        return object;
    }
    else {
        var parentOperator = object.parentNode.firstElementChild;
        if (getTagName(parentOperator) == "OMS") {
            return parentOperator;
        }
    }
    return result;
}

/**
 * expand - function that performs the definition lookup service. It extracts the neccessary
 * data from within the html document and establishes the connection to the service host via
 * the ajax module helping functions.
 *
 * @throws an alert in case the function is invoked and the service is unavailable
 */
function expand(){
	populateDialog('_jobad_dialog', "<img src='"+ gContext + "/../../classpath/ajax-loader.gif'></img>", 'Definition Lookup', false);
    var object = focus;
    
    var contentElement = getContentElement(object);
    var parentOrSelfOMS = parentOrSelfSymbol(contentElement);
    if (parentOrSelfOMS) {
        var paramValue = "action=expandDefinition&file="
        var fileID = $("html").attr("id");
        paramValue = paramValue.concat(fileID);
        paramValue = paramValue + "&cd=";
        var cd = $(parentOrSelfOMS).attr("cd");
        paramValue = paramValue.concat(cd);
        paramValue = paramValue + "&symbol=";
        var symbolName = $(parentOrSelfOMS).attr("name");
        paramValue = paramValue.concat(symbolName);
        
       
	    proxyGet(ServingOMDoc+ '?' +paramValue, '', handleResponseDialog);
	} else {
        serviceNotAvailable();
    }
}

/**
 * handleResponseDialog - callback function that handles the response obtained from the host of the
 * definition lookup service. It actually extracts the relevant data from the response and populates
 * the tooltip dialog box.
 *
 * @param parameters : a list of parameters holding the id of the container element where the content
 * of the server response is to be put, and the response itself.
 */
var handleResponseDialog = function(data){    
    var selector = '_jobad_dialog';
    populateDialog(selector, data, 'Definition Lookup Results');
}




//Register yourself with the Service class
for (x in loadedModules) {
    if (loadedModules[x][0] == "definitionLookup") {
        console.log("Successfully registered defLookup");
        loadedModules[x][2] = defLookup;
		ServingOMDoc = loadedModules[x][3];
        break;
    }
}

//Finally, do init

$.getScript(gContext + '/parallelMarkup.js', function(){
    $.getScript(gContext + '/html.js', function(){
        $.getScript(gContext + '/mathml.js', function(){
            $.getScript(gContext + '/dialog.js', function(){
                defLookup.init();
            })
        })
    })
})


$('#_jobad_dialog').dialog({
    autoOpen: false
});
