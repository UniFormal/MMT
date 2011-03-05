/**
 * JOBAD - proxy.js module
 * @author: Catalin David
 * @created: 24 Feb 2010
 * @description: provides definition lookup support for a document
 *
 */
//depends on: parallelMarkup.js, ajax.js

var proxy = clone(Service);
var populated = false;

proxy.contextMenuEntries = function(target){
    return [['Wolfram|Alpha', 'proxy_expand()']];
};

/**
 * expand - function that performs the definition lookup service. It extracts the neccessary
 * data from within the html document and establishes the connection to the service host via
 * the ajax module helping functions.
 *
 * @throws an alert in case the function is invoked and the service is unavailable
 */
function proxy_expand(){
    var object = focus;
    var ServingOMDoc = "http://raspberry.eecs.jacobs-university.de:8081/proxy/"; //   http://localhost:8080/proxy/
    var url = "http://api.wolframalpha.com/v1/query?input=" + object.textContent;//+"&format=image";
    var paramValue = "url=" + escape(url) + "&method=GET&service=wa";
    populateDialog('_jobad_dialog', "<img src='"+ gContext + "/../../../classpath/ajax-loader.gif'></img>", 'Wolfram|Alpha Response'); 
    var contentElement = getContentElement(object);
    
    $.post(ServingOMDoc, {
        'data': $('<div>').append($(contentElement).clone()).remove().html().replace('\n', ' ').replace('\t', ' ').replace(/\s+/g, ' ')
    }, function(data){
        populateDialog('_jobad_dialog', data.replace(/pod/g, 'div'), "Wolfram|Alpha Response");
    });
}

/**
 * handleResponseDialog - callback function that handles the response obtained from the host of the
 * definition lookup service. It actually extracts the relevant data from the response and populates
 * the tooltip dialog box.
 *
 * @param parameters : a list of parameters holding the id of the container element where the content
 * of the server response is to be put, and the response itself.
 */
var handleResponseDialog = function(parameters){
    var arg = parameters["containerElementID"];
    var content = parameters["response"];
    populateDialog(arg, content, 'Wolfram|Alpha Results');
}


//Register yourself with the Service class
for (x in loadedModules) {
    if (loadedModules[x][0] == "proxy") {
        console.log("Successfully registered proxy");
        loadedModules[x][2] = proxy;
        break;
    }
}

//Finally, do init
$.get(gContext + "/parallelMarkup.js", function(){
    $.get(gContext + "/ajax.js", function(){
        $.get(gContext + "/mathml.js", function(){
            $.get(gContext + "/html.js", function(){
                $.get(gContext + "/dialog.js", function(){
                    proxy.init();
                    $('#_jobad_dialog').dialog({
                        autoOpen: false
                    });
                })
            })
        })
    })
});

//$.ui.dialog.defaults.bgiframe = true;
