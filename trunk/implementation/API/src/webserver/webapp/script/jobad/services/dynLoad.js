/**
 * JOBAD - load.js module
 * @author: Catalin David
 * @created: 22 Apr 2010
 * @description: provides support for dynamic module loading capabilities
 *
 */
var dynLoad = clone(Service);

var dynLoadURL = gHost;

dynLoad.init = function(){
    var div = document.createElement('div');
    div.setAttribute("id", "load-dialog");
    div.setAttribute("title", "Load Modules");
    document.body.appendChild(div);
    $('#load-dialog').dialog('destroy');
    $('#load-dialog').dialog({
        modal: true,
        autoOpen: false,
        buttons: {
            Ok: function(){
                //TODO: load/remove here
                var jc = document.getElementById('_jobad_content');
                if (jc) {
                    for (var i = 0; i < jc.childNodes.length; i++) {
                        if (jc.childNodes[i].checked) {
                            //if it is checked, load it regardless
                            loadModule(gContext.replace('lib', 'services'), jc.childNodes[i].name);
                        }
                        else {
                            for (x in loadedModules) 
                                if (loadedModules[x][0] == jc.childNodes[i].name) {
                                    removejscssfile(jc.childNodes[i].name, "js");
                                    loadedModules.splice(x, 1);
                                }
                        }
                    }
                    
                    //save the COOKIE
                    var loaded = "";
                    for (x in loadedModules) 
                        loaded += loadedModules[x][0] + ':';
                    createCookie('loadedModules', loaded, 365);
                    $(this).dialog('close');
                    clearDialog();
                }
            }
        }
    });
    
    var span = document.createElement('span');
    var popen = document.createElement('p');
    popen.setAttribute('id', 'load-dialog-open');
    popen.innerHTML = "Click me to configure the loaded modules";
    span.appendChild(popen);
    var br = document.createElement('br');
    span.appendChild(br);
    document.body.insertBefore(span, document.body.firstChild);
    //load the COOKIE	
    var x = readCookie('loadedModules');
    if (x) {
        var arr = x.split(':');
        for (var i = 0; i < arr.length - 1; i++) {
            loadModule('services', arr[i]);
        }
    }
}
dynLoad.leftClick = function(target){
    if (target.hasAttribute('id') && $(target).attr('id') == 'load-dialog-open') {
		clearDialog();
		populateDialog('load-dialog', "<img src='"+ gContext + "/../../../classpath/ajax-loader.gif'></img>", 'Modules', true); 
		$.get(dynLoadURL, function(data){
            var arr = data.split('\n');
            var html = "";
            for (var i = 0; i < arr.length - 1; i++) {
				arr[i] = arr[i].replace('\r', '');
                var newcb = "<input type='checkbox' name ='" + escape(arr[i].toString()) + "' ";
                //if it is already loaded, check it
                for (x in loadedModules) {
                    if (loadedModules[x][0] == arr[i]) {
                        newcb += 'checked="true" ';
                        break;
                    }
                }
                if (arr[i].toString() == "dynLoad") 
                    newcb += "disabled='disabled'";
                newcb += "/>" + arr[i].toString();
                //TODO: add support for services that require the host: definitionLookup, report etc. -- add a textbox for the host
                newcb += "<br/>";
                html += newcb;
            }
            populateDialog('load-dialog', html, 'Modules', true);
        });
        $('#load-dialog').dialog('open');
    }
}


//Register yourself with the Service class
for (x in loadedModules) {
    if (loadedModules[x][0] == "dynLoad") {
        console.log("Successfully registered dynamic module loading");
        loadedModules[x][2] = dynLoad;
        break;
    }
}
//Finally, do init
$.getScript(gContext + '/dialog.js', function(){
    $.getScript(gContext + '/html.js', function(){
        $.getScript(gContext + '/cookie.js', function(){
            dynLoad.init();
        })
    })
});

