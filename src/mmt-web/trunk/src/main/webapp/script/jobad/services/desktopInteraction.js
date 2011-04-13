/**
 * JOBAD - desktopInteraction.js module
 * @author: Catalin David
 * @created: 29/06/2010
 * 
 */
/*
This module is used for the SACHS project
or integrating Hets with Firefox as a graph display mode.

This service provides a context menu item that posts a request
to the backend, which in turn, talks to a daemon that assures 
the communication between the Application (Excel for SACHS, Hets
for Hets) and the Browser (Firefox).
*/
var desktopInteraction = clone(Service);
var hostCallBack = null;
var myID = (function(){
	var qstring = unescape(location.search.substring(1));
	var pairs = qstring.split(/\&/);
	for( var i in pairs){
		var varName = pairs[i].split(/\=/);
		if(varName[0] == 'id')
			return varName[1];
	}
	return -1;	
})();

desktopInteraction.contextMenuEntries = function(target){
	console.log(target);
	if(target.hasAttribute('id') || $(target).parents('[id]').length > 0)
 		return [['Send notification to client', 'notif("false", "right-click")']];
	else
		return null;
}

desktopInteraction.leftClick = function(target){
	console.log(target);
	if(target.hasAttribute('id') || $(target).parents('[id]').length > 0)
        notif("true", 'click', target);
}

function notif(wait, action, obj){
	var object;
	console.log(typeof(obj));
	if(typeof(obj) !== 'undefined')
		object = obj;
	else
		object = focus;
	var mathId = object.hasAttribute('id') ? $(object).attr('id') : $($(object).parents('[id]')[0]).attr('id');

	if(wait == "true")
		populateDialog('_jobad_dialog', "<img src='"+ gContext + "/../../classpath/ajax-loader.gif'></img>", 'Modules', true);

	$.get(hostCallBack, {'id': myID, 'mathId': mathId, 'action': action, 'wait': wait}, function(data){
		console.log(data);
		if(wait == "true"){
			//display the result in a nice popup...
			populateDialog('_jobad_dialog', data, 'Notification', true);
			$('#_jobad_dialog').dialog('open');
		}
	});
}

$('#_jobad_dialog').dialog('destroy');
$('#_jobad_dialog').dialog({modal:true, autoOpen:false});

//Register yourself with the Service class
for (x in loadedModules) {
    if (loadedModules[x][0] == "desktopInteraction") {
        console.log("Successfully registered the Desktop Interaction module");
        loadedModules[x][2] = desktopInteraction;
		hostCallBack = loadedModules[x][3];
        break;
    }
}


$.getScript(gContext + '/dialog.js', function(){
    $.getScript(gContext + '/html.js', function(){
        $.getScript(gContext + '/cookie.js', function(){
            desktopInteraction.init();
        })
    })
});

