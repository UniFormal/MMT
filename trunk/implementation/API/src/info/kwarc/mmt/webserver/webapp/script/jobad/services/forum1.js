/**
 * 	JOBAD - forum1.js module
 * 	@author: Catalin David
 * 	@created: 13/06/2010
 * 	@description: provides forum data lookup
 */
var forum1 = clone(Service);
var scriptURL = gHost;

function avail(data){
    if (data.split(' ')[0] == 0) 
        return 'unavailable';
    if (data.split(' ')[0] == data.split(' ')[1]) 
        return 'closed';
    return 'unsolved';
}

forum1.init = function(){
    //find all the math elements that have an ID
    //for each element, ask from the server for a string (total, closed)
    //(0,0) - gray
    //(x,0) - red
    //(x,x) - green	
	//var it = $(document.body).simplebar().addItem('forumItemLI', '');
    var res = document.evaluate('//m:math[@id]', document, nsResolver, XPathResult.ORDERED_NODE_SNAPSHOT_TYPE, null);
    for (var i = 0; i < res.snapshotLength; i++) {
        $.ajax({
            type: 'POST',
            url: scriptURL + 'forumSQ.php',
            data: {
                'fragmentURI': location.href.split('#')[0] + '#' + $(res.snapshotItem(i)).attr('id')
            },
            success: function(data){
                var resItem = res.snapshotItem(i);
				$(resItem).data("avail", avail(data));
				
//                $(resItem).simpletip({
//                    content: ' ',// + data.split(' ')[0] + ' ',
//                    fixed: true,
//                    position: 'right',
//                    activeClass: avail(data)
//                });
            },
            async: false
        });
    }
}

function forumPostNewMessage(mathID){
    //subject, text, categ, fragmentURI
    $.post(scriptURL + 'forumPost.php', {
        'subject': document.getElementById('forumPostTitle').value,
        'text': "About: " + document.getElementById('forumFragmentURI').value + "\n\n" + document.getElementById('forumReportArea').value + "\n\n Posted by " + document.getElementById('forumReporterName').value,
        'categ': document.getElementById('forumSelect').value,
        'fragmentURI': document.getElementById('forumFragmentURI').value
    }, function(data){
        if (data != "") {
            alert('Oups! Something went wrong while processing your request. Please try again!');
        }
        else {
            $('#forumAddFormDiv').remove();
            $('#_jobad_dialog').dialog('close');
            console.log(mathID);
            console.log($('#' + mathID));
            //and update the value
            $.ajax({
                type: 'POST',
                url: scriptURL + 'forumSQ.php',
                data: {
                    'fragmentURI': location.href.split('#')[0] + '#' + $('#' + mathID).attr('id')
                },
                success: function(data){
                    $('#' + mathID).simpletip().update(' ' + data.split(' ')[0] + ' ');
					$('#' + mathID).simpletip().updateActiveClass(avail(data));
                },
                async: false
            });
        }
    });
}

function showForumDetails(){
    var obj = focus;
    populateDialog('_jobad_dialog', "<img src='" + gContext + "/../../../classpath/ajax-loader.gif'></img>", 'Forum Entries');
    $.post(scriptURL + 'forumQ.php', {
        'fragmentURI': location.href.split('#')[0] + '#' + $(obj).closest('math').attr('id')
    }, function(data){
        var button = "<br/><button id='buttonToggleAddForm' type='button'>Add a new forum entry</button><br/>";
		
        var div = '<div id="forumAddFormDiv">' +
        '<label>Fragment URI: </label>' +
        '<input id="forumFragmentURI" type="text" disabled="disabled" value="' + location.href.split('#')[0] + "#" + $(obj).closest('math').attr('id') + '"/>' +
		'<br/>'+
		'<label>Your Name: </label>' +
        '<input type="text" id="forumReporterName" value="" />'+
        '<br/>' +
        '<label>Category: </label>' +     //retrieve categories -- JavaScript magic :O :O
        (function(data){
            var data1 = data.split(','), optionString = "";
            for (var i = 0; i < data1.length; i++) {
                optionString += "<option value='" + data1[i].split(' ')[0] + "'>" + data1[i].split(' ')[1] + "</option>"
            }
            return '<select id="forumSelect">' + optionString + '</select>';
        })($.ajax({
            type: 'GET',
            url: scriptURL + 'forumCateg.php',
            async: false
        }).responseText)+
        '<br/>' +
        '<label>Post title: </label>' +
        '<input type="text" id="forumPostTitle" value=""/>' +
        '<br/>' +
        "<label>Message text: </label>" +
        "<textarea id='forumReportArea' cols='25' rows='5' />" +
		'<br/>'+
		'<button onclick="forumPostNewMessage(\''+$(obj).closest('math').attr('id')+'\')">POST!</button>'+
        '</div>';
        
        populateDialog('_jobad_dialog', data + button + div, 'Forum Entries');
        
        $('#forumAddFormDiv').hide();
        $('#buttonToggleAddForm').toggle(function(){
            $('#forumAddFormDiv').show()
        }, function(){
            $('#forumAddFormDiv').hide()
        });
        //also add button for adding a post to the forum
    });
}

forum1.contextMenuEntries = function(target){
    if (checkMathMLEncapsulation('math', target) && $(target).closest('math').attr('id')) {
        return [['Forum entries details', 'showForumDetails()']];
    }
    return null;
}

forum1.hoverText = function(target){
	if (checkMathMLEncapsulation) {
		if (checkMathMLEncapsulation('math', target) && $(target).closest('math').attr('id')) {
			console.log("math");
			var av = $(target).closest('math').data("avail");
			console.log($(target).closest('math').data("avail"));
			$(document.body).simplebar().editItem('forumItemLI', "<div class='" + av + "' style='display: block; z-index: 10000;'></div>");
		}
		else {
			$(document.body).simplebar().removeItem('forumItemLI');
		}
	}
		
	return null;
}


$.getScript(gContext + '/mathml.js', function(){
    $.getScript(gContext + '/dialog.js', function(){
        $.getScript(gContext + '/html.js', function(){
            forum1.init();
            $('#_jobad_dialog').dialog({
                autoOpen: false
            });
        });
    });
});
//Register yourself with the Service class
for (x in loadedModules) {
    if (loadedModules[x][0] == "forum1") {
        console.log("Successfully registered forum1");
        loadedModules[x][2] = forum1;
        scriptURL = loadedModules[x][3];
        break;
    }
}