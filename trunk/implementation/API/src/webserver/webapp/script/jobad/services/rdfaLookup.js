/**
 * JOBAD - rdfaLookup.js module
 * @author: Catalin David
 * @created: 15 May 2010
 * @description: provides support for RDFa target lookup
 *
 */
var rdfaLookup = clone(Service);
var rdfstore = null;

rdfaLookup.init = function(){
    rdfstore = $(document).rdf();
}

function rdfaNav(url){
    window.location.href = url;
	var hash = window.location.hash;
    if (hash != "") {
        //handle highlighting
        var t = document.getElementById(window.location.hash.split('#')[1]);
        if (t) {
        	handled_light(t);
        }
    }
}

rdfaLookup.contextMenuEntries = function(target){
    var obj = target;
    var b = obj;
    var res = [];
    while (b != document.body && b != null) {
        if (b.hasAttribute('about')) {
            res = rdfstore.where('<' + b.getAttribute('about') + '> ?w1 ?w2').sources();
            break;
        }
        b = b.parentNode;
    }
    var cm2 = [];
    for (var i = 0; i < res.length; i++) {
        if (res[i][0].object.value._string) {
            if (res[i][0].object.value._string.indexOf('#') != -1) 
                var name = res[i][0].object.value._string.split('#')[1];
            else {
                var name = res[i][0].object.value._string;
            }
        }
        var link = res[i][0].object.value._string;
        cm2[i] = [name, "rdfaNav('" + link + "')", null];
    }
    var cm1 = [];
    for (var i = 0; i < res.length; i++) {
        if (res[i][0].property.value._string) {
			if (res[i][0].property.value._string.indexOf('#') != -1) 
				var name = res[i][0].property.value._string.split('#')[1];
			else 
				var name = res[i][0].property.value._string;
		}
        var link = res[i][0].property.value._string;
        var added = false;
        for (var j = 0; j < cm1.length; j++) {
            if (cm1[j][1] == "rdfaNav('" + link + "')") {
                cm1[j][2][cm1[j][2].length] = cm2[i];
                added = true;
            }
        }
        if (!added) {
            cm1[cm1.length] = [name, "rdfaNav('" + link + "')", [cm2[i]]];
        }
    }
    if (cm1.length != 0) 
        return [['Look up RDFa', '', cm1]];
    else 
        return null;
}

//Register yourself with the Service class
for (x in loadedModules) {
    if (loadedModules[x][0] == "rdfaLookup") {
        console.log("Successfully registered rdfaLookup");
        loadedModules[x][2] = rdfaLookup;
        break;
    }
}

//Finally, do init
$.getScript(gContext + '/dialog.js', function(){
    $.getScript(gContext + '/html.js', function(){
        $.getScript(gContext + '/rdfa/jquery.rdfquery.core-1.0.js', function(){
            $.getScript(gContext + '/rdfa/jquery.rdfquery.core-1.0.js', function(){
                $.getScript(gContext + '/rdfa/jquery.rdfquery.rules-1.0.js', function(){
                    rdfaLookup.init();
                    console.log("rdfaLookup init done");
                })
            })
        })
    })
});
