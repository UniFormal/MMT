var last_MWS_query = "";
        
$(function(){
	var textarea = $('#searchQuery');
	$('#searchQuery').tAutoResize({}).focus();
	$('#searchQuery2').tAutoResize({
		maxHeight   : 400
	});
    
        $('#searchQuery').keypress(function(e){
	    if(e.which == 13){
		e.preventDefault();
		$('#search').click();
	    }
	});

        
 
        expandResults();
    
  	$('#search').bind('click', function(){
  		if( $('#searchQuery').val().length > 0 ){
  			$('#resultHolder').html(
		  		//$(document.createElement('img')).attr({ 'src':'images/ajax.gif', 'class':'loading' })
			);
			$('#resultWrapper').slideDown();
              
			req=new XMLHttpRequest();
			req.open("POST", '/:search?lf', true);
			req.setRequestHeader("Content-type","text/xml");
			req.setRequestHeader("Size","2030");
		    
		        req.setRequestHeader("scope","http://latin.omdoc.org/math?IntArith");
			
		        req.onload = function(e) {
			    var resp = e.target.response.replace(/mws:/g,"");
			    var xmlResp = $.parseXML(resp);
		        
		            $xml = $( xmlResp );
			    
		            var elems = [];
		            elems.push(
				'<table class="output-table" cellspacing="0" cellpadding="0">'
				//    '<tr>'+
				//    '<th>URL</th>'+
				//    '</tr>'
			    );
		            
		            var elem;
                            var uris = [];
		            $xml.find("answ").each(function() {
				var uri = $(this).attr("uri");
				if ($.inArray(uri, uris) == -1) { //ensuring unique results
				    elems.push('<tr><td>' + processURI(uri) + '</td></tr>');
				    uris.push(uri);
				}
			    });
		            elems.push("</table>");
			    
		            $("#resultWrapper").html(elems.join("\n") + '\n');
			}
		    
		        req.send($('#searchQuery').val());       
		        
		} else {
      		    $('#searchQuery').focus();
    		    M( 'Query empty', 'warn' );
		}
	});
	
	$('#search2').bind('click', function(){
		if($('#searchQuery2').val().length > 0 ) {
			$('#resultHolder').html(
				$(document.createElement('img')).attr({ 'src':'images/ajax.gif', 'class':'loading' })
			);
			last_MWS_query = $('#searchQuery2').val();
			mwsQuery( last_MWS_query, 0 );
		} else {
		    $('#searchQuery2').focus();
		    M( 'Query empty', 'warn' );
		}
	});
});



function getDeclURI(uri) {
    var arr = uri.split("?");
    var doc = (arr.length >= 1) ? arr[0] : "";
    var mod = (arr.length >= 2) ? arr[1] : "";
    var sym = (arr.length >= 3) ? arr[2] : "";
    
    var splMod = mod.split("^");
    var splSym = sym.split("/");
    
    var decl = uri
    if (splMod.length == 3) {
	decl = doc + "?" + splMod[0] + "?" + splMod[1].split("!")[1] + "/" + sym 
    } else if (splSym.length > 1) {
	decl = doc + "?" + mod + "?" + splSym[0].split("%3F")[1] + "/" + splSym[1]
    } 
    return decl;
}

function processURI(uri) {
    
    xml = "<div class='result' uri='" + uri + "'><h2>" + getDeclURI(uri) + "</h2></div>";
   
    return xml;
}

function getQueryURI(uri) {
    var arr = uri.split("?");
    var doc = (arr.length >= 1) ? arr[0] : "";
    var mod = (arr.length >= 2) ? arr[1] : "";
    var sym = (arr.length >= 3) ? arr[2] : "";
    var splSym = sym.split("/");
    
    if (splSym.length > 1) {
	var quri = splSym[0].replace(/%2F/g, "/").replace(/%3F/g, "?") + "?" + splSym[1];

	return quri;
    } else {
	return uri;
    }
    
}


function expandResults() {
    $("body").on('click', '.result h2', function (e) {
	var res = $(this).parent();
	var uri = res.attr('uri');
	if (res.hasClass('expanded')) {
	   var node = res;
	   node.html("<h2>" + getDeclURI(uri) + "</h2>");
	} else {
	    var title = res.text();
	    var qurl = "/:mmt?" + getQueryURI(uri) + "?_present_http://cds.omdoc.org/foundations/lf/mathml.omdoc?twelf";
	    req=new XMLHttpRequest();
	    req.open("POST", qurl , true);
	    req.setRequestHeader("Content-type","text/xml");
	    var node = res;
	    req.onload = function(e) {

		node.html("<h2>" + title + "</h2><div class='expres'>" + e.target.response + "</div>" + "<h4> Justification : </h4>" + getJustification(uri));
		node.hide();
		node.fadeIn(1000);
		
		
	    }
	    
	    req.send();
	}
	res.toggleClass('expanded');
	
    });
}

function getJustification(uri) {
    var arr = uri.split("?");
    var doc = (arr.length >= 1) ? arr[0] : "";
    var mod = (arr.length >= 2) ? arr[1] : "";
    var sym = (arr.length >= 3) ? arr[2] : "";
    
    var splMod = mod.split("^");
    var splSym = sym.split("/");
    var just = ""
    if (splMod.length == 3) {
	just = just + "<p> Induced statement found in <u>" + doc + "?" + splMod[0] + "</u></p>";
	just = just + "<p> <u>" + splMod[0] + "</u> is a <u>" + splMod[2].split("!")[1] + "</u> if we interpret over view <u>" + splMod[1].split("!")[1] + "</u></p>";
	just = just + "<p><u>" + splMod[2].split("!")[1] + "</u> contains the statement <u>" + sym + "</u></p>";
	req=new XMLHttpRequest();
	var qurl = "/:mmt?" + splMod[2].split("!").join("?").split("|").join("/") + "?" + sym + "?_present_http://cds.omdoc.org/foundations/lf/mathml.omdoc?twelf";
	req.open("POST", qurl , false);
	req.setRequestHeader("Content-type","text/xml");
	req.send();
	just = just + "<div class='expres'>" + req.response + "</div>"
	
    } else if (splSym.length > 1) {
	just = just + "<p> Induced statement found in <u>" + doc + "?" + mod + "</u></p>";
	just = just + "<p><u> " + mod + "</u> is a <u>" + splSym[0].split("%3F")[1] + "</u> by construction </p>";
	just = just + "<p><u>" + splSym[0].split("%3F")[1] + "</u> contains the statement <u>" + splSym[1] + "</u></p>";
	req=new XMLHttpRequest();
	var qurl = "/:mmt?" + splSym[0].split("%3F").join("?").split("%2F").join("/") + "?" + splSym[1] + "?_present_http://cds.omdoc.org/foundations/lf/mathml.omdoc?twelf";
	req.open("POST", qurl , false);
	req.setRequestHeader("Content-type","text/xml");
	req.send();
	just = just + "<div class='expres'>" + req.response + "</div>"
    } else {
	just = just + "<p> Explicit (declared) statement found in <u>" + doc + "?" + mod + "</u></p>";
    }
    
    return just
}


function unescape(uri) {
    
    var res = uri.replace(/|/g, "/").replace(/!/g, "?");
    
    return(res);
}




