var alreadyAdded={};
var lazyParent="#";
var lastGraphTypeUsed;
var lastGraphDataUsed;

function setLocation(curLoc)
{
    try 
	{
        history.pushState(null, null, curLoc);
        return false;
    } 
	catch(e) 
	{
		
	}
    location.hash = '#' + curLoc;
}

function generateCustomSideMenu()
{
	html="";
	for(var i=0;i<GRAPH_TYPES.length;i++)
	{
		html+='<li data-action="'+GRAPH_TYPES[i].id+'" title="'+GRAPH_TYPES[i].tooltip+'">'+GRAPH_TYPES[i].menuText+'</li>';
	}
	html+='<li data-action="close" title="Hides this menu">Hide</li>';
	document.getElementById('side-menu').innerHTML = html;
}	

function setStatusText(text)
{
	statusbar = document.getElementById('statusBar');
	statusBar.innerHTML=text;
}

function createNewGraph(type,graphdata) 
{
	var type=(typeof type =="undefined") ? lastGraphTypeUsed : type;
	var graphdata=(typeof graphdata =="undefined") ? lastGraphDataUsed : graphdata;
	
	lastGraphTypeUsed=type;
	lastGraphDataUsed=graphdata;
	theoryGraph.getGraph( graphDataURL+graphDataURLTypeParameterName+ type + "&" + graphDataURLDataParameterName + graphdata);
	var newURL=location.protocol + '//' + location.host + location.pathname+"?"+graphDataURLTypeParameterName+ type + "&" + graphDataURLDataParameterName + graphdata;
	setLocation(newURL);
}	

function addTreeNodes(data)
{
	var childNodes=data;
	for(var i=0;i<childNodes.length;i++)
	{
		var child=(childNodes[i].hasChildren==true) ? [{"id":"placeholder"}] : undefined;
		var node=
		{ 
			"text" : childNodes[i].menuText, 
			"id" : childNodes[i].id,
			"graphdata": childNodes[i].uri, 
			"typeGraph": childNodes[i].type, 
			"children": child,
			"state" : {"opened": !childNodes[i].hasChildren}
		};
		$('#theory_tree').jstree().create_node(lazyParent, node, 'last',function() {});
	}
}		

function getParameterByName(name, url) 
{
	if (!url) 
	{
		url = window.location.href;
	}
	name = name.replace(/[\[\]]/g, "\\$&");
	var regex = new RegExp("[?&]" + name + "(=([^&#]*)|&|#|$)"),
	results = regex.exec(url);
	if (!results) return null;
	if (!results[2]) return '';
	return decodeURIComponent(results[2].replace(/\+/g, " "));
}

function getStartToEnd(start, theLen) 
{
    return theLen > 0 ? {start: start, end: start + theLen} : {start: start + theLen, end: start};
}


