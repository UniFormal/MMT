var alreadyAdded={};
var lazyParent="#";


function createNewGraph(type,graphdata) 
{
	theoryGraph.getGraph(mmtUrl+":jgraph/json?key=" + type + "&uri=" + graphdata);
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
		$('#theory_tree').jstree().create_node(lazyParent, node, 'last',function() {console.log("Child created");});
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


