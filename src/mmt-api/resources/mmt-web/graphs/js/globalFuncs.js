// Variable holding menu entries, which were already added
// --> Redownloading is not neccessary
var alreadyAdded={};
// Parent to add downloaded menu entries to
var lazyParent="#";
// Last graph type used
var lastGraphTypeUsed;
// Last graph data used
var lastGraphDataUsed;
// Array of history actions (used for undo button)
var historyStates=[];
// Array of undone history actions (used for redo button)
var undoneHistoryStates=[];

var lastActionWasUndoRedo=0;

// Sets the URL to given location (without reloading page)
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



function generateEdgesNodesHideDiv()
{
	var usedEdgeTypes = theoryGraph.getUsedEdgeTypes();
	var html="<strong>Hide/Show Edges</strong><br>";
	for(var i=0;i<usedEdgeTypes.length;i++)
	{
		var alias=(typeof ARROW_STYLES[usedEdgeTypes[i]] === "undefined" ? ARROW_STYLES[usedEdgeTypes[i].replace("graph","")].alias : ARROW_STYLES[usedEdgeTypes[i]].alias);
		html+="<img onClick='selectEdgesByType(\""+usedEdgeTypes[i]+"\")' src='img/select.png' width='14' style='width:14px;' title='Select all "+alias+"'> </img>";
		html+='<input type="checkbox" '+(usedEdgeTypes[i]=="meta" || usedEdgeTypes[i]=="graphmeta" ? "" : "checked")+' id="edgesCheckbox_'+i+'" value="'+usedEdgeTypes[i]+'" onChange="hideEdges(this.value, !this.checked)"><label for="edgesCheckbox_'+i+'">Show '+alias+'</label>';
		html+="<br>";
	}
	
	var usedNodeTypes = theoryGraph.getUsedNodeTypes();
	html+="<br><strong>Hide/Show Nodes</strong><br>";
	for(var i=0;i<usedNodeTypes.length;i++)
	{
		var alias=(typeof NODE_STYLES[usedNodeTypes[i]] === "undefined" ? NODE_STYLES[usedNodeTypes[i].replace("graph","")].alias : NODE_STYLES[usedNodeTypes[i]].alias);
		html+="<img onClick='selectNodesByType(\""+usedNodeTypes[i]+"\")' src='img/select.png' width='14' style='width:14px;' title='Select all "+alias+"'> </img>";
		html+='<input type="checkbox" '+"checked"+' id="nodesCheckbox_'+i+'" value="'+usedNodeTypes[i]+'" onChange="hideNodes(this.value, !this.checked)"><label for="nodesCheckbox_'+i+'">Show '+alias+'</label>';
		if(i!=usedNodeTypes.length-1)
		{
			html+="<br>";
		}
	}
	
	document.getElementById("edgesShowHideDiv").innerHTML=html;
}

// Executed after first drawing of graph finished
function updateNetworkOnFirstCall()
{
	theoryGraph.colorizeNodesByName(getParameterByName(graphDataURLHighlightParameterNameTGView),highlightColorByURI);
	generateEdgesNodesHideDiv();
	theoryGraph.hideEdges("graphmeta",true);
}

// Creates right-click menu for MMT menu (left side)
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

// Sets status text
function setStatusText(text)
{
	statusbar = document.getElementById('statusBar');
	statusBar.innerHTML=text;
}

// Creates a new graph using type and graphdata parameter (if empty lastGraphTypeUsed and lastGraphDataUsed will be used)
function createNewGraph(type,graphdata, hightlightNodes) 
{
	var type=(typeof type =="undefined") ? lastGraphTypeUsed : type;
	var graphdata=(typeof graphdata =="undefined") ? lastGraphDataUsed : graphdata;
	
	lastGraphTypeUsed=type;
	lastGraphDataUsed=graphdata;
	theoryGraph.onConstructionDone=updateNetworkOnFirstCall;
	theoryGraph.getGraph( graphDataURL+graphDataURLTypeParameterName+"="+ type + "&" + graphDataURLDataParameterName+"=" + graphdata);
	var newURL=location.protocol + '//' + location.host + location.pathname+"?"+graphDataURLTypeParameterNameTGView+"="+ type + "&" + graphDataURLDataParameterNameTGView+"=" + graphdata;
	
	if(typeof hightlightNodes != "undefined")
	{
		newURL+="&"+graphDataURLHighlightParameterNameTGView+"="+hightlightNodes;
	}
	
	setLocation(newURL);
}	

// Adds (child-)nodes to the MMT menu
function addTreeNodes(data)
{
	var childNodes=data;
	for(var i=0;i<childNodes.length;i++)
	{
		var child=(childNodes[i].hasChildren==true) ? [{"id":"placeholder"}] : undefined;
		var node=
		{ 
			"text" : childNodes[i].menuText, 
			"id": "js_tree_node_"+currentMenuNodeId,
			"serverId" : childNodes[i].id,
			"graphdata": childNodes[i].uri, 
			"typeGraph": childNodes[i].type, 
			"children": child,
			"state" : {"opened": !childNodes[i].hasChildren}
		};
		$('#theory_tree').jstree().create_node(lazyParent, node, 'last',function() {});
		currentMenuNodeId++;
	}
}		

// Extracts parameter from url by name 
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

function addToStateHistory(func, parameterArray)
{
	historyStates.push({"func":func, "param": parameterArray});
	if(lastActionWasUndoRedo==0)
	{
		undoneHistoryStates=[];
	}
	lastActionWasUndoRedo=0;
}


// Undos the last action
function undoLastAction()
{
	if(historyStates.length==0)
		return;
	
	lastActionWasUndoRedo=1;
	
	var lastState = historyStates.pop();
	undoneHistoryStates.push(lastState);
	
	if(lastState.func=="cluster")
	{
		theoryGraph.openCluster(lastState.param.clusterId);
	}
	else if(lastState.func=="uncluster")
	{
		theoryGraph.cluster(lastState.param.nodes,lastState.param.name,lastState.param.clusterId);
	}
	else if(lastState.func=="select")
	{
		theoryGraph.selectNodes([]);
	}
	else if(lastState.func=="unselect")
	{
		theoryGraph.selectNodes(lastState.param.nodes);
	}
	
	historyStates.pop();
	doLastAction();
	lastActionWasUndoRedo=0;
}

// Redos last undone action
function redoLastAction()
{
	if(undoneHistoryStates.length==0)
		return;
	
	lastActionWasUndoRedo=1;
	
	var lastState = undoneHistoryStates.pop();
	
	if(lastState.func=="cluster")
	{
		theoryGraph.cluster(lastState.param.nodes,lastState.param.name,lastState.param.clusterId);
	}
	else if(lastState.func=="uncluster")
	{
		theoryGraph.openCluster(lastState.param.clusterId);
	}
	else if(lastState.func=="select")
	{
		theoryGraph.selectNodes(lastState.param.nodes);
	}
	else if(lastState.func=="unselect")
	{
		theoryGraph.selectNodes([]);
	}
	//undoneHistoryStates.pop();
	//doLastAction();
	lastActionWasUndoRedo=0;
}

// Dos the last action
function doLastAction()
{
	if(historyStates.length==0)
		return;

	lastActionWasUndoRedo=1;
	
	var lastState = historyStates[historyStates.length-1];
	if(lastState.func=="unselect")
	{
		theoryGraph.selectNodes([]);
		historyStates.pop();
	}
	else if(lastState.func=="select")
	{
		theoryGraph.selectNodes(lastState.param.nodes);
		historyStates.pop();
	}
	
	lastActionWasUndoRedo=0;
}
