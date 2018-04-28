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

function hideEdges(type, hide)
{
	theoryGraph.hideEdges(type, hide);
}

function hideNodes(type, hide)
{
	theoryGraph.hideNodes(type, hide);
}

function generateEdgesNodesHideDiv()
{
	console.log("Generate EdgesNodesDiv");
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
	theoryGraph.hideEdges("meta",true);
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
	console.log(text);
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


// Undoes the last action

function undoLastAction()
{
	console.log(historyStates);
	
	if(historyStates.length==0)
		return;
	
	lastActionWasUndoRedo=1;
	
	var lastState = historyStates.pop();
	console.log(lastState);
	undoneHistoryStates.push(lastState);
	
	if(typeof lastState.func=="undefined")
		return;
	
	var repeatlastAction=true;
	
	if(lastState.func=="cluster")
	{
		theoryGraph.openCluster(lastState.param.clusterId);
		historyStates.pop();
	}
	else if(lastState.func=="uncluster")
	{
		theoryGraph.cluster(lastState.param.nodes,lastState.param.name,lastState.param.clusterId);
		historyStates.pop();
	}
	else if(lastState.func=="select")
	{
		theoryGraph.selectNodes([]);
		historyStates.pop();
	}
	else if(lastState.func=="unselect")
	{
		theoryGraph.selectNodes(lastState.param.nodes);
		historyStates.pop();
	}
	else if(lastState.func=="addNode")
	{
		theoryGraph.deleteNodes(lastState.param.node.id);
		historyStates.pop();
		repeatlastAction=false;
	}
	else if(lastState.func=="editNode")
	{
		theoryGraph.saveNode(lastState.param.oldNode);
		historyStates.pop();
	}
	else if(lastState.func=="deleteNodes")
	{
		for(var i=0;i<lastState.param.nodes.length;i++)
		{
			theoryGraph.addNode(lastState.param.nodes[i]);
			historyStates.pop();
		}
		
		for(var i=0;i<lastState.param.edges.length;i++)
		{
			theoryGraph.addEdge(lastState.param.edges[i]);
			historyStates.pop();
		}
	}
	else if(lastState.func=="addEdge")
	{
		theoryGraph.deleteEdges([lastState.param.edge.id]);
		historyStates.pop();
		repeatlastAction=false;
	}
	else if(lastState.func=="editEdge")
	{
		theoryGraph.saveEdge(lastState.param.oldEdge);
		historyStates.pop();
	}
	else if(lastState.func=="deleteEdges")
	{
		for(var i=0;i<lastState.param.edges.length;i++)
		{
			theoryGraph.addEdge(lastState.param.edges[i]);
			historyStates.pop();
		}
	}	
	else if(lastState.func=="cageNodes")
	{
		theoryGraph.removeNodeRegion(lastState.param.index);
	}
	else if(lastState.func=="hideNodes")
	{
		theoryGraph.hideNodesById(lastState.param.nodesToHide,!lastState.param.hidden);
		historyStates.pop();
	}
	else if(lastState.func=="hideEdges")
	{
		var edgeIds=[];
		
		for(var i=0;i<lastState.param.hideEdges.length;i++)
		{
			edgeIds.push(lastState.param.hideEdges[i].id);
		}
		
		theoryGraph.hideEdgesById(edgeIds,!lastState.param.hidden);
		historyStates.pop();
	}
	else if(lastState.func=="selectEdges")
	{
		theoryGraph.selectEdgesById([]);
		historyStates.pop();
	}
	
	
	if(repeatlastAction==true)
	{
		doLastAction();
	}
	lastActionWasUndoRedo=0;
}

// Redos last undone action

function redoLastAction()
{
	if(undoneHistoryStates.length==0)
		return;
	
	lastActionWasUndoRedo=1;
	
	var lastState = undoneHistoryStates.pop();
	
	if(typeof lastState.func=="undefined")
		return;
			
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
	else if(lastState.func=="addNode")
	{
		theoryGraph.addNode(lastState.param.node);
	}
	else if(lastState.func=="editNode")
	{
		theoryGraph.saveNode(lastState.param.newNode);
	}
	else if(lastState.func=="deleteNodes")
	{
		var toDelete=[];
		for(var i=0;i<lastState.param.edges.length;i++)
		{
			toDelete.push(lastState.param.edges[i].id);
		}
		theoryGraph.deleteEdges(toDelete);
		
		toDelete=[];
		for(var i=0;i<lastState.param.nodes.length;i++)
		{
			toDelete.push(lastState.param.nodes[i].id);
		}
		theoryGraph.deleteNodes(toDelete);
	}
	else if(lastState.func=="addEdge")
	{
		theoryGraph.addEdge(lastState.param.edge);
	}
	else if(lastState.func=="editEdge")
	{
		theoryGraph.saveEdge(lastState.param.newEdge);
	}
	else if(lastState.func=="deleteEdges")
	{
		var toDelete=[];
		for(var i=0;i<lastState.param.edges.length;i++)
		{
			toDelete.push(lastState.param.edges[i].id);
		}
		theoryGraph.deleteEdges(toDelete);
	}
	else if(lastState.func=="cageNodes")
	{
		theoryGraph.cageNodes(lastState.param.nodeIds, lastState.param.color);
	}
	else if(lastState.func=="hideNodes")
	{
		theoryGraph.hideNodesById(lastState.param.nodesToHide,lastState.param.hidden);
	}
	else if(lastState.func=="hideEdges")
	{
		var edgeIds=[];
		
		for(var i=0;i<lastState.param.hideEdges.length;i++)
		{
			edgeIds.push(lastState.param.hideEdges[i].id);
		}
		
		theoryGraph.hideEdgesById(edgeIds,lastState.param.hidden);
	}
	else if(lastState.func=="selectEdges")
	{
		theoryGraph.selectEdgesById(lastState.param.edges);
		historyStates.pop();
	}

	
	//undoneHistoryStates.pop();
	//doLastAction();
	lastActionWasUndoRedo=0;
}


// Does the last action
function doLastAction()
{
	if(historyStates.length==0)
		return;

	lastActionWasUndoRedo=1;
	
	for(var i=historyStates.length-1;i>=0;i--)
	{
		var lastState = historyStates[i];
		if(lastState.func=="unselect")
		{
			theoryGraph.selectNodes([]);
			historyStates.pop();
			break;
		}
		else if(lastState.func=="select")
		{
			theoryGraph.selectNodes(lastState.param.nodes);
			historyStates.pop();
			break;
		}
	}
	
	lastActionWasUndoRedo=0;
}



function addDataNode(data, callback) 
{
	data.id = document.getElementById('node-id').value;
	if(theoryGraph.isUniqueId(data.id)==false)
	{
		alert("The ID entered is already used, please enter an unique ID.");
		return;
	}
	
	data.label = document.getElementById('node-label').value;
	data.url = document.getElementById('node-url').value;
	data.mathml = document.getElementById('node-mathml').value;
	data.style = document.getElementById('node-style').value;
	clearPopUp();
	theoryGraph.addNode(data);
}

function addDataEdge(data, callback) 
{
	var edge={};
	edge.id = document.getElementById('edge-id').value;
	if(theoryGraph.isUniqueEdgeId(edge.id)==false)
	{
		alert("The ID entered is already used, please enter an unique ID.");
		return;
	}
	
	edge.label = document.getElementById('edge-label').value;
	edge.url = document.getElementById('edge-url').value;
	edge.style = document.getElementById('edge-style').value;
	edge.from=data.from;
	edge.to=data.to;
	clearPopUp();
	theoryGraph.addEdge(edge);
}

function editDataEdge(data, callback) 
{
	var edge={};
	edge.id = document.getElementById('edge-id').value;
	edge.label = document.getElementById('edge-label').value;
	edge.url = document.getElementById('edge-url').value;
	edge.style = document.getElementById('edge-style').value;
	edge.from=data.from;
	edge.to=data.to;
	clearPopUp();
	theoryGraph.saveEdge(edge);
	callback(null);
}

function saveDataNode(data, callback) 
{
	var node={};
	node.id = document.getElementById('node-id').value;
	node.label = document.getElementById('node-label').value;
	node.url = document.getElementById('node-url').value;
	node.mathml = document.getElementById('node-mathml').value;
	node.style = document.getElementById('node-style').value;
	clearPopUp();
	theoryGraph.saveNode(node);
	callback(null);
}

function clearPopUp() 
{
	document.getElementById('saveButton').onclick = null;
	document.getElementById('cancelButton').onclick = null;
	document.getElementById('network-popUp').style.display = 'none';
	
	document.getElementById('edge-saveButton').onclick = null;
	document.getElementById('edge-cancelButton').onclick = null;
	document.getElementById('network-edge-popUp').style.display = 'none';
}

function cancelEdit(callback) 
{
	clearPopUp();
	callback(null);
}

function addNodeCallback(data, callback) 
{
	// filling in the popup DOM elements
	document.getElementById('operation').innerHTML = "Add Node";
	document.getElementById('node-id').value = data.id;
	document.getElementById('node-label').value = data.label;
	document.getElementById('node-url').value = "";
	document.getElementById('node-mathml').value = "";
	
	var html="";
	Object.keys(NODE_STYLES).forEach(function (key) 
	{
	   html+='<option value="'+key+'">'+NODE_STYLES[key].alias+'</option>';
	});
	
	document.getElementById('node-style').innerHTML = html;
	document.getElementById('saveButton').onclick = addDataNode.bind(this, data, callback);
	document.getElementById('cancelButton').onclick = clearPopUp.bind();
	document.getElementById('network-popUp').style.display = 'block';
}

function editNodeCallback(data, callback) 
{
	// filling in the popup DOM elements
	document.getElementById('operation').innerHTML = "Edit Node";
	document.getElementById('node-id').value = data.id;
	document.getElementById('node-id').disabled=true;
	document.getElementById('node-label').value = (typeof data.label !="undefined") ? data.label : "";
	document.getElementById('node-url').value = (typeof data.url !="undefined") ? data.url : "";
	document.getElementById('node-mathml').value = (typeof data.mathml !="undefined") ? data.mathml : "";
	
	var html="";
	Object.keys(NODE_STYLES).forEach(function (key) 
	{
	   html+='<option value="'+key+'">'+NODE_STYLES[key].alias+'</option>';
	});
	
	document.getElementById('node-style').innerHTML = html;
	
	if(typeof data.style !="undefined" )
	{
		document.getElementById('node-style').value = data.style;
	}
	
	document.getElementById('saveButton').onclick = saveDataNode.bind(this, data, callback);
	document.getElementById('cancelButton').onclick = cancelEdit.bind(this,callback);
	document.getElementById('network-popUp').style.display = 'block';
}

function addEdgeCallbackHelper(data, callback)
{
	// filling in the popup DOM elements
	document.getElementById('edge-operation').innerHTML = "Add Edge";
	document.getElementById('edge-id').value = 'edge_' + Math.random().toString(36).substr(2, 9);
	document.getElementById('edge-label').value = "";
	document.getElementById('edge-url').value = "";
	
	var html="";
	Object.keys(ARROW_STYLES).forEach(function (key) 
	{
	   html+='<option value="'+key+'">'+ARROW_STYLES[key].alias+'</option>';
	});
	
	document.getElementById('edge-style').innerHTML = html;
	document.getElementById('edge-saveButton').onclick = addDataEdge.bind(this, data, callback);
	document.getElementById('edge-cancelButton').onclick = clearPopUp.bind();
	document.getElementById('network-edge-popUp').style.display = 'block';
}

function addEdgeCallback(data, callback) 
{
	if (data.from == data.to) 
	{
		var r = confirm("Do you want to connect the node to itself?");
		if (r == true) 
		{
			addEdgeCallbackHelper(data, callback);
		}
	}
	else 
	{
		addEdgeCallbackHelper(data, callback);
	}
}

function deleteEdgeCallback(data, callback) 
{
	console.log(data);
	theoryGraph.deleteEdges(data["edges"]);
}

function deleteNodeCallback(data, callback) 
{
	console.log(data);
	theoryGraph.deleteNodes(data["nodes"],data["edges"]);
}

function editEdgeCallbackHelper(data, callback) 
{
	// filling in the popup DOM elements
	document.getElementById('edge-operation').innerHTML = "Edit Edge";
	document.getElementById('edge-id').value = data.id;
	document.getElementById('edge-label').value = (typeof data.label !="undefined") ? data.label : "";
	document.getElementById('edge-url').value = (typeof data.url !="undefined") ? data.url : "";

	var html="";
	Object.keys(ARROW_STYLES).forEach(function (key) 
	{
	   html+='<option value="'+key+'">'+ARROW_STYLES[key].alias+'</option>';
	});
	
	document.getElementById('edge-style').innerHTML = html;
	document.getElementById('edge-saveButton').onclick = editDataEdge.bind(this, data, callback);
	document.getElementById('edge-cancelButton').onclick = clearPopUp.bind();
	document.getElementById('network-edge-popUp').style.display = 'block';
}

function editEdgeCallback(data, callback) 
{
	if (data.from == data.to) 
	{
		var r = confirm("Do you want to connect the node to itself?");
		if (r == true) 
		{
			editEdgeCallbackHelper(data, callback);
		}
	}
	else 
	{
		editEdgeCallbackHelper(data, callback);
	}
}
