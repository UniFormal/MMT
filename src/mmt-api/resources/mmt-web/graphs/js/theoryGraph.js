function TheoryGraph()
{
	// Array holding original nodes
	var originalNodes = null;
	// Array holding original edges
	var originalEdges = null;
	// Object of vis.Network class
	var network = null;
	// Next unique Id to use for cluster
	var clusterId=0;
	// Array holding (parsed) nodes
	var nodes;
	// Array holding (parsed) edges
	var edges;
	// Last used zoom level for clustering
	var lastClusterZoomLevel = 0;
	// Cluster factor when zooming
    var clusterFactor = 1;
	// Clustered points when zooming
	var zoomClusters=[];
	// Positions of nodes before clustering
	var clusterPositions=[];
	var allClusters=[];
	var hiddenNodes={};
	
	
	var edgesNameToHide=[];
	this.onConstructionDone=undefined;
	var that=this;
	
	this.selectNodesByType=function(type)
	{
		var nodeIds = network.getSelectedNodes();;
		for (var i = 0; i < originalNodes.length; i++) 
		{
			var curNode = originalNodes[i];
			if(curNode["style"]==type)
			{
				nodeIds.push(curNode.id);
			}
			
		}
		addToStateHistory("select", {"nodes": nodeIds});
		network.selectNodes(nodeIds);
	}
	
	this.selectEdgesByType=function(type)
	{
		var edgeIds = [];
		for (var i = 0; i < originalEdges.length; i++) 
		{
			var currEdge = originalEdges[i];
			if(currEdge["style"]==type)
			{
				edgeIds.push(currEdge.id);
			}
			
		}
		addToStateHistory("select", {"nodes": edgeIds});
		network.selectEdges(edgeIds);
	}
	
	this.getUsedNodeTypes=function()
	{
		var usedNodeTypes=[];
		for (var i = 0; i < originalNodes.length; i++) 
		{
			if(typeof originalNodes[i]["style"]!="undefined" && usedNodeTypes.indexOf(originalNodes[i]["style"])==-1)
			{
				usedNodeTypes.push(originalNodes[i]["style"]);
			}
		}
		return usedNodeTypes;
	}
	
	this.getUsedEdgeTypes=function()
	{
		var usedEdgeTypes=[];
		for (var i = 0; i < originalEdges.length; i++) 
		{
			if(typeof originalEdges[i]["style"]!="undefined" && usedEdgeTypes.indexOf(originalEdges[i]["style"])==-1)
			{
				usedEdgeTypes.push(originalEdges[i]["style"]);
			}
		}
		return usedEdgeTypes;
	}
	
	this.graphToIFrameString=function(parameterName, onlySelected, compressionRate)
	{
		if (typeof parameterName == "undefined")
		{
			parameterName="tgviewGraphData_"+Math.floor(new Date() / 1000)+"_"+Math.floor(Math.random() * 1000);
		}
		
		if (typeof onlySelected == "undefined")
		{
			onlySelected=false;
		}

		if (typeof compressionRate == "undefined")
		{
			compressionRate=0;
		}
		
		return {"storage":"localStorage.setItem('"+parameterName+"', '"+generateCompressedJSON(onlySelected, compressionRate).split("'").join("\\'")+"');", "uri":location.protocol + '//' + location.host + location.pathname+"?"+graphDataURLSourceParameterNameTGView+"=iframe&"+graphDataURLDataSourceParameterNameTGView+"="+parameterName, "id":parameterName};
	}
	
	this.graphToLocalStorageString=function(parameterName, onlySelected, compressionRate)
	{
		if (typeof parameterName == "undefined")
		{
			parameterName="tgviewGraphData_"+Math.floor(new Date() / 1000)+"_"+Math.floor(Math.random() * 1000);
		}
		
		if (typeof onlySelected == "undefined")
		{
			onlySelected=false;
		}

		if (typeof compressionRate == "undefined")
		{
			compressionRate=0;
		}
		
		return {"storage":"localStorage.setItem('"+parameterName+"', '"+generateCompressedJSON(onlySelected, compressionRate).split("'").join("\\'")+"');", "uri":location.protocol + '//' + location.host + location.pathname+"?"+graphDataURLSourceParameterNameTGView+"=param&"+graphDataURLDataSourceParameterNameTGView+"="+parameterName, "name":parameterName};
	}
	
	this.graphToURIParameterString=function(onlySelected, compressionRate)
	{
		if (typeof onlySelected == "undefined")
		{
			onlySelected=false;
		}

		if (typeof compressionRate == "undefined")
		{
			compressionRate=2;
		}
		
		return location.protocol + '//' + location.host + location.pathname+"?"+graphDataURLSourceParameterNameTGView+"=param&"+graphDataURLDataSourceParameterNameTGView+"="+encodeURI(generateCompressedJSON(onlySelected, compressionRate));
	}
	
	function generateCompressedJSON(onlySelected, compressionRate)
	{	
		var allNodePositions=[];
		
		var json="{\"nodes\":[";
		if (typeof onlySelected == "undefined")
		{
			onlySelected=false;
		}
		
		if (typeof compressionRate == "undefined")
		{
			compressionRate=0;
		}

		if(compressionRate==0)
		{
			allNodePositions=network.getPositions();
		}
		
		var nodeIds=undefined;
		var nodeIdMapping=[];
		
		if(onlySelected==true)
		{
			nodeIds=network.getSelectedNodes();
			
			for (var i = 0; i < nodeIds.length; i++) 
			{
				nodeIdMapping[nodeIds[i]]=1;
			}
		}
		
		var mapping=[];
		var counter=0;
		for (var i = 0; i < originalNodes.length; i++) 
		{
			var currentNodeJson="{";
			var curNode = originalNodes[i];
			
			if(onlySelected==true && typeof nodeIdMapping[curNode.id] == "undefined")
			{
				continue;
			}
			
			if(typeof mapping[curNode.id] == "undefined")
			{
				mapping[curNode.id]=counter;
				counter++;
			}
			
			currentNodeJson+='"id":"'+curNode.id+'",';
			currentNodeJson+='"label":"'+curNode.label+'",';
			currentNodeJson+='"style":"'+curNode.style+'"';
			
			if(typeof curNode.shape != "undefined" && curNode.shape!="")
			{
				currentNodeJson+=',"shape":"'+curNode.shape+'"';
			}
			
			if(typeof curNode.mathml != "undefined" && curNode.mathml!="")
			{
				currentNodeJson+=',"mathml":"'+curNode.mathml.split('"').join("'")+'"';
			}
			
			if(typeof curNode.url != "undefined" && curNode.url!="" && compressionRate<2)
			{
				currentNodeJson+=',"url":"'+curNode.url+'"';
			}
			
			if(compressionRate==0)
			{
				currentNodeJson+=',"x":"'+allNodePositions[curNode.id].x+'"';
				currentNodeJson+=',"y":"'+allNodePositions[curNode.id].y+'"';
			}
			
			currentNodeJson+="},";
			json+=currentNodeJson;
		}
		
		json=json.substring(0, json.length - 1)+"],\"edges\":[";
		
		for (var i = 0; i < originalEdges.length; i++) 
		{				
			var currEdge = originalEdges[i];
			if(typeof mapping[currEdge.to] != "undefined" && mapping[currEdge.from] != "undefined" )
			{
				var currentEdgeJson="{";
				
				currentEdgeJson+='"to":"'+currEdge.to+'",';
				currentEdgeJson+='"from":"'+currEdge.from+'",';
				currentEdgeJson+='"style":"'+currEdge.style+'"';
				
				if(typeof currEdge.label != "undefined" && currEdge.label!="" && compressionRate<2)
				{
					currentEdgeJson+=',"label":"'+currEdge.label+'"';
				}
				
				if(typeof currEdge.weight != "undefined" && currEdge.weight!="" && compressionRate<2)
				{
					currentEdgeJson+=',"weight":"'+currEdge.weight+'"';
				}
				
				if(typeof currEdge.url != "undefined" && currEdge.url!="" && compressionRate<2)
				{
					currentEdgeJson+=',"url":"'+currEdge.url+'"';
				}
				
				currentEdgeJson+="},";
				json+=currentEdgeJson;
			}
		}
		
		if(allClusters.length>0)
		{
			json=json.substring(0, json.length - 1)+"],\"cluster\":[";
			
			for (var i = 0; i < allClusters.length; i++) 
			{		
				var currentClusterJson="{\"nodeIds\":";
				currentClusterJson+=JSON.stringify(clusterPositions[allClusters[i]][0]);
				currentClusterJson+=",";
				
				currentClusterJson+="\"nodePositions\":";
				currentClusterJson+=JSON.stringify(clusterPositions[allClusters[i]][1]);
				currentClusterJson+="";
				
				currentClusterJson+="},";
				json+=currentClusterJson;
			}
		}

		json=json.substring(0, json.length - 1)+"]}";
		return json;
	}
	
	this.loadGraphByLocalStorage=function(parameterName)
	{
		if (typeof parameterName == "undefined")
		{
			parameterName="tgviewGraphData";
		}

		var graphData=localStorage.getItem(parameterName);
		drawGraph(JSON.parse(graphData));
	}
	
	this.loadGraphByURIParameter=function()
	{
		var graphData=getParameterByName(graphDataURLDataSourceParameterNameTGView);
		drawGraph(JSON.parse(graphData));
	}

	// Hides all edges with given type
	this.hideEdges=function(type, hideEdge)
	{
		that.setEdgesHidden(type, hideEdge);
		var edgesToHide=[];
		for(var i=0;i<originalEdges.length;i++)
		{
			//console.log(type+""+originalEdges[i]["style"]);
			if(type==originalEdges[i]["style"] || ("graph"+type)==originalEdges[i]["style"] )
			{
				if(hideEdge==true)
				{
					edgesToHide.push({id: originalEdges[i]["id"], hidden: hideEdge});
				}
				else if(hideEdge==false && (hiddenNodes[originalEdges[i].to]==false && hiddenNodes[originalEdges[i].from]==false))
				{
					edgesToHide.push({id: originalEdges[i]["id"], hidden: hideEdge});
				}
			}
		}
		edges.update(edgesToHide);
	}

	this.hideNodes=function(type, hideEdge)
	{
		//that.setEdgesHidden(type, hideEdge);
		var nodesToHide=[];
		
		for(var i=0;i<originalNodes.length;i++)
		{
			//console.log(type+""+originalEdges[i]["style"]);
			if(type==originalNodes[i]["style"] || ("graph"+type)==originalNodes[i]["style"] )
			{
				nodesToHide.push({id: originalNodes[i]["id"], hidden: hideEdge});
				hiddenNodes[originalNodes[i]["id"]]=hideEdge;
			}
		}
		nodes.update(nodesToHide);
		
		var mappedEdges={};
		for(var i=0;i<edgesNameToHide.length;i++)
		{
			mappedEdges[edgesNameToHide[i].type]=edgesNameToHide[i].hidden;
		}
		
		var edgesToHide=[];
		for(var i=0;i<originalEdges.length;i++)
		{
			if(hideEdge==true && (hiddenNodes[originalEdges[i].to] == true || hiddenNodes[originalEdges[i].from] == true))
			{
				edgesToHide.push({id: originalEdges[i]["id"], hidden: hideEdge});
			}
			
			if(typeof mappedEdges[originalEdges[i]["style"]] != "undefined" && mappedEdges[originalEdges[i]["style"]]!=hideEdge)
			{
				continue;
			}
			
			
			if(hideEdge==false && (hiddenNodes[originalEdges[i].to]==false && hiddenNodes[originalEdges[i].from]==false))
			{
				edgesToHide.push({id: originalEdges[i]["id"], hidden: hideEdge});
			}
		}
		edges.update(edgesToHide);
	}
	
	this.setEdgesHidden=function(type, hideEdge)
	{
		for(var i=0;i<edgesNameToHide.length;i++)
		{
			if(type==edgesNameToHide[i].type)
			{
				edgesNameToHide[i].hidden=hideEdge;
				return;
			}
		}

		edgesNameToHide.push({"hidden": hideEdge,"type": type});
	}
	
	// Downloads canvas as image
	this.downloadCanvasAsImage = function(button)
	{
		var minX=111110;
		var minY=111110;
		var maxX=-111110;
		var maxY=-111110;
		for (var i = 0; i < originalNodes.length; i++) 
		{
			var curNode = originalNodes[i];
			var nodePosition = network.getPositions([curNode.id]);
			
			minX=Math.min(nodePosition[curNode.id].x,minX);
			maxX=Math.max(nodePosition[curNode.id].x,maxX);
			
			minY=Math.min(nodePosition[curNode.id].y,minY);
			maxY=Math.max(nodePosition[curNode.id].y,maxY);
		}
		
		var originalWidth=network.canvas.frame.canvas.width;
		var originalHeight=network.canvas.frame.canvas.height;
		
		network.setSize(Math.min((maxX-minX)*1.2,3500),Math.min((maxY-minY)*1.2,3500));
		
		network.redraw();
		network.fit();
		
		network.once("afterDrawing",function () 
		{
			
			//button.href = network.canvas.frame.canvas.toDataURL();
			//button.download = "graph.png";

			var downloadLink      = document.createElement('a');
			downloadLink.target   = '_blank';
			downloadLink.download = 'graph.png';

			var image=network.canvas.frame.canvas.toDataURL("image/png");

			var URL = window.URL || window.webkitURL;
			var downloadUrl = image;

			// set object URL as the anchor's href
			downloadLink.href = downloadUrl;

			// append the anchor to document body
			document.body.appendChild(downloadLink);

			// fire a click event on the anchor
			downloadLink.click();

			// cleanup: remove element and revoke object URL
			document.body.removeChild(downloadLink);
			URL.revokeObjectURL(downloadUrl);
						
			
			//window.open(image);
			network.setSize(originalWidth,originalHeight);
			network.redraw();
			network.fit();
			setStatusText("");
		});
	}
	
	// Opens all clusters which were clustered "clusterOutliers"
    function openOutlierClusters(scale) 
	{
        var newClusters = [];
        var declustered = false;
        for (var i = 0; i < zoomClusters.length; i++) 
		{
            if (zoomClusters[i].scale < scale) 
			{
                network.openCluster(zoomClusters[i].id);
                lastClusterZoomLevel = scale;
                declustered = true;
            }
            else 
			{
                newClusters.push(zoomClusters[i])
            }
        }
        zoomClusters = newClusters;
    }
	
	// Select all nodes with nodeid
	this.selectNodes = function(nodeIds)
	{
		network.selectNodes(nodeIds);
		addToStateHistory("select", {"nodes": nodeIds});
	}
	
	// Select nodes which has an Id similar to searchId
	this.selectNodesWithIdLike=function(searchId)
	{
		var nodeIds = [];
		for (var i = 0; i < originalNodes.length; i++) 
		{
			var curNode = originalNodes[i];
			if(curNode.id.indexOf(searchId)>-1)
			{
				nodeIds.push(curNode.id);
			}
			
		}
		addToStateHistory("select", {"nodes": nodeIds});
		network.selectNodes(nodeIds);
	}
	
	// Clusters all outliers
	this.clusterOutliers=function(scale)
	{
		var clusterOptionsByData = 
		{
			processProperties: function (clusterOptions, childNodes) 
			{
				clusterId = clusterId + 1;
				var childrenCount = 0;
				for (var i = 0; i < childNodes.length; i++) 
				{
					childrenCount += childNodes[i].childrenCount || 1;
				}
				clusterOptions.childrenCount = childrenCount;
				clusterOptions.label = "# " + childrenCount + "";
				clusterOptions.font = {size: Math.min(childrenCount+20,40)}
				clusterOptions.id = 'cluster_' + clusterId;
				zoomClusters.push({id:'cluster_' + clusterId, scale:scale});
				return clusterOptions;
			},
			clusterNodeProperties: {borderWidth: 2, shape: 'database', color:"orange"}
		}
		network.clusterOutliers(clusterOptionsByData);
	}
	
	// Selects all nodes in area of given rect
		this.selectNodesInRect = function(rect) 
	{
		var fromX;
		var toX;
		var fromY;
		var toY;
		var nodesIdInDrawing = [];
		var xRange = getStartToEnd(rect.startX, rect.w);
		var yRange = getStartToEnd(rect.startY, rect.h);

		for (var i = 0; i < originalNodes.length; i++) 
		{
			var curNode = originalNodes[i];
			var nodePosition = network.getPositions([curNode.id]);
			if(typeof nodePosition!="undefined" && typeof network.body.nodes[curNode.id] !="undefined" && network.body.nodes[curNode.id].options.hidden!=true)
			{
				var nodeXY = network.canvasToDOM({x: nodePosition[curNode.id].x, y: nodePosition[curNode.id].y});
				if (xRange.start <= nodeXY.x && nodeXY.x <= xRange.end && yRange.start <= nodeXY.y && nodeXY.y <= yRange.end) 
				{
					nodesIdInDrawing.push(curNode.id);
				}
			}
		}
		addToStateHistory("select", {"nodes": nodesIdInDrawing});
		network.selectNodes(nodesIdInDrawing);
	}
	
	// Colorizes nodes by name (* used as wildcard, e.g. "identity*" will colorize "identity" and "identity_probs")
	// nodeNames can be an array of names or list of names joined with "," e.g: name1,name2,name3
	this.colorizeNodesByName = function(nodeNames, color)
	{
		if(typeof nodeNames == "undefined" || nodeNames==null || nodeNames==undefined)
		{
			return;
		}
		
		var colorizingIds=[];
		var nodeNamesArray=[];
		if( typeof nodeNames == 'string' ) 
		{
			var nodeNamesArray = nodeNames.replace(" ", "").split(",");
			
		}
		else
		{
			nodeNamesArray=nodeNames;
		}
		
		for(var i=0;i<nodeNamesArray.length;i++)
		{
			console.log("^"+nodeNamesArray[i].replace("*", "(.*)")+"$");
			var re = new RegExp("^"+nodeNamesArray[i].split("*").join("(.*)")+"$");
			for (var j = 0; j < originalNodes.length; j++) 
			{
				if (re.test(originalNodes[j].label)) 
				{
					colorizingIds.push(originalNodes[j].id);
				}
			}
		}
		that.colorizeNodes(colorizingIds,color);
	}
	
	// Colorizes nodes by id
	this.colorizeNodes = function(nodeIds,color)
	{
		if(nodeIds==undefined)
		{
			nodeIds=network.getSelectedNodes();
		}
		
		if(color==undefined)
		{
			color="blue";
		}
		
		if(network!=null)
		{
			var toUpdate=[];
			for (var i=0;i<nodeIds.length;i++) 
			{
				toUpdate.push({id: nodeIds[i], color:{background:color,highlight:{background:color}}});
			}
			nodes.update(toUpdate);
			network.redraw();
		}
	}
	
	// Cluster given nodes 
	this.cluster = function(nodeIds,name,givenClusterId)
	{
		if(typeof givenClusterId ==="undefined")
		{
			givenClusterId=clusterId;
		}
		
		if(nodeIds==undefined)
		{
			nodeIds=network.getSelectedNodes();
		}
		
		if(name==undefined)
		{
			name='cluster_' +givenClusterId;
		}
		
		if(network!=null)
		{
			clusterPositions['cluster_' +givenClusterId]=[];
			clusterPositions['cluster_' +givenClusterId][0]=nodeIds;
			clusterPositions['cluster_' +givenClusterId][1]=network.getPositions(nodeIds);
			allClusters.push('cluster_' +givenClusterId);
			var options = 
			{
				joinCondition:function(nodeOptions) 
				{
					return nodeIds.indexOf(nodeOptions.id) != -1;
				},
				processProperties: function (clusterOptions, childNodes, childEdges) 
				{
                  var totalMass = 0;
                  for (var i = 0; i < childNodes.length; i++) 
				  {
                      totalMass += childNodes[i].mass;
                  }
                  clusterOptions.mass = totalMass;
                  return clusterOptions;
              },
              clusterNodeProperties: {id: 'cluster_' +givenClusterId , borderWidth: 2, shape: 'database', color:"orange", label:name}
			}
			network.clustering.cluster(options);
			addToStateHistory("cluster", {"clusterId": 'cluster_' +givenClusterId, "name": name, "nodes": nodeIds});
			clusterId++;
		}
	}
	
	// Get graph located at jsonURL, downlaod it and render it
	this.getGraph= function(jsonURL)
	{
		setStatusText("Downloading graph...");
		document.body.style.cursor = 'wait';
		
		$.ajaxSetup(
		{
            error: function(x, e) 
			{
                if (x.status == 0) 
				{
					setStatusText('<font color="red">Downloading graph failed (Check Your Network)</font>');
					document.body.style.cursor = 'auto';
                } 
                else if (x.status == 404) 
				{
					setStatusText('<font color="red">Downloading graph failed (Requested URL not found)</font>');
					document.body.style.cursor = 'auto';
                } 
				else if (x.status == 500) 
				{
					setStatusText('<font color="red">Downloading graph failed (Internel Server Error)</font>');
                    document.body.style.cursor = 'auto';
                }  
				else 
				{
					setStatusText('<font color="red">Downloading graph failed (HTTP-Error-Code: '+x.status+')</font>');
					document.body.style.cursor = 'auto';
                }
            }
        });
		
		$.get(jsonURL, drawGraph);
	}

	// Loads graph using JSON
	this.loadJSONGraph=function(data)
	{
		if(data.length<20)
		{
			setStatusText('<font color="red">Graph-File is empty or corrupt</font>');
			document.body.style.cursor = 'auto';
			return;
		}
		
		if(typeof data["nodes"] == 'undefined' || typeof data["edges"] == 'undefined')
		{
			setStatusText('<font color="red">Graph-File is invalid (maybe incorrect JSON?)</font>');
			document.body.style.cursor = 'auto';
			return;
		}

		originalNodes=data["nodes"];
		originalEdges=data["edges"];

		addUsedButNotDefinedNodes();
		
		ensureUniqueIds(originalNodes);
		ensureUniqueIds(originalEdges);
		
		postprocessEdges();
		
		startConstruction(true);
	}
	
	// Draws given data as graph
	function drawGraph(data, status=200)
	{
		if(status!=200 && status!="success")
		{
			setStatusText('<font color="red">Downloading graph failed (HTTP-Error-Code: '+status+')</font>');
			document.body.style.cursor = 'auto';
			return;
		}
	
		if(typeof data["nodes"] == 'undefined' || data.length<20)
		{
			setStatusText('<font color="red">Graph-File is empty</font>');
			document.body.style.cursor = 'auto';
			return;
		}
		
		originalNodes=data["nodes"];
		originalEdges=data["edges"];

		addUsedButNotDefinedNodes();
		
		ensureUniqueIds(originalNodes);
		ensureUniqueIds(originalEdges);
		
		postprocessEdges();
		
		startConstruction();
	}
	
	// Adds nodes to node-array, which were referenced by edges but not specified in nodes-array
	function addUsedButNotDefinedNodes()
	{
		setStatusText("Adding used but not defined nodes...");
		var mappedNodes=[];
		for(var i=0;i< originalNodes.length;i++ )
		{
			mappedNodes[originalNodes[i].id]=originalNodes[i];
		}
		
		for(var i=0;i< originalEdges.length;i++ )
		{
			if(originalEdges[i].from != undefined && mappedNodes[originalEdges[i].from]==undefined)
			{
				var nodeLabel=originalEdges[i].from;
				var exploded=originalEdges[i].from.split("?");
				if(exploded[1]!=undefined)
				{
					nodeLabel=exploded[1];
				}
				
				var addNode=
				{
					"id" : originalEdges[i].from,
					"style" : "border",
					"label" : nodeLabel,
					"url" : originalEdges[i].from
				};
				
				originalNodes.push(addNode);
				mappedNodes[originalEdges[i].from]=addNode;
				console.log("Border-Node: "+nodeLabel+" ("+originalEdges[i].from+")");
			}
			if(originalEdges[i].to!=undefined && mappedNodes[originalEdges[i].to]==undefined)
			{
				var nodeLabel=originalEdges[i].to;
				var exploded=originalEdges[i].to.split("?");
				if(exploded[1]!=undefined)
				{
					nodeLabel=exploded[1];
				}
				
				var addNode=
				{
					"id" : originalEdges[i].to,
					"style" : "border",
					"label" : nodeLabel,
					"url" : originalEdges[i].to
				};
				
				originalNodes.push(addNode);
				mappedNodes[originalEdges[i].to]=addNode;
				console.log("Border-Node: "+nodeLabel+" ("+originalEdges[i].to+")");
			}
		}
	}
	
	// Apply styles to nodes/edges
	function postprocessEdges(nodesIn, edgesIn)
	{
		if(typeof edgesIn =="undefined" )
		{
			edgesIn=originalEdges;
		}
		
		if(typeof nodesIn =="undefined" )
		{
			nodesIn=originalNodes;
		}
		
		for(var i=0;i<edgesIn.length;i++)
		{
			if(edgesIn[i].style!=undefined && ARROW_STYLES[edgesIn[i].style]!=undefined)
			{
				var styleInfos=ARROW_STYLES[edgesIn[i].style];
				edgesIn[i].arrows = {to:{enabled:styleInfos.directed}};
				
				if(styleInfos.circle==true)
				{
					edgesIn[i].arrows.to.type="circle";
				}
				else
				{
					edgesIn[i].arrows.to.type="arrow";
				}

				if(styleInfos.smoothEdge==false)
				{
					edgesIn[i].smooth={enabled: false};
				}
				
				edgesIn[i].dashes=styleInfos.dashes;
				edgesIn[i].width=styleInfos.width;
				edgesIn[i].color={color:styleInfos.color, highlight:styleInfos.colorHighlight, hover:styleInfos.colorHover};
			}
		}

		for(var i=0;i<nodesIn.length;i++)
		{
			if(nodesIn[i].style!=undefined && NODE_STYLES[nodesIn[i].style]!=undefined)
			{
				var styleInfos=NODE_STYLES[nodesIn[i].style];

				if(styleInfos.shape=="ellipse" || styleInfos.shape=="circle")
				{
					if(nodesIn[i].mathml!=undefined && nodesIn[i].mathml!="" && nodesIn[i].mathml.length>10)
						nodesIn[i].shape="circularImage";
					else
						nodesIn[i].shape="ellipse";
				}
				else if(styleInfos.shape=="square")
				{
					if(nodesIn[i].mathml!=undefined && nodesIn[i].mathml!="" && nodesIn[i].mathml.length>10)
						nodesIn[i].shape="image";
					else
						nodesIn[i].shape="square";
				}
				else
				{
					nodesIn[i].shape=styleInfos.shape;
				}
				
				if(typeof nodesIn[i].color=="undefined")
				{
					nodesIn[i].color={highlight:{}};
				}
				
				if(typeof nodesIn[i].shapeProperties=="undefined")
				{
					nodesIn[i].shapeProperties={};
				}
				
				if (typeof styleInfos.color!="undefined" && styleInfos.color!="") 
				{
					nodesIn[i].color.background=styleInfos.color;
				}
				if (typeof styleInfos.colorBorder!="undefined" && styleInfos.colorBorder!="") 
				{
					nodesIn[i].color.border=styleInfos.colorBorder;
				}
				if (typeof styleInfos.colorHighlightBorder!="undefined" && styleInfos.colorHighlightBorder!="") 
				{
					nodesIn[i].color.highlight.border=styleInfos.colorHighlightBorder;
				}
				if (typeof styleInfos.colorHighlight!="undefined" && styleInfos.colorHighlight!="") 
				{
					nodesIn[i].color.highlight.background=styleInfos.colorHighlight;
				}
				if (typeof styleInfos.dashes!="undefined" && styleInfos.dashes==true) 
				{
					nodesIn[i].shapeProperties.borderDashes=[5,5];
				}

			}
		}
	}
	
	function addNodesAndEdges(data, status=200)
	{
		if(status!=200 && status!="success")
		{
			setStatusText('<font color="red">Downloading nodes failed (HTTP-Error-Code: '+status+')</font>');
			document.body.style.cursor = 'auto';
			return;
		}
	
		if(data.length<20)
		{
			setStatusText('<font color="red">Graph-File is empty or corrupt</font>');
			document.body.style.cursor = 'auto';
			return;
		}
		
		if(typeof data["nodes"] == 'undefined' || typeof data["edges"] == 'undefined')
		{
			setStatusText('<font color="red">Graph-File is invalid (maybe incorrect JSON?)</font>');
			document.body.style.cursor = 'auto';
			return;
		}
		
		var nodesJSON=data["nodes"];
		var edgesJSON=data["edges"];
		
		ensureUniqueIds(nodesJSON);
		ensureUniqueIds(edgesJSON);
		
		postprocessEdges(nodesJSON, edgesJSON);
		
		edges.update(edgesJSON);
		nodes.update(nodesJSON);
		
		originalEdges=originalEdges.concat(edgesJSON);
		originalNodes=originalNodes.concat(nodesJSON);
		
		setStatusText("<font color=\"green\">Successfully recieved "+nodesJSON.length+" node(s) and "+edgesJSON.length+" edge(s)!</font>");
		document.body.style.cursor = 'auto';
	}
	
	this.lazyLoadNodes=function(jsonURL)
	{
		if(jsonURL==undefined || jsonURL.length<3)
		{
			return;
		}
		
		setStatusText("Downloading nodes...");
		document.body.style.cursor = 'wait';
		
		$.ajaxSetup(
		{
            error: function(x, e) 
			{
                if (x.status == 0) 
				{
					setStatusText('<font color="red">Downloading nodes failed (Check Your Network)</font>');
					document.body.style.cursor = 'auto';
                } 
                else if (x.status == 404) 
				{
					setStatusText('<font color="red">Downloading nodes failed (Requested URL not found)</font>');
					document.body.style.cursor = 'auto';
                } 
				else if (x.status == 500) 
				{
					setStatusText('<font color="red">Downloading nodes failed (Internel Server Error)</font>');
                    document.body.style.cursor = 'auto';
                }  
				else 
				{
					setStatusText('<font color="red">Downloading nodes failed (HTTP-Error-Code: '+x.status+')</font>');
					document.body.style.cursor = 'auto';
                }
            }
        });
		
		$.get(jsonURL, addNodesAndEdges);
	}
	
	// Make sure every node and edge has unique ids 
	function ensureUniqueIds(arrays)
	{
		var idArray=[];
		for(var i=0;i<arrays.length;i++)
		{
			if(idArray[arrays[i]["id"]]==undefined)
			{
				idArray[arrays[i]["id"]]=1;
			}
			else
			{
				arrays[i]["id"]+="_"+i;
				idArray[arrays[i]["id"]]=1;
			}
		}
	}
	
	// Opens given cluster by id
	this.openCluster = function(nodeId)
	{
		if (network.isCluster(nodeId) == true) 
		{
			var node = network.body.nodes[nodeId].options;
              network.openCluster(nodeId);
			  var toUpdate=[];
			  for (var i=0;i<clusterPositions[nodeId][0].length;i++) 
			  {
				  var id=clusterPositions[nodeId][0][i];
				  toUpdate.push({id: id, x:clusterPositions[nodeId][1][id].x, y:clusterPositions[nodeId][1][id].y});
			  }
			  
			var index = allClusters.indexOf(nodeId);
			if (index > -1) 
			{
				allClusters.splice(index, 1);
			}
			  
			  addToStateHistory("uncluster", {"clusterId": nodeId, "nodes": toUpdate, "name":node.label});
			  nodes.update(toUpdate);
			  network.redraw();
        }
	}
	
	// Estimates extra height of MathML as SVG
	function estimateExtraSVGHeight(expression)
	{
		if(expression.indexOf("frac") == -1 && expression.indexOf("under") == -1  && expression.indexOf("over") == -1)
		{
			return 0;
		}
		else
		{
			return 0;
		}
	}
	
	// Converts MathML node to SVG node
	function nodeToSVGMath(node)
	{
		$('#string_span').html(node["mathml"]);
		var width=$('#string_span').width();
		var height=$('#string_span').height();
		$('#string_span').html("");
		var svg;
		
		if(node["shape"]=="image")
		{
			var overallheight=height+estimateExtraSVGHeight(node["mathml"]);
			svg = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 '+(width+16*1)+' '+(16*1+overallheight)+'" width="'+(width+16*1)+'px" height="'+(16*1+overallheight)+'px" preserveAspectRatio="none">' +
			//svg = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100" preserveAspectRatio="xMinYMin">' +
			'<foreignObject x="8" y="8" width="'+(width+15)+'" height="'+overallheight+'">' +
			node["mathml"] +
			'</foreignObject>' +
			'</svg>';
		}
		else
		{
			svg = '<svg xmlns="http://www.w3.org/2000/svg" width="'+(30*1+width)+'px" height="'+(30*1+height+estimateExtraSVGHeight(node["mathml"]))+'px" preserveAspectRatio="none">' +
			'<foreignObject x="15" y="13" width="100%" height="100%">' +
			node["mathml"] +
			'</foreignObject>' +
			'</svg>';
		}
		node["image"]="data:image/svg+xml;charset=utf-8,"+ encodeURIComponent(svg);
	}
	
	// Start construction of graph
	function startConstruction(fixedPositions=false)
	{
		setStatusText("Constructing graph...");
		var processedNodes=0;
		var nodesCount=0;
		
		for(var i=0;i<originalNodes.length;i++)
		{
			if(originalNodes[i]["image"]!="" && originalNodes[i]["image"]!=undefined)
			{
				nodesCount++;
			}
		}

		for(var i=0;i<originalNodes.length;i++)
		{
			hiddenNodes[originalNodes[i]["id"]]=false;
			if(originalNodes[i]["image"]!="" && originalNodes[i]["image"]!=undefined)
			{
				function callback(node,data, status)
				{
					node["mathml"]=data;
					nodeToSVGMath(node);
					processedNodes++;
					if(processedNodes==nodesCount)
					{
						startRendering();
					}
				}

				var callback2=callback.bind(null,originalNodes[i]);

				$.get(originalNodes[i]["image"], callback2);
			}
			else if(originalNodes[i]["mathml"]!=undefined && originalNodes[i]["mathml"].length>10 && originalNodes[i]["mathml"]!="")
			{
				nodeToSVGMath(originalNodes[i]);
			}
		}
		
		if(nodesCount==0)
		{
			startRendering(fixedPositions);
		}
	}
	
	// Called when the Visualization API is loaded.
	function startRendering(fixedPositions=false) 
	{
		setStatusText("Rendering graph...");
		if(fixedPositions==false)
		{
			if(typeof THEORY_GRAPH_OPTIONS.layout === 'undefined' || typeof THEORY_GRAPH_OPTIONS.layout.ownLayoutIdx === 'undefined' || THEORY_GRAPH_OPTIONS.layout.ownLayoutIdx==1)
			{
				var opti=new Optimizer(originalNodes,originalEdges);
				if(originalNodes.length+originalEdges.length>3000)
				{
					opti.weaklyHierarchicalLayout(500,document.getElementById('nodeSpacingBox').value);
				}
				else if(originalNodes.length+originalEdges.length>2000)
				{
					opti.weaklyHierarchicalLayout(700,document.getElementById('nodeSpacingBox').value);
				}
				else
				{
					opti.weaklyHierarchicalLayout(1000,document.getElementById('nodeSpacingBox').value);
				}
			}
			else if(THEORY_GRAPH_OPTIONS.layout.ownLayoutIdx==2)
			{
				var opti=new Optimizer(originalNodes,originalEdges);
				opti.GenerateRandomSolution();
				if(originalNodes.length+originalEdges.length>3000)
				{
					opti.SolveUsingForces(200,document.getElementById('nodeSpacingBox').value);
				}
				else if(originalNodes.length+originalEdges.length>2000)
				{
					opti.SolveUsingForces(400,document.getElementById('nodeSpacingBox').value);
				}
				else
				{
					opti.SolveUsingForces(600,document.getElementById('nodeSpacingBox').value);
				}
			}
		}
		
		for(var i=0;i<originalEdges.length;i++)
		{
			originalEdges[i].hidden=false;
		}
		
		for(var j=0;j<edgesNameToHide.length;j++)
		{
			for(var i=0;i<originalEdges.length;i++)
			{
				var type=edgesNameToHide[j].type;
				if(type==originalEdges[i]["style"] || ("graph"+type)==originalEdges[i]["style"] )
				{
					originalEdges[i].hidden=edgesNameToHide[j].hidden;
				}
			}
		}
		
		nodes = new vis.DataSet(originalNodes);
		edges = new vis.DataSet(originalEdges);
		
		// create a network
		var container = document.getElementById('mynetwork');
		var data = 
		{
			nodes: nodes,
			edges: edges
		};
		
		network = new vis.Network(container, data, THEORY_GRAPH_OPTIONS);
		//network.startSimulation(10);
		
		if(THEORY_GRAPH_OPTIONS.physics.enabled==false)
		{
			document.body.style.cursor = 'auto';
			setStatusText('<font color="green">Received '+originalNodes.length+' nodes</font>');
		}
		
		network.on('afterDrawing', function() 
		{	
			if(that.onConstructionDone!=undefined)
			{
				var tmp=that.onConstructionDone;
				that.onConstructionDone=undefined;;
				tmp();
				
			}
		});
		
		// If the document is clicked somewhere
		network.on("click", function (e) 
		{
			$("#tooltip-container").hide(10);
			// If the clicked element is not the menu
			if (!$(e.target).parents(".custom-menu").length > 0) 
			{
				// Hide it
				$(".custom-menu").hide(10);
			}
		});
		
		// If the menu element is clicked
		$(".custom-menu li").click(function()
		{
			var nodesFound=network.getSelectedNodes();
			var selectedNode=network.body.nodes[nodesFound[0]];
			
			if (selectedNode==undefined)
			{
				for(var i=0;i<originalNodes.length;i++)
				{
					if(originalNodes[i]["id"]==nodesFound[0])
					{
						selectedNode=originalNodes[i];
						break;
					}
				}
			}
			else
			{
				selectedNode=selectedNode.options;
			}
			
			
			
			var edgesFound=network.getSelectedEdges();
			var selectedEdge=undefined;
			for(var i=0;i<originalEdges.length;i++)
			{
				if(originalEdges[i]["id"]==edgesFound[0])
				{
					selectedEdge=originalEdges[i];
					break;
				}
			}
			
			var selected=undefined;
			
			if(selectedEdge!=undefined)
			{
				selected=selectedEdge;
			}
			
			if(selectedNode!=undefined)
			{
				selected=selectedNode;
			}
			
			if(selected!=undefined)
			{
				// This is the triggered action name
				switch($(this).attr("data-action")) 
				{
					// A case for each action
					case "openWindow": window.open(serverUrl+selected["url"]); break;
					case "showURL": alert(serverUrl+selected["url"]); break;
					case "openCluster": that.openCluster(selected["id"]); break;
					case "inferType": alert("Not implemented yet!"); break;
					case "showDecl": alert("Not implemented yet!"); break;
					case "childNodes": that.lazyLoadNodes(selectedNode.childsURL) ; break;
				}
			}
			
			// Hide it AFTER the action was triggered
			$(".custom-menu").hide(10);
		});
		
		network.on("oncontext", function (params) 
		{
			$("#tooltip-container").hide(10);
			$(".custom-menu").hide(10);
			
			var node=network.getNodeAt({x: params["pointer"]["DOM"]["x"],y: params["pointer"]["DOM"]["y"]});
			
			if(node!=undefined)
			{
				network.selectNodes([node]);
				// Show contextmenu
				$(".custom-menu").finish().show(10).
				
				// In the right position (the mouse)
				css({
					top: params["pointer"]["DOM"]["y"]*1+20 + "px",
					left: params["pointer"]["DOM"]["x"]*1+16+document.getElementById("mainbox").offsetLeft + "px",
				});
				return;
			}
			
			var edge=network.getEdgeAt({x: params["pointer"]["DOM"]["x"],y: params["pointer"]["DOM"]["y"]});
			
			if(edge!=undefined)
			{
				network.selectEdges([edge]);
				
				var selectedEdge=undefined;
				for(var i=0;i<originalEdges.length;i++)
				{
					if(originalEdges[i]["id"]==edge)
					{
						selectedEdge=originalEdges[i];
						break;
					}
				}
					
				if (typeof selectedEdge.clickText != "undefined")
				{
					// Show contextmenu
					$("#tooltip-container").finish().show(10).
					html(selectedEdge.clickText ).
					// In the right position (the mouse)
					css({
						top: params["pointer"]["DOM"]["y"]*1+20 + "px",
						left: params["pointer"]["DOM"]["x"]*1+16+document.getElementById("mainbox").offsetLeft + "px"
					});
				}
			}
			
		});
		
		network.on("stabilizationIterationsDone", function (params) 
		{
			network.stopSimulation();
			var options = 
			{
				physics: 
				{
					enabled: false
				}
			};
			network.setOptions(options);
			document.body.style.cursor = 'auto';
			setStatusText('<font color="green">Received '+originalNodes.length+' nodes</font>');
		});
		
		
		// we use the zoom event for our clustering
		/*network.on('zoom', function (params) 
		{
			console.log(params.direction+" "+params.scale+" < "+lastClusterZoomLevel*clusterFactor);
			if (params.direction == '-') 
			{
				if (params.scale < lastClusterZoomLevel*clusterFactor) 
				{
					that.clusterOutliers(params.scale);
					lastClusterZoomLevel = params.scale;
				}
			}
			else 
			{
				openOutlierClusters(params.scale);
			} 
		});*/
		
		network.once('initRedraw', function() 
		{
			if (lastClusterZoomLevel === 0) 
			{
				lastClusterZoomLevel = network.getScale();
			}
			document.body.style.cursor = 'auto';
		});
	}
}
