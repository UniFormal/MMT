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

	var that=this;
	
	// Hides all edges with given type
	this.hideEdges=function(type, hideEdge)
	{
		var edgesToHide=[];
		for(var i=0;i<originalEdges.length;i++)
		{
			console.log(type+""+originalEdges[i]["style"]);
			if(type==originalEdges[i]["style"] || ("graph"+type)==originalEdges[i]["style"] )
			{
				edgesToHide.push({id: originalEdges[i]["id"], hidden: hideEdge});
			}
		}
		edges.update(edgesToHide);
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
			var nodeXY = network.canvasToDOM({x: nodePosition[curNode.id].x, y: nodePosition[curNode.id].y});
			if (xRange.start <= nodeXY.x && nodeXY.x <= xRange.end && yRange.start <= nodeXY.y && nodeXY.y <= yRange.end) 
			{
				nodesIdInDrawing.push(curNode.id);
			}
		}
		addToStateHistory("select", {"nodes": nodesIdInDrawing});
		network.selectNodes(nodesIdInDrawing);
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
		drawGraph(data);
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
	function postprocessEdges()
	{
		for(var i=0;i<originalEdges.length;i++)
		{
			if(originalEdges[i].style!=undefined && ARROW_STYLES[originalEdges[i].style]!=undefined)
			{
				var styleInfos=ARROW_STYLES[originalEdges[i].style];
				originalEdges[i].arrows = {to:{enabled:styleInfos.directed}};
				
				if(styleInfos.circle==true)
				{
					originalEdges[i].arrows.to.type="circle";
				}
				else
				{
					originalEdges[i].arrows.to.type="arrow";
				}

				if(styleInfos.smoothEdge==false)
				{
					originalEdges[i].smooth={enabled: false};
				}
				
				originalEdges[i].dashes=styleInfos.dashes;
				originalEdges[i].width=styleInfos.width;
				originalEdges[i].color={color:styleInfos.color, highlight:styleInfos.colorHighlight, hover:styleInfos.colorHover};
			}
		}

		for(var i=0;i<originalNodes.length;i++)
		{
			if(originalNodes[i].style!=undefined && NODE_STYLES[originalNodes[i].style]!=undefined)
			{
				var styleInfos=NODE_STYLES[originalNodes[i].style];

				if(styleInfos.shape=="ellipse" || styleInfos.shape=="circle")
				{
					if(originalNodes[i].mathml!=undefined && originalNodes[i].mathml!="" && originalNodes[i].mathml.length>10)
						originalNodes[i].shape="circularImage";
					else
						originalNodes[i].shape="ellipse";
				}
				else if(styleInfos.shape=="square")
				{
					if(originalNodes[i].mathml!=undefined && originalNodes[i].mathml!="" && originalNodes[i].mathml.length>10)
						originalNodes[i].shape="image";
					else
						originalNodes[i].shape="square";
				}
				else
				{
					originalNodes[i].shape=styleInfos.shape;
				}
				
				if(typeof originalNodes[i].color=="undefined")
				{
					originalNodes[i].color={highlight:{}};
				}
				
				if(typeof originalNodes[i].shapeProperties=="undefined")
				{
					originalNodes[i].shapeProperties={};
				}
				
				if (typeof styleInfos.color!="undefined" && styleInfos.color!="") 
				{
					originalNodes[i].color.background=styleInfos.color;
				}
				if (typeof styleInfos.colorBorder!="undefined" && styleInfos.colorBorder!="") 
				{
					originalNodes[i].color.border=styleInfos.colorBorder;
				}
				if (typeof styleInfos.colorHighlightBorder!="undefined" && styleInfos.colorHighlightBorder!="") 
				{
					originalNodes[i].color.highlight.border=styleInfos.colorHighlightBorder;
				}
				if (typeof styleInfos.colorHighlight!="undefined" && styleInfos.colorHighlight!="") 
				{
					originalNodes[i].color.highlight.background=styleInfos.colorHighlight;
				}
				if (typeof styleInfos.dashes!="undefined" && styleInfos.dashes==true) 
				{
					originalNodes[i].shapeProperties.borderDashes=[5,5];
				}

			}
		}
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
              network.openCluster(nodeId);
			  var toUpdate=[];
			  for (var i=0;i<clusterPositions[nodeId][0].length;i++) 
			  {
				  var id=clusterPositions[nodeId][0][i];
				  toUpdate.push({id: id, x:clusterPositions[nodeId][1][id].x, y:clusterPositions[nodeId][1][id].y});
			  }
			  addToStateHistory("uncluster", {"clusterId": nodeId, "nodes": toUpdate});
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
	function startConstruction()
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
			startRendering();
		}
	}
	
	// Called when the Visualization API is loaded.
	function startRendering() 
	{
		setStatusText("Rendering graph...");
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
