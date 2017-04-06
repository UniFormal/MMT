function TheoryGraph()
{
	var originalNodes = null;
	var originalEdges = null;
	var network = null;
	
	this.getGraph= function(jsonURL)
	{
		document.body.style.cursor = 'wait';
		$.get(jsonURL, drawGraph);
	}

	function drawGraph(data, status)
	{
		originalNodes=data["nodes"];
		originalEdges=data["edges"];
		
		ensureUniqueIds(originalNodes);
		ensureUniqueIds(originalEdges);
		
		postprocessEdges();
		
		startRendering();
	}
	
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

				originalEdges[i].dashes=styleInfos.dashes;
				originalEdges[i].color={color:styleInfos.color, highlight:styleInfos.colorHighlight, hover:styleInfos.colorHover};
			}
		}
	}
	
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
	
	// Called when the Visualization API is loaded.
	function startRendering() 
	{
		var opti=new Optimizer(originalNodes,originalEdges);
		opti.GenerateRandomSolution();
		opti.SolveUsingForces(1000);
		
		var nodes = new vis.DataSet(originalNodes);
		var edges = new vis.DataSet(originalEdges);
		
		// create a network
		var container = document.getElementById('mynetwork');
		var data = 
		{
			nodes: nodes,
			edges: edges
		};
		
		network = new vis.Network(container, data, THEORY_GRAPH_OPTIONS);
		
		if(THEORY_GRAPH_OPTIONS.physics.enabled==false)
		{
			document.body.style.cursor = 'auto';
		}
		
		// If the document is clicked somewhere
		network.on("click", function (e) 
		{
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
			var selectedNode=undefined;
			
			for(var i=0;i<originalNodes.length;i++)
			{
				if(originalNodes[i]["id"]==nodesFound[0])
				{
					selectedNode=originalNodes[i];
					break;
				}
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
					case "first": window.open(selected["url"]); break;
				}
			}
			
			// Hide it AFTER the action was triggered
			$(".custom-menu").hide(10);
		});
		
		network.on("oncontext", function (params) 
		{
			var node=network.getNodeAt({x: params["pointer"]["DOM"]["x"],y: params["pointer"]["DOM"]["y"]});
			
			if(node!=undefined)
			{
				network.selectNodes([node]);
				// Show contextmenu
				$(".custom-menu").finish().show(10).
				
				// In the right position (the mouse)
				css({
					top: params["pointer"]["DOM"]["y"]*1+20 + "px",
					left: params["pointer"]["DOM"]["x"]*1+16 + "px"
				});
				return;
			}
			
			var edge=network.getEdgeAt({x: params["pointer"]["DOM"]["x"],y: params["pointer"]["DOM"]["y"]});
			
			if(edge!=undefined)
			{
				network.selectEdges([edge]);
				// Show contextmenu
				$(".custom-menu").finish().show(10).
				
				// In the right position (the mouse)
				css({
					top: params["pointer"]["DOM"]["y"]*1+20 + "px",
					left: params["pointer"]["DOM"]["x"]*1+16 + "px"
				});
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
		});
	}
}