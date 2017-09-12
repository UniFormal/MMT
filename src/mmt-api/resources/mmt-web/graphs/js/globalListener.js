var canvasTools;
var ctxTools;
var rectTools;
var dragTools=false;
var containerTools;

$(function () 
{ 
	$('#theory_tree').jstree(
	{
		'core' : 
		{
			"check_callback" : true,
			"themes" : { "stripes" : false,"icons":false },
		},	
		"types" : 
		{
			"default" : 
			{
			  "valid_children" : ["default","file"]
			}
		 },

		"plugins" : 
		[
			"contextmenu", "dnd", "search",
			"state", "types", "wholerow"
		]
	}); 
	 
	//var jsonURL="http://neuralocean.de/graph/test/menu.json";
	var jsonURL=menuEntriesURL;
	lazyParent="#";
	$.get(jsonURL, addTreeNodes);

	$("#theory_tree").on("select_node.jstree",
		function(evt, data)
		{
			lastGraphDataUsed=data.node.original.graphdata;
			var $node = $("#" + data.node.id);
			var y = $node.position().top + 30;
            var x = $node.position().left+96;

			$(".custom-menu-side").finish().show(10).
			// In the right position (the mouse)
			css({
				top: y + "px",
				left: x + "px",
			});
		}
	);
	
	// If the menu element is clicked
	$(".custom-menu-side li").click(function()
	{
		var type=$(this).attr("data-action");
		
		if(type!="close")
		{
			createNewGraph(type, lastGraphDataUsed);
		}
			
		// Hide it AFTER the action was triggered
		$(".custom-menu-side").hide(10);
	});

	
	$("#theory_tree").on("open_node.jstree",
		function(evt, data)
		{
			$(".custom-menu-side").hide(10);
			lazyParent=data.node.id;
			data.node.children=[];
			if(alreadyAdded[lazyParent]!=true)
			{
				console.log(lazyParent+" added: "+alreadyAdded[lazyParent]);
				var jsonURL=menuEntriesURL+data.node.id;
				alreadyAdded[lazyParent]=true;
				$.get(jsonURL, addTreeNodes);
			}
		}
	);
});

$(document).bind("contextmenu", function (event) 
{
	// Avoid the real menu
	event.preventDefault();
});


$(document).ready(function() 
{
	//$('button').button();
	// Accordion
	$(".accordion").accordion({ header: "h3" });
	// Tabs
	$('#tabs').tabs();
	// Button Set
	$("#radio1").buttonset();
	$( "#methodCluster" ).selectmenu();
		
	canvasTools=document.getElementById('toolCanvas');
	ctxTools=canvasTools.getContext('2d');
	rectTools = {};
	containerTools = $("#toolCanvas");

	var canvasOffset=containerTools.offset();
    var offsetX=canvasOffset.left;
    var offsetY=canvasOffset.top;
	
    containerTools.on("mousemove", function(e) 
	{

        if (dragTools==true && selectionMode==true) 
		{ 
			rectTools.w = e.offsetX  - rectTools.startX;
			rectTools.h = e.offsetY  - rectTools.startY ;

			ctxTools.clearRect(0, 0, canvasTools.width, canvasTools.height);
            ctxTools.setLineDash([5]);
            ctxTools.strokeStyle = "rgb(0, 102, 0)";
            ctxTools.strokeRect(rectTools.startX, rectTools.startY, rectTools.w, rectTools.h);
            ctxTools.setLineDash([]);
            ctxTools.fillStyle = "rgba(0, 255, 0, 0.2)";
            ctxTools.fillRect(rectTools.startX, rectTools.startY, rectTools.w, rectTools.h);
			console.log(rectTools.startX,rectTools.startY, rectTools.w, rectTools.h);
        }
		
    });

    containerTools.on("mousedown", function(e) 
	{
        if (selectionMode==true) 
		{ 
			rectTools.w=0;
			rectTools.h=0;
            rectTools.startX = e.offsetX ;
            rectTools.startY = e.offsetY ;
            dragTools = true;   
        }
    }); 

    containerTools.on("mouseup", function(e) 
	{
        if (dragTools==true) 
		{ 
            dragTools = false;
            theoryGraph.selectNodesInRect(rectTools);
			ctxTools.clearRect(0, 0, canvasTools.width, canvasTools.height);
			switchSelectionMode();
        }
    });
});

// Resize Section //
var divW = 0;
jQuery(document).ready(function()
{
	checkResize();
});

function checkResize()
{
	var w = jQuery("#theory_tree_div").width();
	if (w != divW) 
	{
		divW = w;
		
		var treeDiv = jQuery('#theory_tree_div');
		
		var htmlCanvas = document.getElementById('toolCanvas');
		htmlCanvas.width = (window.innerWidth*0.90-divW)|0;
		htmlCanvas.height = (window.innerHeight*0.80)|0;
		htmlCanvas.style.width=htmlCanvas.width+"px";
		htmlCanvas.style.height=htmlCanvas.height+"px";
		
		
		htmlCanvas = document.getElementById('mainbox');
		htmlCanvas.width = (window.innerWidth*0.90-divW)|0;
		htmlCanvas.style.width=htmlCanvas.width+"px";
		
		htmlCanvas = document.getElementById('wholeNetwork');
		htmlCanvas.width = (window.innerWidth*0.90-divW)|0;
		htmlCanvas.height = (window.innerHeight*0.80)|0;
		htmlCanvas.style.width=htmlCanvas.width+"px";
		htmlCanvas.style.height=htmlCanvas.height+"px";
	}
}
jQuery(window).resize(checkResize);
var timer = setInterval(checkResize, 250);


