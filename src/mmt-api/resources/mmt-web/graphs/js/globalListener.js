var canvasTools;
var ctxTools;
var rectTools;
var dragTools=false;
var containerTools;


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