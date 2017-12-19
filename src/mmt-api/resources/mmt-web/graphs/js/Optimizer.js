// Some constants
var Constants=[];
Constants.DependencyWidth = 30;
Constants.DependencyHeight=30;

// Class representing a line
function HelperLine(xStart, yStart, xEnd, yEnd)
{
	this.xStart = xStart;
	this.yStart = yStart;
	this.xEnd = xEnd;
	this.yEnd = yEnd;
}

// Optimizer CLass
function Optimizer(nodes, edges)
{
	// Amount of line colisions in current solution
	var overAllColision;
	// Copy of all nodes
	var myAllNodes=nodes;
	// Array representing a colision matrix, to check for colisions quickly
	var field=[];
	// Max width of canvas/area to use
	var myWidth=12000;
	// Max height of canvas/area to use
	var myHeight=12000;
	// Amount of nodes in graph
	var countNodesInGraph;
	// Amount of edges in graph
	var edgesCount=edges.length;
	var that=this;
	
	mapEdgesIntoNodes(edges);
	identifySubgraphs();
	
	// Find subgraphs and give each graph an unique number, which will be propagated to the nodes
	// e.g. myAllNodes[i].graphNumber=1
	function identifySubgraphs()
	{
		setStatusText("Identify Subgraphs...");
		for(var i=0;i< myAllNodes.length;i++ )
		{
			myAllNodes[i].graphNumber=-1;
		}
		
		var nodesToCheck = [];
		countNodesInGraph = [];
		var graphNumber = 1;
		for(var i=0;i<myAllNodes.length;i++ )
		{
			var n=myAllNodes[i];
			if( n.graphNumber < 1 )
			{
				nodesToCheck.push( n );
				countNodesInGraph.push( 0 );
				
				while( nodesToCheck.length > 0 )
				{
					countNodesInGraph[ countNodesInGraph.length - 1 ]++;
					
					var currNode = nodesToCheck.pop( );
					currNode.graphNumber = graphNumber;
					
					for(var j=0;j<currNode.connectedNodes.length;j++ )
					{
						var u=currNode.connectedNodes[j];
						if( u.graphNumber < 1 )
						{
							nodesToCheck.push( u );
						}
					}
				}
				graphNumber++;
			}
		}
	}
	
	// Sets the connection between each node (so maps the edges-array directly to attributes of nodes)
	// e.g. mappedNodes[edges[i].from].toConnected.push(mappedNodes[edges[i].to]);
	function mapEdgesIntoNodes(edges)
	{
		setStatusText("Mapping Edges to Nodes...");
		var mappedNodes=[];
		for(var i=0;i< myAllNodes.length;i++ )
		{
			myAllNodes[i].toConnected=[];
			myAllNodes[i].fromConnected=[];
			myAllNodes[i].connectedNodes=[];
			mappedNodes[myAllNodes[i].id]=myAllNodes[i];
		}
		
		for(var i=0;i< edges.length;i++ )
		{
			if(edges[i].from == undefined || mappedNodes[edges[i].from]==undefined)
			{
				console.log("Not found: "+edges[i].from);
			}
			else
			{
				if(edges[i].to==undefined || mappedNodes[edges[i].to]==undefined)
				{
					console.log("Not found: "+edges[i].to);
				}
				else
				{
					mappedNodes[edges[i].from].toConnected.push(mappedNodes[edges[i].to]);
					mappedNodes[edges[i].to].fromConnected.push(mappedNodes[edges[i].from]);
					
					mappedNodes[edges[i].to].connectedNodes.push(mappedNodes[edges[i].from]);
					mappedNodes[edges[i].from].connectedNodes.push(mappedNodes[edges[i].to]);
				}
			}
		}
	}
	
	// Initializes the optimizer with a random but heurstically optimized solution
	this.GenerateRandomSolution = function()
	{
		lines = [];
		var i = 0;
		
		for(var i=0;i< myAllNodes.length;i++ )
		{
			InsertNodeAtGoodPosition( myAllNodes[i], lines);
		}
	}

	// Sets x and y of colision field
	function UpdateFieldColision( x,  y,  value = 1 )
	{
		field[ y * myWidth + x ] = value;
	}
	
	// Finds heuristically a good node position to insert new node to
	function InsertNodeAtGoodPosition(n, lines, iterations = 5 )
	{
		var xOffset = Constants.DependencyWidth / 2;
		var yOffset = Constants.DependencyHeight / 2;

		var x;
		var y;
		var bestX = 0;
		var bestY = 0;
		var bestColision = 10000000;
		var nodes = n.connectedNodes;

		if( nodes == undefined || nodes.length == 0 )
		{
			do
			{
				bestX = (Math.random() * myWidth/Constants.DependencyWidth)|0;
				bestY = (Math.random() * myHeight/Constants.DependencyHeight)|0;
			} while( field[ bestY * myWidth + bestX ] == 1 );

			n.x=( bestX * Constants.DependencyWidth );
			n.y=( bestY * Constants.DependencyHeight );

			UpdateFieldColision( bestX, bestY );
			return;
		}

		var solution = false;
		for( var i = 0; i < iterations || solution == false; i++ )
		{
			x = (Math.random() * myWidth/Constants.DependencyWidth)|0;
			y = (Math.random() * myHeight/Constants.DependencyHeight)|0;

			if( field[ y * myWidth + x ] == 1 )
			{
				continue;
			}

			solution = true;

			var colision = 0;
			var currX = x * Constants.DependencyWidth;
			var currY = y * Constants.DependencyHeight;

			for(var j=0;j< nodes.length;j++ )
			{
				if( currX > nodes[j].x )
				{
					colision += GetLineColision( new HelperLine( nodes[j].x + xOffset, nodes[j].y + yOffset, currX + xOffset, currY + yOffset ), lines );
				}
				else
				{
					colision += GetLineColision( new HelperLine( currX + xOffset, currY + yOffset, nodes[j].x + xOffset, nodes[j].y + yOffset ), lines );
				}
			}

			if( bestColision > colision )
			{
				bestColision = colision;
				bestX = x;
				bestY = y;
			}
		}

		n.x=( bestX * Constants.DependencyWidth );
		n.y=( bestY * Constants.DependencyHeight );

		UpdateFieldColision( bestX, bestY );
		
		for(var j=0;j< nodes.length;j++ )
		{
			if( n.x > nodes[j].x )
			{
				lines.push( new HelperLine( nodes[j].x + xOffset, nodes[j].y + yOffset, n.x + xOffset, n.y + yOffset ) );
			}
			else
			{
				lines.push( new HelperLine( n.x + xOffset, n.y + yOffset, nodes[j].x + xOffset, nodes[j].y + yOffset ) );
			}
		}
	}

	// Calculates fitness of current solution (the less overlapping lines the better is fitness)
	function CalculateFitness()
	{
		if( OverAllColision != -1 )
		{
			return;
		}

		OverAllColision = 0;

		var xOffset = Constants.DependencyWidth / 2;
		var yOffset = Constants.DependencyHeight / 2;

		lines = [];
		for(var i=0;i< myAllNodes.length;i++ )
		{
			nodes = myAllNodes[i].connectedNodes;
			if( nodes != undefined && nodes.length != 0 )
			{
				for(var j=0;j< nodes.length;j++ )
				{
					if( myAllNodes[i].x > nodes[j].x )
					{
						lines.push( new HelperLine( nodes[j].x + xOffset, nodes[j].y + yOffset, myAllNodes[i].x + xOffset, myAllNodes[i].y + yOffset ) );
					}
					else
					{
						lines.push( new HelperLine( myAllNodes[i].x + xOffset, myAllNodes[i].y + yOffset, nodes[j].x + xOffset, nodes[j].y + yOffset ) );
					}
				}
			}
		}

		OverAllColision += GetAllLineColision( lines );
	}

	function ccw(  x1,  y1,  x2,  y2,  x3,  y3 )
	{
		var val = (y2 - y1) * (x3 - x2) - (x2 - x1) * (y3 - y2);
		if( val == 0 )
		{
			return 0; // colinear
		}

		return (val > 0) ? 1 : 2; // clock or counterclock wise
	}

	// Gets amount of line colisions overall
	function GetAllLineColision( lines )
	{
		var colision = 0;

		for( var i = 0; i < lines.length; i++ )
		{
			for(var j=i+1;j< lines.length;j++ )
			{
				var currLine=lines[j];
				if( (currLine.xEnd == lines[ i ].xEnd || currLine.xStart == lines[ i ].xEnd || currLine.xEnd == lines[ i ].xStart
					 || currLine.xStart == lines[ i ].xStart)
					&& (currLine.yEnd == lines[ i ].yEnd || currLine.yStart == lines[ i ].yEnd || currLine.yEnd == lines[ i ].yStart
						|| currLine.yStart == lines[ i ].yStart) )
				{
					continue;
				}

				if( ccw( currLine.xStart, currLine.yStart, currLine.xEnd, currLine.yEnd, lines[ i ].xStart, lines[ i ].yStart )
					!= ccw( currLine.xStart, currLine.yStart, currLine.xEnd, currLine.yEnd, lines[ i ].xEnd, lines[ i ].yEnd )
					&& ccw( lines[ i ].xStart, lines[ i ].yStart, lines[ i ].xEnd, lines[ i ].yEnd, currLine.xStart, currLine.yStart )
					!= ccw( lines[ i ].xStart, lines[ i ].yStart, lines[ i ].xEnd, lines[ i ].yEnd, currLine.xEnd, currLine.yEnd ) )
				{
					colision++;
				}
			}

		}

		return colision;
	}

	// Finds a random path from startNode to any node (until all nodes are visited or there is no other way)
	function findRandomPath(startNode)
	{
		if(startNode===undefined)
			return undefined;
		var nodesToprocess=[startNode];
		var currentPath=[];
		while(nodesToprocess.length>0)
		{
			var n=nodesToprocess.pop();
			n.overallLength+=currentPath.length;
			n.overallVisited+=1;
			currentPath.push(n);
			var nextNode=Math.floor(Math.random() * n.connectedNodes.length);
			
			if(n.connectedNodes.length==0)
			{
				return currentPath;
			}
			
			var maxNode=n.connectedNodes[0];
			if(Math.floor(Math.random() * 100)>66)
			{
				for(var i=0;i<n.connectedNodes.length;i++)
				{
					if(n.connectedNodes[i].connectedNodes!=undefined && maxNode!=undefined && n.connectedNodes[i].connectedNodes.length>maxNode.connectedNodes.length && n.connectedNodes[i].visited!=true)
					{
						maxNode=n.connectedNodes[i];
						nextNode=i;
					}
				}
			}
			
			var i=0;
			while(n.connectedNodes[nextNode].visited==true || n.connectedNodes[nextNode].connectedNodes==undefined || n.connectedNodes[nextNode].connectedNodes.length==0)
			{
				nextNode=i;
				i++;
				if(i>=n.connectedNodes.length)
				{
					return currentPath;
				}
			}
			n.connectedNodes[nextNode].visited=true;
			nodesToprocess.push(n.connectedNodes[nextNode]);
		}
		
		for( var j = 0; j < startNode.connectedNodes.length; j++ )
		{
			var n=startNode.connectedNodes[j];
		}
		return currentPath;
	}
	
	// Function to calculate weakly hierarchical layout of graph
	this.weaklyHierarchicalLayout = function(iterations, spacingValue, usingMinMax = false, currTemperature = 0.9, initialStep = 30.0 )
	{
		for( var j = 0; j < myAllNodes.length; j++ )
		{
			var n=myAllNodes[j];
			n.overallLength=0;
			n.overallVisited=0;
		}
		
		var distClusterCenter=40*spacingValue;
		var distNodes=40*spacingValue;
		
		var nodesOrderedByEdges=[];
		var maxEdgesDif=0;
		var maxNode=myAllNodes[0];
		for( var j = 0; j < myAllNodes.length; j++ )
		{
			var n=myAllNodes[j];
			n.forcesFixed=false;
			var edgesDif=Math.abs(n.toConnected.length-n.fromConnected.length);
			if(typeof nodesOrderedByEdges[edgesDif]==='undefined')
			{
				nodesOrderedByEdges[edgesDif]=[];
			}
			nodesOrderedByEdges[edgesDif].push(n);
			
			if(edgesDif>maxEdgesDif)
			{
				maxEdgesDif=edgesDif;
			}
			
			if(maxNode.toConnected.length < n.toConnected.length)
			{
				maxNode=n;
			}
		}
		
		var longPath=[];
		for(var i=0;i<iterations*16;i++)
		{
			if(i%50==0)
			{
				setStatusText("Beautify Layout: Iteration "+i+" of "+(iterations*2)+"...");
			}
			
			for( var j = 0; j < myAllNodes.length; j++ )
			{
				myAllNodes[j].visited=false;
			}
			
			var tempPath=findRandomPath(maxNode);
			
			if(tempPath!==undefined && tempPath.length>longPath.length)
			{
				longPath=tempPath.slice();
			}
		}
		
		for( var j = 0; j < myAllNodes.length; j++ )
		{
			if(myAllNodes[j].overallLength==undefined || myAllNodes[j].overallVisited==0)
			{
				if(myAllNodes[j].connectedNodes.length!=0)
				{
					for(var i=0;i<myAllNodes[j].connectedNodes.length;i++)
					{
						myAllNodes[j].overallLength+=myAllNodes[j].connectedNodes[i].overallLength;
					}
				}
				
				if(myAllNodes[j].overallLength==0 || myAllNodes[j].connectedNodes.length==0)
				{
					myAllNodes[j].overallLength=Math.floor(Math.random() * 20);
				}
				else
				{
					myAllNodes[j].overallLength=Math.floor(myAllNodes[j].overallLength/myAllNodes[j].connectedNodes.length);
				}
			}
			else
			{
				myAllNodes[j].overallLength=Math.floor(myAllNodes[j].overallLength/myAllNodes[j].overallVisited);
			}
		}
		
		var xUsed=[];
		var yUsed=[];
		
		for( var j = 0; j < myAllNodes.length*2+20; j++ )
		{
			xUsed[j]=0;
			yUsed[j]=0;
		}
		
		var maxLevel=0; 
		for(var i=0;i<myAllNodes.length;i++)
		{
			var level=myAllNodes[i].overallLength;
			if(level>maxLevel)
			{
				maxLevel=level;
			}
		}
		
		maxLevel+=1;
		var lengthMapping=[];
		for(var i=0;i<myAllNodes.length;i++)
		{
			if(myAllNodes[i].overallLength>=0)
			{
				lengthMapping[myAllNodes[i].overallLength]=myAllNodes[i].overallLength;
			}
		}
		
		lengthMapping[maxLevel]=maxLevel;
		var sub=0;
		for(var i=0;i<=maxLevel+1;i++)
		{
			if(typeof lengthMapping[i] === 'undefined' || lengthMapping[i]<0)
			{
				sub+=1;
			}
			else
			{
				lengthMapping[i]-=sub;
			}
		}
		
		var upDown=true;
		
		for(var i=0;i<myAllNodes.length;i++)
		{
			if(myAllNodes[i].toConnected==undefined || myAllNodes[i].toConnected.length==0 || (myAllNodes[i].fromConnected.length-myAllNodes[i].toConnected.length)>myAllNodes.length/3)
			{
				myAllNodes[i].overallLength=maxLevel;
			}

			var level=lengthMapping[myAllNodes[i].overallLength];
			var currX=xUsed[level];
			var currY=yUsed[level];
			if(upDown==false)
			{
				yUsed[level]+=distNodes;
				myAllNodes[i].y=yUsed[level];
				myAllNodes[i].x=level*distClusterCenter;
			}
			else
			{
				xUsed[level]+=distNodes;
				myAllNodes[i].x=xUsed[level];
				myAllNodes[i].y=level*distClusterCenter;
			}
			
			console.log("Level["+level+"; real:"+myAllNodes[i].overallLength+"]: "+myAllNodes[i].x+" "+myAllNodes[i].y);
			
		}
		
		for(var i=0;i<myAllNodes.length;i++)
		{
			if(myAllNodes[i].connectedNodes.length>1)
			{
				myAllNodes[i].forcesFixed=true;
			}
			
			var level=lengthMapping[myAllNodes[i].overallLength];
			if(upDown==false)
			{
				myAllNodes[i].y-=yUsed[level]/2;
			}
			else
			{
				myAllNodes[i].x-=xUsed[level]/2;
			}
		}
		that.SolveUsingForces(iterations*2, spacingValue, false , usingMinMax, currTemperature , initialStep);
		that.SolveUsingForces(5, spacingValue, true, usingMinMax, currTemperature , 3);
	}
	
	// Function to calculate forces driven layout
	this.SolveUsingForces = function(iterations, spacingValue, resetForcesFixed=true, usingMinMax = false, currTemperature = 0.9, initialStep = 30.0 )
	{
		if(resetForcesFixed==true)
		{
			for( var j = 0; j < myAllNodes.length; j++ )
			{
				var n=myAllNodes[j];
				n.forcesFixed=false;
			}
		}
		
		var energy = 1000000;
		var step = initialStep;
		var success = 0;

		var area = myWidth * myHeight;
		var kVal =  Math.max(Math.min((myAllNodes.length*4+edgesCount/2.5)/2 * 0.5*spacingValue/7.0,300),70);
		var kSquared = kVal * kVal;

		
		for( var i = 0; i < iterations; i++ )
		{
			if(i%100==0)
			{
				setStatusText("Beautify Layout: Iteration "+i+" of "+iterations+"...");
			}
			
			var energyBefore = energy;
			energy = 0;
			for( var j = 0; j < myAllNodes.length; j++ )
			{
				var n=myAllNodes[j];
				if(n.forcesFixed===false)
				{
					n.dispX = 0;
					n.dispY = 0;
					// calculate global (repulsive) forces
					for( var k = 0; k < myAllNodes.length; k++ )
					{
						var u=myAllNodes[k];
						if(u.graphNumber==n.graphNumber && n != u && u.connectedNodes != undefined && u.connectedNodes.length > 0 )
						{
							var differenceNodesX = u.x - n.x;
							var differenceNodesY = u.y - n.y;
							
							var lengthDiff =  Math.sqrt( differenceNodesX * differenceNodesX + differenceNodesY * differenceNodesY ) + 0.001;
							var repulsiveForce = - (kSquared / lengthDiff);

							n.dispX += (differenceNodesX / lengthDiff) * repulsiveForce;
							n.dispY += (differenceNodesY / lengthDiff) * repulsiveForce;
						}
					}
					
					// calculate local (spring) forces
					for( var k = 0; k < n.connectedNodes.length; k++ )
					{
						var u=n.connectedNodes[k];
						var differenceNodesX = u.x - n.x;
						var differenceNodesY = u.y - n.y;
						
						var lengthDiff =  Math.sqrt( differenceNodesX * differenceNodesX + differenceNodesY * differenceNodesY ) + 0.001;
						var attractiveForce = (lengthDiff * lengthDiff / kVal);

						n.dispX += (differenceNodesX / lengthDiff) * attractiveForce;
						n.dispY += (differenceNodesY / lengthDiff) * attractiveForce;
					}

					
					// Limit max displacement to temperature currTemperature
					var dispLength =  Math.sqrt( n.dispX * n.dispX + n.dispY * n.dispY ) + 0.001;
					n.x=(  (n.x + (n.dispX / dispLength) * step) );
					n.y=(  (n.y + (n.dispY / dispLength) * step) );

					// Prevent from displacement outside of frame
					if( usingMinMax == true )
					{
						n.x=( Math.max( 48, Math.min( n.x, myWidth - 48 ) ) );
						n.y=( Math.max( 8, Math.min( n.y, myHeight - 32 ) ) );
					}
					energy += dispLength * dispLength;
				}
			}
			// Reduce the temperature as the layout approaches a better configuration

			if( energy < energyBefore )
			{
				success++;
				if( success >= 5 )
				{
					success = 0;
					step /= currTemperature;
				}
			}
			else
			{
				success = 0;
				step *= currTemperature;
			}
		}
		placeGraphs();
	}

	// Places all different graphs on canvas with no overlap
	function placeGraphs()
	{
		var rows=(countNodesInGraph.length+1)>>1;
		var graphRects=[];
		
		for(var i=1;i<= countNodesInGraph.length;i++ )
		{
			graphRects[i]=[];
			graphRects[i][0]=1000000;
			graphRects[i][1]=1000000;
			graphRects[i][2]=-1000000;
			graphRects[i][3]=-1000000;
		}
		
		for(var i=0;i< myAllNodes.length;i++ )
		{
			graphRects[myAllNodes[i].graphNumber][0]=Math.min(graphRects[myAllNodes[i].graphNumber][0],myAllNodes[i].x);
			graphRects[myAllNodes[i].graphNumber][1]=Math.min(graphRects[myAllNodes[i].graphNumber][1],myAllNodes[i].y);
			graphRects[myAllNodes[i].graphNumber][2]=Math.max(graphRects[myAllNodes[i].graphNumber][2],myAllNodes[i].x+100);
			graphRects[myAllNodes[i].graphNumber][3]=Math.max(graphRects[myAllNodes[i].graphNumber][3],myAllNodes[i].y+100);
		}

		
		var rectNewPos=[];
		var currX=0;
		var currY=0;
		var biggestHeight=0;
		for(var i=1;i< graphRects.length;i++ )
		{	
			if(i%rows==0)
			{
				currX=0;
				currY+=biggestHeight;
				biggestHeight=0;
			}
			
			rectNewPos[i]=[];
			rectNewPos[i][0]=currX;
			rectNewPos[i][1]=currY;

			currX+=graphRects[i][2]-graphRects[i][0]+10;
			biggestHeight=Math.max(biggestHeight,graphRects[i][3]-graphRects[i][1]+10);
		}

		for(var i=0;i< myAllNodes.length;i++ )
		{
			myAllNodes[i].x=rectNewPos[myAllNodes[i].graphNumber][0]+(myAllNodes[i].x-graphRects[myAllNodes[i].graphNumber][0]);
			myAllNodes[i].y=rectNewPos[myAllNodes[i].graphNumber][1]+(myAllNodes[i].y-graphRects[myAllNodes[i].graphNumber][1]);
		}
	}
	
	// Check if current line colides with any other line
	function GetLineColision(currLine, lines)
	{
		var colision = 0;
		
		for(var i = 0; i < lines.length; i++ )
		{
			if( (currLine.xEnd == lines[ i ].xEnd || currLine.xStart == lines[ i ].xEnd || currLine.xEnd == lines[ i ].xStart || currLine.xStart == lines[ i ].xStart)
				&& (currLine.yEnd == lines[ i ].yEnd || currLine.yStart == lines[ i ].yEnd || currLine.yEnd == lines[ i ].yStart || currLine.yStart == lines[ i ].yStart) )
			{
				continue;
			}

			if( ccw( currLine.xStart, currLine.yStart, currLine.xEnd, currLine.yEnd, lines[ i ].xStart, lines[ i ].yStart )
				!= ccw( currLine.xStart, currLine.yStart, currLine.xEnd, currLine.yEnd, lines[ i ].xEnd, lines[ i ].yEnd )
				&& ccw( lines[ i ].xStart, lines[ i ].yStart, lines[ i ].xEnd, lines[ i ].yEnd, currLine.xStart, currLine.yStart )
				!= ccw( lines[ i ].xStart, lines[ i ].yStart, lines[ i ].xEnd, lines[ i ].yEnd, currLine.xEnd, currLine.yEnd ) )
			{
				colision++;
			}
		}

		return colision;
	}


}
