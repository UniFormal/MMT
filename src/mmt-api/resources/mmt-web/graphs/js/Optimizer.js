var Constants=[];
Constants.DependencyWidth = 30;
Constants.DependencyHeight=30;

function HelperLine(xStart, yStart, xEnd, yEnd)
{
	this.xStart = xStart;
	this.yStart = yStart;
	this.xEnd = xEnd;
	this.yEnd = yEnd;
}

function Optimizer(nodes, edges)
{
	var overAllColision;
	var myAllNodes=nodes;
	var field=[];
	var myWidth=12000;
	var myHeight=12000;
	var countNodesInGraph;
	var edgesCount=edges.length;
	
	mapEdgesIntoNodes(edges);
	identifySubgraphs();
	
	function identifySubgraphs()
	{
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
	
	function mapEdgesIntoNodes(edges)
	{
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
	
	this.GenerateRandomSolution = function()
	{
		lines = [];
		var i = 0;
		
		for(var i=0;i< myAllNodes.length;i++ )
		{
			InsertNodeAtGoodPosition( myAllNodes[i], lines);
		}
	}

	function UpdateFieldColision( x,  y,  value = 1 )
	{
		field[ y * myWidth + x ] = value;
	}
	
	function InsertNodeAtGoodPosition(n, lines, iterations = 20 )
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

	this.SolveUsingForces = function(iterations, usingMinMax = false, currTemperature = 0.9, initialStep = 30.0 )
	{
		var energy = 1000000;
		var step = initialStep;
		var success = 0;

		var area = myWidth * myHeight;
		var kVal =  Math.max(Math.min((myAllNodes.length*4+edgesCount/2.5)/2 * 0.5,280),100);
		var kSquared = kVal * kVal;

		for( var i = 0; i < iterations; i++ )
		{
			var energyBefore = energy;
			energy = 0;
			for( var j = 0; j < myAllNodes.length; j++ )
			{
				var n=myAllNodes[j];
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
