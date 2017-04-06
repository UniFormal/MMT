var THEORY_GRAPH_OPTIONS = 
{
	physics: 
	{	
		enabled: false,
		solver: 'forceAtlas2Based',			
		forceAtlas2Based: 
		{
			gravitationalConstant: -25,
			centralGravity: 0.01,
			springConstant: 0.08,
			springLength: 200,
			damping: 0.4,
			avoidOverlap: 1.0
		},
		stabilization: 
		{
			enabled: true,
			iterations: 10, // maximum number of iteration to stabilize
			updateInterval: 10,
			onlyDynamicEdges: false,
		}
	},
	edges: {smooth: false},
	layout: 
	{
		hierarchical: 
		{
			sortMethod: "directed",
			direction: "LR"
		}
	}
};

var ARROW_STYLES=
{
	"graphinclude":
	{
		color:"#cccccc",
		colorHighlight:"#cccccc",
		colorHover:"#cccccc",
		dashes: false,
		circle:false,
		directed: true
	},
	"graphtheory":
	{
		color:"red",
		colorHighlight:"red",
		colorHover:"red",
		dashes: false,
		circle:false,
		directed: true
	},
	"graphview":
	{
		color:"black",
		colorHighlight:"black",
		colorHover:"black",
		dashes: false,
		circle:false,
		directed: true
	},
	"graphstructure":
	{
		color:"#cccccc",
		colorHighlight:"#cccccc",
		colorHover:"#cccccc",
		dashes: true,
		circle:false,
		directed: true
	}
};
