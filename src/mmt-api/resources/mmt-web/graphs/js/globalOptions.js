var THEORY_GRAPH_OPTIONS = 
{
	physics: 
	{	
		enabled: false,
		stabilization: true,
		solver: 'barnesHut',			
		"barnesHut": 
		{
			"avoidOverlap": 1
		},
		stabilization: 
		{
			enabled: true,
			iterations: 5			// maximum number of iteration to stabilize
		}
	},
	interaction:
	{
		multiselect: true
	},
	nodes: 
	{
		physics:false
	},
	edges: {smooth: true}
	/*layout: 
	{
		hierarchical: 
		{
			sortMethod: "directed",
			direction: "LR"
		}
	}*/
};

var ARROW_STYLES=
{
	"include":
	{
		color:"#cccccc",
		colorHighlight:"#cccccc",
		colorHover:"#cccccc",
		dashes: false,
		circle:false,
		directed: true
	},
	"meta":
	{
		color:"green",
		colorHighlight:"green",
		colorHover:"green",
		dashes: true,
		circle: true,
		directed: true
	},
	"alignment":
	{
		color:"red",
		colorHighlight:"red",
		colorHover:"red",
		dashes: true,
		circle: false,
		directed: false
	},
	"view":
	{
		color:"black",
		colorHighlight:"black",
		colorHover:"black",
		dashes: false,
		circle:false,
		directed: true
	},
	"structure":
	{
		color:"#cccccc",
		colorHighlight:"#cccccc",
		colorHover:"#cccccc",
		dashes: true,
		circle:false,
		directed: true
	}
};

var NODE_STYLES =
{
	"model":
	{
		shape: "square"
	},
	"theory":
	{
		shape: "circle"
	}
};
