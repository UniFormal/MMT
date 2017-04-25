var THEORY_GRAPH_OPTIONS = 
{
	physics: 
	{	
		
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
	"graphmeta":
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
