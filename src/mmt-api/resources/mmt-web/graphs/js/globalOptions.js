var THEORY_GRAPH_OPTIONS = 
{
	physics: 
	{	
		//enabled: false,
		solver: 'barnesHut',			
		"barnesHut": 
		{
			"avoidOverlap": 0.1
		},
		stabilization: 
		{
			enabled: true,
			iterations: 10 // maximum number of iteration to stabilize
		}
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