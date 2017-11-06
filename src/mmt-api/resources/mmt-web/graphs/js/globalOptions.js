var serverBaseURL = undefined;
var serverUrl = "/";

// URL for getting menu-entries in side-menu
var menuEntriesURL=serverUrl+":jgraph/menu?id=";

// URL parts for getting graphdata, construction looks like:
// graphDataURL + graphDataURLTypeParameterName + concreteTypeValue + "&" + graphDataURLDataParameterName + concreteGraphdataValue
var graphDataURL=serverUrl+":jgraph/json?";
// For Backend
var graphDataURLTypeParameterName = "key=";
var graphDataURLDataParameterName = "uri=";
// For TGView
var graphDataURLTypeParameterNameTGView = "type=";
var graphDataURLDataParameterNameTGView = "graphdata=";

// Colors to select for colorizing nodes in graph 
var colorizingNodesArray = ["#CCCCFF", "#FFFFCC", "#FFCC99", "#CCFFCC", "#DDDDDD", "#FFCCCC"];


// shapeProperties.useImageSize true
// shapeProperties.useBorderWithImage true
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
		physics:false,
		shapeProperties: 
		{
			useImageSize: true,  // only for image and circularImage shapes
			useBorderWithImage: true  // only for image shape
		}
	},
	edges: 
	{
		smooth: 
		{
			enabled: true,
			type: "straightCross",
			roundness: 0.3
		}
	}
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
		directed: true,
		smoothEdge: true
	},
	"modelinclude":
	{
		color:"black",
		colorHighlight:"black",
		colorHover:"black",
		dashes: false,
		circle:false,
		directed: false,
		smoothEdge: false
	},
	"meta":
	{
		color:"green",
		colorHighlight:"green",
		colorHover:"green",
		dashes: true,
		circle: true,
		directed: true,
		smoothEdge: true
	},
	"alignment":
	{
		color:"red",
		colorHighlight:"red",
		colorHover:"red",
		dashes: true,
		circle: false,
		directed: false,
		smoothEdge: true
	},
	"view":
	{
		color:"black",
		colorHighlight:"black",
		colorHover:"black",
		dashes: false,
		circle:false,
		directed: true,
		smoothEdge: true
	},
	"structure":
	{
		color:"#cccccc",
		colorHighlight:"#cccccc",
		colorHover:"#cccccc",
		dashes: true,
		circle:false,
		directed: true,
		smoothEdge: true
	}
};



var NODE_STYLES =
{
	"model":
	{
		shape: "square",
		color: "#DDDDDD",
		colorBorder: "#222222",
		colorHighlightBorder: "#444444",
		colorHighlight: "#EEEEEE",
		dashes: false
	},
	"theory":
	{
		shape: "circle",
		color: "#D2E5FF",
		colorBorder: "#2B7CE9",
		colorHighlightBorder: "#2B7CE9",
		colorHighlight: "#D2E5FF",
		dashes: false
	}
};


var GRAPH_TYPES =
[
	{
		id: "thgraph",
		menuText: "Theory Graph",
		tooltip: ""
	},
	{
		id: "pgraph",
		menuText: "Path Graph",
		tooltip: ""
	},
	{
		id: "docgraph",
		menuText: "Doc Graph",
		tooltip: ""
	},
	{
		id: "archivegraph",
		menuText: "Archive Graph",
		tooltip: ""
	},
	{
		id: "mpd",
		menuText: "Model Pathway Diagram",
		tooltip: "MPD Graph-Viewer"
	}
];

