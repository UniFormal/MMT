var serverBaseURL = undefined;
var serverUrl = (typeof serverBaseURL == "undefined" || serverBaseURL==undefined) ? ((window.location.protocol=='file:')? "/" : "/mh/mmt/") : serverBaseURL;
if (location.hostname === "localhost" || location.hostname === "127.0.0.1" || location.hostname === "")
	serverUrl="/";

// URL for getting menu-entries in side-menu
var menuEntriesURL=serverUrl+":jgraph/menu?id=";

// URL parts for getting graphdata, construction looks like:
// graphDataURL + graphDataURLTypeParameterName + concreteTypeValue + "&" + graphDataURLDataParameterName + concreteGraphdataValue
var graphDataURL=serverUrl+":jgraph/json?";
var graphDataURLTypeParameterName = "key=";
var graphDataURLDataParameterName = "uri=";

// Colors to select for colorizing nodes in graph 
var colorizingNodesArray = ["#CCCCFF", "#FFFFCC", "#FFCC99", "#CCFFCC", "#DDDDDD", "#FFCCCC"];


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


var GRAPH_TYPES =
[
	{
		id: "thgraph",
		menuText: "Th. Graph",
		tooltip: ""
	},
	{
		id: "pgraph",
		menuText: "P Graph",
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
		menuText: "MPD Graph",
		tooltip: "MPD Graph-Viewer"
	}
];

