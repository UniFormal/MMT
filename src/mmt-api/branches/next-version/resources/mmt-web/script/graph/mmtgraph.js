//Mihaela Rusu - Guided Research 2011

var labelType, useGradients, nativeTextSupport, animate, multipleURI;

//From the demo. Detects the possibilities of the canvas
(function() {
    var ua = navigator.userAgent,
        iStuff = ua.match(/iPhone/i) || ua.match(/iPad/i),
        typeOfCanvas = typeof HTMLCanvasElement,
        nativeCanvasSupport = (typeOfCanvas == 'object' || typeOfCanvas == 'function'),
        textSupport = nativeCanvasSupport && (typeof document.createElement('canvas').getContext('2d').fillText == 'function');
    //I'm setting this based on the fact that ExCanvas provides text support for IE
    //and that as of today iPhone/iPad current text support is lame
    labelType = (!nativeCanvasSupport || (textSupport && !iStuff))? 'Native' : 'HTML';
    nativeTextSupport = labelType == 'Native';
    useGradients = nativeCanvasSupport;
    animate = !(iStuff || !nativeCanvasSupport);
})();

// notation style, null if none
var notstyle = 'http://cds.omdoc.org/foundations/lf/mathml.omdoc?twelf';  // hard-coding a default style for LF content

/**
 * adaptMMTURI - convert MMTURI to URL using current catalog and possibly notation style
 * act: String: action to call on MMTURI
 * present: Boolean: add presentation to action
 */
function adaptMMTURI(uri, act, present){
	var arr = uri.split("?");
	var doc = (arr.length >= 1) ? arr[0] : "";
	var mod = (arr.length >= 2) ? arr[1] : "";
	var sym = (arr.length >= 3) ? arr[2] : "";
	if (present && notstyle !== null)
	   var pres = "_present_" + notstyle;
	else
	   var pres = '';
	return '/:mmt?' + doc + '?' + mod + '?' + sym + '?' + act + pres;
}
function ajaxReplaceIn(url, targetid) {
	function cont(data) {
         var targetnode = $('#' + targetid).children('div');
         targetnode.replaceWith(data.firstChild);
	}
	$.ajax({ 'url': url,
            'dataType': 'xml',
            'success': cont
	});
}

function latin_navigate(uri) {
		var url = adaptMMTURI(uri, '', true);
		ajaxReplaceIn(url, 'theory_content');
}

//From the demo. Defines logging procedure
var Log = {
    elem: false,
    write: function(text){
        if (!this.elem) 
            this.elem = document.getElementById('log');
        this.elem.innerHTML = text;
        this.elem.style.left = (400 - this.elem.offsetWidth / 2) + 'px';
    }
};

function initGraph(json) {
	config.json = json;
	config.orig_json = json;
    //Extending the edge types
    $jit.ForceDirected.Plot.EdgeTypes.implement({
		//CD
		// should be re-tested.
        'double_arrow': {
            'render': function(adj, canvas) {
                var from = adj.nodeFrom.pos.getc(true),
                    to = adj.nodeTo.pos.getc(true),
                    dim = adj.getData('dim'),
                    ctx = canvas.getCtx(),
                    vect = new $jit.Complex(to.x - from.x, to.y - from.y);
                vect.$scale(dim / vect.norm());
                // Needed for drawing the first arrow
                var intermediatePoint = new $jit.Complex(to.x - vect.x, to.y - vect.y),
                    normal = new $jit.Complex(-vect.y / 2, vect.x / 2),
                    v1 = intermediatePoint.add(normal), 
                    v2 = intermediatePoint.$add(normal.$scale(-1));
                    
                var vect2 = new $jit.Complex(to.x - from.x, to.y - from.y);
                vect2.$scale(dim / vect2.norm());
                // Needed for drawing the second arrow
                var intermediatePoint2 = new $jit.Complex(from.x + vect2.x, from.y + vect2.y),
                    normal = new $jit.Complex(-vect2.y / 2, vect2.x / 2),
                    v12 = intermediatePoint2.add(normal), 
                    v22 = intermediatePoint2.$add(normal.$scale(-1));
                
                // Drawing the double arrow on the canvas, first the line, then the ends
                ctx.beginPath();
                ctx.moveTo(from.x, from.y);
                ctx.lineTo(to.x, to.y);
                ctx.stroke();
                ctx.beginPath();
                ctx.moveTo(v1.x, v1.y);
                ctx.lineTo(v2.x, v2.y);
                ctx.lineTo(to.x, to.y);
                ctx.closePath();
                ctx.fill();
                ctx.beginPath();
                ctx.moveTo(v12.x, v12.y);
                ctx.lineTo(v22.x, v22.y);
                ctx.lineTo(from.x, from.y);
                ctx.closePath();
                ctx.fill();
                // Check for edge label in data
                var data = adj.data.uri;
                if (data) {
                    // Adjust the label placement
                    var elabel = data.slice(data.lastIndexOf("?") + 1); 
                    var radius = this.viz.canvas.getSize();
                    var x = parseInt((from.x + to.x - (elabel.length * 5)) /2);
                    var y = parseInt((from.y + to.y ) /2);
                    this.viz.canvas.getCtx().fillText(elabel, x, y); 
                }
            },
            'contains': function(adj, pos) { //Returns true if the position is inside the edge (a small threshold is considered)
                var posFrom = adj.nodeFrom.pos.getc(true),
                    posTo = adj.nodeTo.pos.getc(true),
                    min = Math.min, 
                    max = Math.max,
                    minPosX = min(posFrom.x, posTo.x),
                    maxPosX = max(posFrom.x, posTo.x),
                    minPosY = min(posFrom.y, posTo.y),
                    maxPosY = max(posFrom.y, posTo.y);
            
                if (pos.x >= minPosX && pos.x <= maxPosX && pos.y >= minPosY && pos.y <= maxPosY) {
                    if (Math.abs(posTo.x - posFrom.x) <= this.edge.epsilon) 
                        return true;
                    var dist = (posTo.y - posFrom.y) / (posTo.x - posFrom.x) * (pos.x - posFrom.x) + posFrom.y;
                        return Math.abs(dist - pos.y) <= this.edge.epsilon;
                }
                return false;   
            }
        },
        'self_arrow': {
            'render': function(adj, canvas) {
                var from = adj.nodeFrom.pos.getc(true),
                    dim = adj.getData('dim'),
                    data = adj.data,
                    ctx = canvas.getCtx(),
                    dist = 1; //Depends on the graph size. Might be decreased further
                var vect = new $jit.Complex(20*dist, 20*dist);
                vect.$scale(dim / vect.norm());
                // Empirically - this is the point from where the arrow end will be computed
                var intermediatePoint = new $jit.Complex(from.x + 24*dist - vect.x, from.y + 19*dist - vect.y), 
                    normal = new $jit.Complex(-vect.y / 2, vect.x / 2),
                    v1 = intermediatePoint.add(normal), 
                    v2 = intermediatePoint.$add(normal.$scale(-1));   
                // The drawing procedure - basically a circle with an arrow at the connection to the center of the node     
                ctx.beginPath();
                ctx.arc(from.x, from.y + 20*dist, dist*20, 0, 2*Math.PI, true);
                ctx.stroke();
                ctx.beginPath();
                ctx.moveTo(v1.x, v1.y);
                ctx.lineTo(v2.x, v2.y);
                ctx.lineTo(from.x, from.y);
                ctx.closePath();
                ctx.fill();
                // Checking for the label and drawing it
                var data = adj.data.uri;
                if (data) {
                    // Now adjust the label placement
                    var elabel = data.slice(data.lastIndexOf("?") + 1); 
                    var radius = this.viz.canvas.getSize();
                    var x = parseInt(from.x - (elabel.length * 3));
                    var y = parseInt(from.y + 40*dist);
                    this.viz.canvas.getCtx().fillText(elabel, x, y); 
                }
            },
            'contains': function(adj, pos) { //Returns true if the position is inside the edge (a small threshold is considered)
                var posFrom = adj.nodeFrom.pos.getc(true),
                    dist = 1,
                    r = Math.sqrt((pos.x - posFrom.x)*(pos.x - posFrom.x) + (pos.y - posFrom.y - 20*dist)*(pos.y - posFrom.y - 20*dist));
                if (Math.abs(dist*20 - r) <= this.edge.epsilon)
                    return true;
                return false;
            }
        },
        'multiple_arrow': {
            'render': function(adj, canvas) {
                var fromIn = adj.nodeFrom.pos.getc(true),
                    toIn = adj.nodeTo.pos.getc(true),
                    dim = adj.getData('dim'),
                    direction = adj.data.$direction,
                    ctx = canvas.getCtx(),
                    dist = 0; //The distance of the control point to the middle of the line from "fromIn" to "toIn"					
                //for (e in direction) {
                    dist*=-1;
                    if (dist <= 0)
                        dist-=2;  
                    
                    //Inversing the direction of the arrow when necessary
                    var from, to; 
                    var inv = (direction.from != adj.nodeFrom.id);
                    if (inv) {
                        from = toIn;
                        to = fromIn; 
                    } else {
                        from = fromIn;
                        to = toIn;
                    }
                    //Computing the exact position of the control point
                    var slope = (from.x - to.x)/(to.y - from.y),
                        mid = new $jit.Complex((from.x + to.x)/2, (from.y + to.y)/2),
                        d = mid.y - slope*mid.x,
                        a = slope * slope + 1,
                        b = 2 * (slope * d - mid.x - mid.y * slope),
                        c = mid.x * mid.x + mid.y * mid.y - 2 * mid.y * d + d * d - dist * dist * 200,
                        tmpx = 0;
                    if (dist > 0) {
                        tmpx = (-b + Math.sqrt(b * b - 4 * a *c))/(2*a);
                    } else {
                        tmpx = (-b - Math.sqrt(b * b - 4 * a *c))/(2*a);
                    }
                    
                    var control = new $jit.Complex(tmpx, tmpx * slope + d);
                    
                    var vect = new $jit.Complex(to.x - control.x, to.y - control.y);
                    vect.$scale(dim / vect.norm());
                    //Used for the arrow tip
                    var intermediatePoint = new $jit.Complex(to.x - vect.x, to.y - vect.y),
                        normal = new $jit.Complex(-vect.y / 3, vect.x / 3),
                        v1 = intermediatePoint.add(normal), 
                        v2 = intermediatePoint.$add(normal.$scale(-1));
                    //Drawing one edge - each edge is a quadratic curve defined by endpoints and the control point
                    ctx.beginPath();
                    ctx.moveTo(from.x, from.y);
                    ctx.quadraticCurveTo(control.x, control.y, to.x, to.y);
                    ctx.stroke();
                    ctx.beginPath();
                    ctx.moveTo(v1.x, v1.y);
                    ctx.lineTo(v2.x, v2.y);
                    ctx.lineTo(to.x, to.y);
                    ctx.closePath();
                    ctx.fill();
                    var data = direction.uri;
                    //Checking for the label and drawing it
                    if (data) {
                        //Now adjust the label placement
                        var elabel = data.slice(data.lastIndexOf("?") + 1); 
                        var radius = this.viz.canvas.getSize();
                        var x = parseInt(control.x  - (elabel.length));
                        var y = parseInt(control.y);
                        this.viz.canvas.getCtx().fillText(elabel, x, y); 
                    }
                //}
            },
            'contains': function(adj, pos) { //Trading precision for speed - checking for the lines connecting the control point with the end points
                var from = adj.nodeFrom.pos.getc(true),
                    to = adj.nodeTo.pos.getc(true),
                    direction = adj.data.$direction,
                    dist = 0,
                    slope = (from.x - to.x)/(to.y - from.y),
                    mid = new $jit.Complex((from.x + to.x)/2, (from.y + to.y)/2),
                    d = mid.y - slope*mid.x,
                    a = slope * slope + 1,
                    b = 2 * (slope * d - mid.x - mid.y * slope);
                for (var i = 0; i < direction.length; i++) {
                    dist*=-1;
                    if (dist <= 0)
                        dist-=2;  

                    var c = mid.x * mid.x + mid.y * mid.y - 2 * mid.y * d + d * d - dist * dist * 200,
                        tmpx = 0;
                    if (dist > 0) {
                        tmpx = (-b + Math.sqrt(b * b - 4 * a *c))/(2*a);
                    } else {
                        tmpx = (-b - Math.sqrt(b * b - 4 * a *c))/(2*a);
                    }
                    
                    var control = new $jit.Complex(tmpx, tmpx * slope + d);
                    
                    var min = Math.min, 
                        max = Math.max,
                        minPosX = min(from.x, control.x),
                        maxPosX = max(from.x, control.x),
                        minPosY = min(from.y, control.y),
                        maxPosY = max(from.y, control.y);
                    
                    if (pos.x >= minPosX && pos.x <= maxPosX && pos.y >= minPosY && pos.y <= maxPosY) {
                        if(Math.abs(control.x - from.x) <= this.edge.epsilon) {
                            multipleURI = direction[i].uri;
                            return true;
                        }
                        var dist = (control.y - from.y) / (control.x - from.x) * (pos.x - from.x) + from.y;
                        if (Math.abs(dist - pos.y) <= this.edge.epsilon) {
                            multipleURI = direction[i].uri;
                            return true;
                        }
                    }
                    
                        minPosX = min(to.x, control.x);
                        maxPosX = max(to.x, control.x);
                        minPosY = min(to.y, control.y);
                        maxPosY = max(to.y, control.y);
                    
                    if (pos.x >= minPosX && pos.x <= maxPosX && pos.y >= minPosY && pos.y <= maxPosY) {
                        if(Math.abs(control.x - to.x) <= this.edge.epsilon) {
                            multipleURI = direction[i].uri;
                            return true;
                        }
                        var dist = (to.y - control.y) / (to.x - control.x) * (pos.x - control.x) + control.y;
                        if (Math.abs(dist - pos.y) <= this.edge.epsilon) {
                            multipleURI = direction[i].uri;
                            return true;
                        }
                    }
                }
                    return false;   
                    
            }
        },
        'labeled_arrow': {
			//CD
			//should be re-tested.
            'render': function(adj, canvas) {
                //Plot arrow edge
                var from = adj.nodeFrom.pos.getc(true),
                    to = adj.nodeTo.pos.getc(true),
                    dim = adj.getData('dim'),
                    ctx = canvas.getCtx(),
                    vect = new $jit.Complex(to.x - from.x, to.y - from.y);
                vect.$scale(dim / vect.norm());
                //Needed for drawing the arrow tip
                var intermediatePoint = new $jit.Complex(to.x - vect.x, to.y - vect.y),
                    normal = new $jit.Complex(-vect.y / 2, vect.x / 2),
                    v1 = intermediatePoint.add(normal), 
                    v2 = intermediatePoint.$add(normal.$scale(-1)); 
                
                ctx.beginPath();
                ctx.moveTo(from.x, from.y);
                ctx.lineTo(to.x, to.y);
                ctx.stroke();
                ctx.beginPath();
                ctx.moveTo(v1.x, v1.y);
                ctx.lineTo(v2.x, v2.y);
                ctx.lineTo(to.x, to.y);
                ctx.closePath();
                ctx.fill();
                //Check for edge label in data
                var data = adj.data.uri;
                if (data) {
                    //Adjust the label placement
                    var elabel = data.slice(data.lastIndexOf("?") + 1); 
                    var radius = this.viz.canvas.getSize();
                    var x = parseInt((from.x + to.x - (elabel.length * 5)) /2);
                    var y = parseInt((from.y + to.y ) /2);
                    this.viz.canvas.getCtx().fillText(elabel, x, y); 
                }
            },
            'contains': function(adj, pos) { //Returns true if the position is inside the edge (a small threshold is considered)
                var posFrom = adj.nodeFrom.pos.getc(true),
                    posTo = adj.nodeTo.pos.getc(true),
                    min = Math.min, 
                    max = Math.max,
                    minPosX = min(posFrom.x, posTo.x),
                    maxPosX = max(posFrom.x, posTo.x),
                    minPosY = min(posFrom.y, posTo.y),
                    maxPosY = max(posFrom.y, posTo.y);
                
                if (pos.x >= minPosX && pos.x <= maxPosX && pos.y >= minPosY && pos.y <= maxPosY) {
                    if (Math.abs(posTo.x - posFrom.x) <= this.edge.epsilon) 
                        return true;
                    var dist = (posTo.y - posFrom.y) / (posTo.x - posFrom.x) * (pos.x - posFrom.x) + posFrom.y;
                    return (Math.abs(dist - pos.y) <= this.edge.epsilon);
                }
                return false;   
            }
        }    
    });
	
    //Initialize the ForceDirected object
    config.graph = new $jit.ForceDirected({
        //Id of the visualization container
        injectInto: 'graph',
        width: 800,
		height:600,
        //Enable zooming and panning by scrolling and DnD
        Navigation: {
            enable: true,
			type: 'Native',
            //Enable panning events only if we're dragging the empty canvas (and not a node).
            panning: 'avoid nodes',
            zooming: 80 //zoom speed. higher is more sensible
        },
        //Change node and edge styles such as color and width.
        //These properties are also set per node with dollar prefixed data-properties in the JSON structure.
        Node: {
            overridable: true,
			type: 'ellipse',
			color: '#ffaa00',
			height: 15,
			width: 15,
        },
        Edge: {
            overridable: true,
            color: '#0000FF',
            lineWidth: 1,
        },
        //Add Tips
        Tips: {
            enable: false,
            onShow: function(tip, node) {
                //count connections
                var count = 0;
                node.eachAdjacency(function() { count++; });
                //display node info in tooltip
                tip.innerHTML = "<div class=\"tip-title\">" + node.name + "</div>";
            }
        },
        // Add node events
        Events: {
            enable: true,
            //Allows the same handlers to be used for edges
            enableForEdges: true,
			type: 'Native',
            //Change cursor style when hovering a node
            onMouseEnter: function(node, eventInfo, e) {
                config.graph.canvas.getElement().style.cursor = 'move';				
            },
            onMouseLeave: function() {
                config.graph.canvas.getElement().style.cursor = '';
            },
            //Update node positions when dragged
            onDragMove: function(node, eventInfo, e) {
                var pos = eventInfo.getPos();
				if (node.pos) {
					node.pos.setc(pos.x, pos.y);
				}
                config.graph.plot();
            },
            //Implement the same handler for touchscreens
            onTouchMove: function(node, eventInfo, e) {
                $jit.util.event.stop(e); //stop default touchmove event
                this.onDragMove(node, eventInfo, e);
            },
            //Add also a click handler to nodes 
            onClick: function(node) {
                if (!node) return;
                // Build the right column relations list.
                // This is done by traversing the clicked node connections.
                var data = node.data;
                if (data.$type == 'multiple_arrow') {
                    var msg = multipleURI;
				}
                else {
                    msg = data.uri;
				}
				
				if (config.modes.collapse == 1) {
					$('#' + config.graph.canvas.id).trigger("collapse", {
						"node" : node
					});
				}
				if (config.modes.expand == 1) {
					$('#' + config.graph.canvas.id).trigger("expand", {
						"node" : node
					});
				}
            },
            //Handler for collapsing and expanding nodes - needs special behaviour for multiedges
            onRightClick: function(node) {
                if (node)
                    if (node.collapsed)
                        config.graph.op.expand(node, {  
                            type: 'replot',  
                            hideLabels: true                     
                        }); 
                    else
                        config.graph.op.contract(node, {  
                            type: 'replot',  
                            hideLabels: true                     
                        });     
            }
        },
        //Number of iterations for the FD algorithm
        iterations: 200,
        //Edge length
        levelDistance: 130,
        //Add text to the labels. This method is only triggered on label creation and only for DOM labels (not native canvas ones).
        onCreateLabel: function(domElement, node){
			var s1 = $('<span></span>').attr('class','name').html(node.name).appendTo(domElement);
			$(s1).click(	function() {
				config.graph.graph.eachNode(function(n) {
					if (n.id != node.id) {
						delete n.selected;
					}
					n.setData('dim', 7, 'end');
					n.eachAdjacency(function(adj) {
						adj.setDataset('end', {
							lineWidth: 1
						});
					});
				});
				if (!node.selected) {
					node.selected = true;
					node.setData('dim', 17, 'end');
					node.eachAdjacency(function(adj) {
						adj.setDataset('end', {
							lineWidth: 5,
						});
					});
				} else {
					delete node.selected;
				}
				config.graph.fx.animate({
					modes: ['node-property:dim',
						'edge-property:lineWidth'],
					duration: 500
				});
				var html = "<h4>" + node.name + "</h4><b> connections:</b><ul><li>";
				var list = [];
				node.eachAdjacency(function(adj){
					if (adj.getData('alpha')) list.push(adj.nodeTo.name);
				});
				$('#adjacencies').html(html + list.join("</li><li>") + "</li></ul>");
				latin_navigate(node.id);
			});
        },
        //Change node styles when DOM labels are placed or moved.
        onPlaceLabel: function(domElement, node){
            var style = domElement.style;
            var left = parseInt(style.left);
            var top = parseInt(style.top);
            var w = domElement.offsetWidth;
            style.left = (left - w / 2) + 'px';
            style.top = (top + 10) + 'px';
            style.display = '';
        },		
    });
    
    //Load JSON data.
    config.graph.loadJSON(config.json);
    //Compute positions incrementally and animate.
    config.graph.computeIncremental({
        iter: 10,
        property: 'end',
        onStep: function(perc){
            Log.write(perc + '% loaded...');
        },
        onComplete: function(){
            Log.write('done');
            config.graph.animate({
                modes: ['linear'],
                //For a nicer effect - transition: $jit.Trans.Elastic.easeOut,
                transition: $jit.Trans.Elastic.easeOut,
                duration: 1000
            });
			init_command();
        }
    });	
}

function init_command() {
	//helper to clean the menu
	var reinit_menu = function() {
		config.modes = {};
		$('#command ul li span').attr('style', '');
	}
	
	//helper to highlight a menu item
	var highlight_menu = function(target) {
		$(target).css('background', 'green');
	}
	
	//helper refresh
	var refresh = function() {
		if (!config.graph) return;
		config.graph.op.morph(config.graph.toJSON("graph"), {type: 'fade', duration: 1000});
		reinit_menu();
	}
	
	//menu constructor
	$('#command .header').click(function() {
		$(this).next().toggle('slow');
		return false;
	}).next().show();
	
	//menu button refresh
	$('#command .submenu .refresh').click(function() {
		refresh();
	});
	
	//menu button collapse
	$('#command .submenu .collapse').click(function() {
		reinit_menu();	
		config.modes.collapse = 1;
		highlight_menu(this);
	});	
	$('#' + config.graph.canvas.id).bind("collapse", function(e,data) {		
		if (!data.node && data.node.collapsed) {
			config.modes.collapse = 0;
			return;
		}
		config.graph.op.contract(data.node, {
			type : 'replot',
			hideLabels : true,
		});
		reinit_menu();
	});
	
	//menu button expand
	$('#command .submenu .expand').click(function() {
		reinit_menu();
		config.modes.expand = 1;
		highlight_menu(this);
	});	
	$('#' + config.graph.canvas.id).bind("expand", function(e,data) {	
		if (!data.node) {
			config.modes.expand = 0;
			return;
		}
		config.graph.op.expand(data.node, {
			type : 'replot',
			hideLabels : true,			
		});
		reinit_menu();
	});
	
	//show external nodes
	$('#edgetype0').click(function() {
		var ref = false;
		var json = config.json;
		if (config.settings.external == true) {
			if (!config || !config.graph || !config.graph.graph || !config.graph.graph.nodes)
				return;
			config.graph.graph.eachNode( function(node) {
				if (node.name && node.id && node.name == node.id) {
					//maybe a better heuristics is needed
					node.collapsed = true;
					node.ignore = true;
					node.setData('alpha', 0, 'current');
					ref = true;
				}
			});
			if (ref) {
				refresh();
			}
			config.settings.external = false;
		} else {
			if (config.settings.external == false) {
				//go through the initial JSON
				//build a list of nodes
				//then inspect the edges
				//build a list of nodes referenced by the edges, but not in list of nodes
				//add nodes { id: "", name: "", data: {}}
				//add edges
				var lon = {}; //list of nodes				
				var lon2 = {}; //list of nodes to be added
				var edg = [];
				if (!config.json) return;
				config.graph.graph.eachNode( function(node) {
					if (node.id) {
						lon[node.id] = node.name;
					}
				});
				$.each(config.orig_json, function(i,v) {
					if (v.adjacencies) {
						$.each(v.adjacencies, function(j,t) {
							if (!t.nodeFrom || !t.nodeTo) return;
							if (!lon[t.nodeFrom] || !lon[t.nodeTo]) {
								if (!lon[t.nodeFrom]) {
									//?? something should be wrong now
									lon2[t.nodeFrom] = "";								
								}
								if (!lon[t.nodeTo]) {
									lon2[t.nodeTo] = "";
								}
								var e = {};
								e.nodeFrom = t.nodeFrom;
								e.nodeTo = t.nodeTo;
								e.data = t.data;								
								edg.push(e);
							}
						});
					}
				});
				//add the nodes
				$.each(lon2, function(i,v) {					
					var t = {};
					t.id = i;
					t.name = i;
					t.data = {};
					config.graph.graph.addNode(t);
					ref = true;
				});
				config.graph.op.morph(config.graph.toJSON("graph"), {type: 'fade', duration: 1000});
				//add the edges
				$.each(edg, function(i,v) {
					if (!config.graph.graph || !config.graph.graph.getNode) return;
					var n1 = config.graph.graph.getNode(v.nodeFrom);
					var n2 = config.graph.graph.getNode(v.nodeTo);
					var t = v.data;				
					if (!n1 || !n2 || !t) return;
					config.graph.graph.addAdjacence(n1, n2, t);
					config.graph.graph.getAdjacence(n1.id, n2.id).setData('alpha', 255);
					ref = true;
				});
				
				if (ref) {
					refresh();
				}
				config.settings.external = true;
			}
		}
	});
	
	var toggleEdge = function(type) {
		var ref = false;
		config.graph.graph.eachNode(function(node) {
			node.eachAdjacency(function(e) {
				if (e.data.$direction.kind.toUpperCase() == type.toUpperCase()) {
					e.setData('alpha', !config.settings[type.toLowerCase()] ? 1 : 0);
					e.setData('ignore', !config.settings[type.toLowerCase()] ? 1 : 0);
					ref = true;
				}
			});
		});
		
		if (ref) {
			refresh();
		}
		config.settings[type.toLowerCase()] = !config.settings[type.toLowerCase()];
	}
	
	$('#edgetype1').click(function() {
		if (!config.graph || !config.graph.graph) return;
		toggleEdge('Include');
	});
	
	$('#edgetype2').click(function() {
		if (!config.graph || !config.graph.graph) return;
		toggleEdge('Meta');
	});
	
	$('#edgetype3').click(function() {
		if (!config.graph || !config.graph.graph) return;
		toggleEdge('Structure');
	});
	
	$('#edgetype4').click(function() {
		if (!config.graph || !config.graph.graph) return;
		toggleEdge('View');
	});
}

function loadAndInitGraph() {
   $.getJSON("/:graph", function(json){initGraph(json);});   
}

//catalin
var config = {
	settings: {
		external : true,
		include : true,
		meta : true,
		structure : true,
		view : true
	},
	json : null,
	orig_json :  null,//the initial JSON -- do not play with it :)
	graph : null,
	modes : {}
};

var asd = null;