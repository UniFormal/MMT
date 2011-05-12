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

//From the demo. Defines logging procedure
var Log = {
    elem: false,
    write: function(text){
        if (!this.elem) 
            this.elem = document.getElementById('log');
        this.elem.innerHTML = text;
        this.elem.style.left = (500 - this.elem.offsetWidth / 2) + 'px';
    }
};

function initGraph(json) {
    //Extending the edge types
    $jit.ForceDirected.Plot.EdgeTypes.implement({
        'double_arrow': {
            'render': function(adj, canvas) {
                var from = adj.nodeFrom.pos.getc(true),
                    to = adj.nodeTo.pos.getc(true),
                    dim = adj.getData('dim'),
                    ctx = canvas.getCtx(),
                    vect = new $jit.Complex(to.x - from.x, to.y - from.y);
                vect.$scale(dim / vect.norm());
                //Needed for drawing the first arrow
                var intermediatePoint = new $jit.Complex(to.x - vect.x, to.y - vect.y),
                    normal = new $jit.Complex(-vect.y / 2, vect.x / 2),
                    v1 = intermediatePoint.add(normal), 
                    v2 = intermediatePoint.$add(normal.$scale(-1));
                    
                var vect2 = new $jit.Complex(to.x - from.x, to.y - from.y);
                vect2.$scale(dim / vect2.norm());
                //Needed for drawing the second arrow
                var intermediatePoint2 = new $jit.Complex(from.x + vect2.x, from.y + vect2.y),
                    normal = new $jit.Complex(-vect2.y / 2, vect2.x / 2),
                    v12 = intermediatePoint2.add(normal), 
                    v22 = intermediatePoint2.$add(normal.$scale(-1));
                
                //Drawing the double arrow on the canvas, first the line, then the ends
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
                //Empirically - this is the point from where the arrow end will be computed
                var intermediatePoint = new $jit.Complex(from.x + 24*dist - vect.x, from.y + 19*dist - vect.y), 
                    normal = new $jit.Complex(-vect.y / 2, vect.x / 2),
                    v1 = intermediatePoint.add(normal), 
                    v2 = intermediatePoint.$add(normal.$scale(-1));   
                //The drawing procedure - basically a circle with an arrow at the connection to the center of the node     
                ctx.beginPath();
                ctx.arc(from.x, from.y + 20*dist, dist*20, 0, 2*Math.PI, true);
                ctx.stroke();
                ctx.beginPath();
                ctx.moveTo(v1.x, v1.y);
                ctx.lineTo(v2.x, v2.y);
                ctx.lineTo(from.x, from.y);
                ctx.closePath();
                ctx.fill();
                //Checking for the label and drawing it
                var data = adj.data.uri;
                if (data) {
                    //Now adjust the label placement
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
                for (e in direction) {
                    dist*=-1;
                    if (dist <= 0)
                        dist-=2;  
                    
                    //Inversing the direction of the arrow when necessary
                    var from, to; 
                    var inv = (direction[e].from != adj.nodeFrom.id);
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
                        normal = new $jit.Complex(-vect.y / 2, vect.x / 2),
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
                    var data = direction[e].uri;
                    //Checking for the label and drawing it
                    if (data) {
                        //Now adjust the label placement
                        var elabel = data.slice(data.lastIndexOf("?") + 1); 
                        var radius = this.viz.canvas.getSize();
                        var x = parseInt(control.x  - (elabel.length));
                        var y = parseInt(control.y);
                        this.viz.canvas.getCtx().fillText(elabel, x, y); 
                    }
                }
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
    var fd = new $jit.ForceDirected({
        //Id of the visualization container
        injectInto: 'graph',
        //Enable zooming and panning by scrolling and DnD
        Navigation: {
            enable: true,
            //Enable panning events only if we're dragging the empty canvas (and not a node).
            panning: 'avoid nodes',
            zooming: 10 //zoom speed. higher is more sensible
        },
        //Change node and edge styles such as color and width.
        //These properties are also set per node with dollar prefixed data-properties in the JSON structure.
        Node: {
            overridable: true,
            color: '#FF3333'
        },
        Edge: {
            overridable: true,
            color: '#23A4FF',
            lineWidth: 0.4
        },
        //Native canvas text styling
        Label: {
            type: labelType, //Native or HTML
            size: 10,
            style: 'bold'
        },
        //Add Tips
        Tips: {
            enable: true,
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
            //Change cursor style when hovering a node
            onMouseEnter: function() {
                fd.canvas.getElement().style.cursor = 'move';
            },
            onMouseLeave: function() {
                fd.canvas.getElement().style.cursor = '';
            },
            //Update node positions when dragged
            onDragMove: function(node, eventInfo, e) {
                var pos = eventInfo.getPos();
                node.pos.setc(pos.x, pos.y);
                fd.plot();
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
                if (data.$type == 'multiple_arrow')
                    var msg = multipleURI;
                else
                    msg = data.uri;
                //Call latin_navigate() //XXX
                Log.write(msg);
                
            },
            //Handler for collapsing and expanding nodes - needs special behaviour for multiedges
            onRightClick: function(node) {
                if (node)
                    if (node.collapsed)
                        fd.op.expand(node, {  
                            type: 'replot',  
                            hideLabels: true                     
                        }); 
                    else
                        fd.op.contract(node, {  
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
            domElement.innerHTML = node.name;
            var style = domElement.style;
            style.fontSize = "0.8em";
            style.color = "#ddd";
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
        }
    });
      
    //Load JSON data.
    fd.loadJSON(json);
    //Compute positions incrementally and animate.
    fd.computeIncremental({
        iter: 40,
        property: 'end',
        onStep: function(perc){
            Log.write(perc + '% loaded...');
        },
        onComplete: function(){
            Log.write('');
            fd.animate({
                modes: ['linear'],
                //For a nicer effect - transition: $jit.Trans.Elastic.easeOut,
                transition: $jit.Trans.linear,
                duration: 0
            });
        }
    });
}
