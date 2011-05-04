var labelType, useGradients, nativeTextSupport, animate;

(function() {
  var ua = navigator.userAgent,
      iStuff = ua.match(/iPhone/i) || ua.match(/iPad/i),
      typeOfCanvas = typeof HTMLCanvasElement,
      nativeCanvasSupport = (typeOfCanvas == 'object' || typeOfCanvas == 'function'),
      textSupport = nativeCanvasSupport 
        && (typeof document.createElement('canvas').getContext('2d').fillText == 'function');
  //I'm setting this based on the fact that ExCanvas provides text support for IE
  //and that as of today iPhone/iPad current text support is lame
  labelType = (!nativeCanvasSupport || (textSupport && !iStuff))? 'Native' : 'HTML';
  nativeTextSupport = labelType == 'Native';
  useGradients = nativeCanvasSupport;
  animate = !(iStuff || !nativeCanvasSupport);
})();

var Log = {
  elem: false,
  write: function(text){
    if (!this.elem) 
      this.elem = document.getElementById('log');
    this.elem.innerHTML = text;
    this.elem.style.left = (500 - this.elem.offsetWidth / 2) + 'px';
  }
};

function initGraph(json){
  $jit.ForceDirected.Plot.EdgeTypes.implement({
    'doublea': {
      'render': function(adj, canvas) {
        var from = adj.nodeFrom.pos.getc(true),
            to = adj.nodeTo.pos.getc(true),
            dim = adj.getData('dim');
        var ctx = canvas.getCtx();
        var vect = new $jit.Complex(to.x - from.x, to.y - from.y);
        vect.$scale(dim / vect.norm());
        var intermediatePoint = new $jit.Complex(to.x - vect.x, to.y - vect.y),
            normal = new $jit.Complex(-vect.y / 2, vect.x / 2),
            v1 = intermediatePoint.add(normal), 
            v2 = intermediatePoint.$add(normal.$scale(-1));
            
        var vect2 = new $jit.Complex(to.x - from.x, to.y - from.y);
        vect2.$scale(dim / vect2.norm());
        var intermediatePoint2 = new $jit.Complex(from.x + vect2.x, from.y + vect2.y),
            normal = new $jit.Complex(-vect2.y / 2, vect2.x / 2),
            v12 = intermediatePoint2.add(normal), 
            v22 = intermediatePoint2.$add(normal.$scale(-1));
        
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
      },
      'contains': function(adj, pos) {
        var from = adj.nodeFrom.pos.getc(true),
            to = adj.nodeTo.pos.getc(true);
        return this.EdgeHelper.line.contains(from, to, pos, this.edge.epsilon);
      }
    },
    'self': {
      'render': function(adj, canvas) {
        var from = adj.nodeFrom.pos.getc(true);
            dim = adj.getData('dim');
        var data = adj.data;
        var param = data.param;
        var ctx = canvas.getCtx();
        var vect = new $jit.Complex(20*param, 20*param);
        vect.$scale(dim / vect.norm());
        var intermediatePoint = new $jit.Complex(from.x + 20*param - vect.x, from.y + 20*param - vect.y),
            normal = new $jit.Complex(-vect.y / 2, vect.x / 2),
            v1 = intermediatePoint.add(normal), 
            v2 = intermediatePoint.$add(normal.$scale(-1));        
        ctx.beginPath();
        ctx.arc(from.x, from.y + 20*param, param*20, 0, 2*Math.PI, true);
        ctx.stroke();
        ctx.beginPath();
        ctx.moveTo(v1.x, v1.y);
        ctx.lineTo(v2.x, v2.y);
        ctx.lineTo(from.x, from.y);
        ctx.closePath();
        ctx.fill();
      },
      'contains': function(adj, pos) {
        var from = adj.nodeFrom.pos.getc(true),
            to = adj.nodeTo.pos.getc(true);
        return this.EdgeHelper.line.contains(from, to, pos, this.edge.epsilon);
      }
    },
    'multiple': {
      'render': function(adj, canvas) {
        var from = adj.nodeFrom.pos.getc(true),
            to = adj.nodeTo.pos.getc(true),
            dim = adj.getData('dim');
        var data = adj.data;
        var param = data.param;
        var ctx = canvas.getCtx();
        var slope = (from.x - to.x)/(to.y - from.y);
        var mid = new $jit.Complex((from.x + to.x)/2, (from.y + to.y)/2);
        var d = mid.y - slope*mid.x;
        var a = slope * slope + 1;
        var b = 2 * (slope * d - mid.x - mid.y * slope);
        var c = mid.x * mid.x + mid.y * mid.y - 2 * mid.y * d + d * d - param * param * 200;
        var tmpx = 0;
        if (param > 0) {
            tmpx = (-b + Math.sqrt(b * b - 4 * a *c))/(2*a);
        } else {
            tmpx = (-b - Math.sqrt(b * b - 4 * a *c))/(2*a);
        }
        
        var control = new $jit.Complex(tmpx, tmpx * slope + d);
        
        var vect = new $jit.Complex(to.x - control.x, to.y - control.y);
        vect.$scale(dim / vect.norm());
        var intermediatePoint = new $jit.Complex(to.x - vect.x, to.y - vect.y),
            normal = new $jit.Complex(-vect.y / 2, vect.x / 2),
            v1 = intermediatePoint.add(normal), 
            v2 = intermediatePoint.$add(normal.$scale(-1));
        
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
      },
      'contains': function(adj, pos) {
        var from = adj.nodeFrom.pos.getc(true),
            to = adj.nodeTo.pos.getc(true);
        return this.EdgeHelper.line.contains(from, to, pos, this.edge.epsilon);
      }
    },
    'label-arrow': {
        'render': function(adj, canvas) {
        //plot arrow edge
        this.edgeTypes.arrow.render.call(this, adj, canvas); 
        //get nodes cartesian coordinates
        var pos = adj.nodeFrom.pos.getc(true);
        var posChild = adj.nodeTo.pos.getc(true);
        //check for edge label in data
        var data = adj.data;
        if(data.uri) {
           //now adjust the label placement
        var radius = this.viz.canvas.getSize();
        var x = parseInt((pos.x + posChild.x - (data.uri.length * 5)) /2);
        var y = parseInt((pos.y + posChild.y ) /2);
        this.viz.canvas.getCtx().fillText(data.uri, x, y); 
        }
    },
    'contains': function(adj, pos) {
        var from = adj.nodeFrom.pos.getc(true),
            to = adj.nodeTo.pos.getc(true);
        return this.EdgeHelper.line.contains(from, to, pos, this.edge.epsilon);
    }
    }    
  });
  // init ForceDirected
  var fd = new $jit.ForceDirected({
    //id of the visualization container
    injectInto: 'graph',
    //Enable zooming and panning
    //by scrolling and DnD
    Navigation: {
      enable: true,
      //Enable panning events only if we're dragging the empty
      //canvas (and not a node).
      panning: 'avoid nodes',
      zooming: 10 //zoom speed. higher is more sensible
    },
    // Change node and edge styles such as
    // color and width.
    // These properties are also set per node
    // with dollar prefixed data-properties in the
    // JSON structure.
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
      //Add also a click handler to nodes //XXX TODO
      onClick: function(node) {
        if(!node) return;
        // Build the right column relations list.
        // This is done by traversing the clicked node connections.
        var data = node.data;
        if (node.nodeFrom)
            var html = "<h4>" + data.$direction[0] + "->" + data.$direction[1] + "</h4>" + data.uri;
        else
            html = "<h4>" + node.name + "</h4>" + data.uri;
        $jit.id('inner-details').innerHTML = html;
      }
    },
    //Number of iterations for the FD algorithm
    iterations: 200,
    //Edge length
    levelDistance: 130,
    // Add text to the labels. This method is only triggered
    // on label creation and only for DOM labels (not native canvas ones).
    onCreateLabel: function(domElement, node){
      domElement.innerHTML = node.name;
      var style = domElement.style;
      style.fontSize = "0.8em";
      style.color = "#ddd";
    },
    // Change node styles when DOM labels are placed
    // or moved.
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
  // load JSON data.
  fd.loadJSON(json);
  // compute positions incrementally and animate.
  fd.computeIncremental({
    iter: 40,
    property: 'end',
    onStep: function(perc){
      Log.write(perc + '% loaded...');
    },
    onComplete: function(){
      Log.write('done');
      fd.animate({
        modes: ['linear'],
        transition: $jit.Trans.Elastic.easeOut,
        duration: 2500
      });
    }
  });
  // end
}
