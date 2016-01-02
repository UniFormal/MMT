var interactiveViewing = {
	/* JOBAD Interface  */
	info: {
		'identifier' : 'kwarc.mmt.intvw',
		'title' : 'MMT Service for Active Documents',
		'author': 'MMT developer team',
		'description' : 'The main service for MMT Active Documents',
		'version' : '1.0',
		'dependencies' : [],
		'hasCleanNamespace': false
	},

	init: function(JOBADInstance) {
		var query = window.location.search.substring(1);
		if (query != "")
		   this.navigate(query);
	},

	navigate: function(uri) {
		// main div
		var url = mmt.adaptMMTURI(uri, '', true);
		mmt.ajaxReplaceIn(url, 'main');
		var bcurl = '/:breadcrumbs?' + uri;
		mmt.ajaxReplaceIn(bcurl, 'breadcrumbs');
		if (uri.split("?").length == 2)
			mmt.setActiveTheory(uri);
	    // setSVGPanRoot($('svg')[0]);
	},
	
	navigateServer: function(uri) {
	   url = '/:admin?navigate ' + uri;
	   $.ajax({'url': url});
	},

	leftClick: function(elem, JOBADInstance) {
	   var target = $(elem);
	   //handling clicks on parts of the document
        /* TODO is this code ever used?
        if(target.hasAttribute('data-mmt-link')) {
			var uri = target.attr('data-mmt-link');
			this.navigate(uri);
		}
		if(target.hasAttribute('loadable')) {
			var elem = target.parent().get(0);
			var ref = mmt.load(elem);
			$(ref).find('span').attr('foldable', 'true');
			$(elem).replaceWith(ref);
		}
		if(target.hasAttribute('data-mmt-flattenable')) {
			var elem = target.parent().get(0);
			var loaded = mmt.load(elem);
			var fc = $(elem).children().filterMClass('flat-container');
			fc.children().replaceWith(loaded);
			fc.toggle();
		}
		if(target.hasAttribute('foldable')) {
			var content = $(target).parent().find('table').toggle();				
		}
		if (target.hasClass("loadable")) {
			var elem = target.get(0); 
			var ref = mmt.load(elem);
			$(elem).replaceWith(ref);
		}*/

		/* clicking on attributes toggleTarget
		   toggles all .toggleTarget children of the next toggle-root ancestor */
		if (target.hasAttribute(mmtattr.toggleTarget)) {
		   var toggleTarget = target.attr(mmtattr.toggleTarget)
			target.closest('.toggle-root').find('.'+toggleTarget).toggle();
		}
		
		/*
		if (target.hasAttribute(mmtattr.symref)) {
			var uri = target.attr(mmtattr.symref);
			this.navigate(uri);
		}*/

		mmt.unsetSelected();	
		return true;
	},

	contextMenuEntries: function(targetArray, JOBADInstance) {
      var target = targetArray[0];
		mmt.setCurrentPosition(target);

		var res = this.visibMenu();

	   if (mmt.focusIsSVG) {
	      var svg = targetArray.closest('svg')[0];
         var zoomFun = function(by) {return function() {svgHelper.zoom(svg,by)}};
	      res["zoom"] = {
	         "in by 2": zoomFun(2),
	         "in by 1.1": zoomFun(1.1),
	         "out by 1.1": zoomFun(1/1.1),
	         "out by 2": zoomFun(1/2),
	      }
	   }
		
      if (mmt.focusIsMath) {
			var me = this;                                                                                                            
			res["infer type"] = function(){me.showComputationResult("i", "inferred type")};
			res["simplify"] = function(){me.showComputationResult("s", "simplified")};
 		    var folded = $(mmt.focus).closest('.math-folded');
            if (folded.length !== 0)
               res['unfold'] = function(){folded.removeMClass('math-folded');};
            else
               res['fold'] = function(){$(mmt.focus).addMClass('math-folded');};
		}
		
		if (mmt.currentURI !== null) {
      	var uri = mmt.currentURI;

			var sub = {};
			var me = this;
   	   sub["in this window"] = function() {
            me.navigate(uri);
      	};
	      sub["in new window"] = function() {mmt.openCurrent();};
   	   sub["in remote listeners"] = function() {
            me.navigateServer(uri);
      	};
      	var name = mmt.splitMMTURI(uri, false).slice(-1)[0];
			res["go to declaration of '" + name + "'"] = sub;

			/*res["set active theory"] = function(){mmt.setActiveTheory(mmt.currentURI);};*/
			res["show declaration"] =
			   function(){me.showComp(null);};
			res["show graph"] = function() {
				var svgURI = ":svg?" + uri;
				var title = uri.split('/');
				var contentNode = inlineBox.create(mmt.target, title.slice(-1)[0]);
				$(contentNode).addClass('graph-body');
				mmt.ajaxAppendBox(svgURI, contentNode);
			};
			res["show URI"] = function(){alert(uri);};
         /*	res["comment"] = function(){me.addComment()};*/
		}
		return res;
	},

	/* functions for context menu items */

	/* highlights all occurrences of the current URI */
	showOccurs : function (){
		$('mo').filter(function(index){
		    return this.getAttribute(mmtattr.symref) == mmt.currentURI;
	   }).toggleMClass('math-occurrence');
	},

	/* showQuery where the query is for the result of applying a wrapper function to the selected expression;
	 * typical wrappers: qmt.infer, qmt.simplify
	 */
	showComputationResult : function (key, title){
		var q = qmt.subobject(qmt.component(qmt.literalPath(mmt.currentElement), mmt.currentComponent), mmt.currentPosition)
		if (key == "i")
		  q = qmt.infer(q, uris.lf)
    	else if (key == "s")
  		  q = qmt.simplify(q, uris.lf)
		this.showQuery(qmt.present(q), title);
	},
	/* showQuery where the query if for (if comp is non-null: a component of) the declaration of the current MMT URI */
	showComp : function(comp) {
		   var q = qmt.literalPath(mmt.currentURI)
		   if (comp != null)
			   q = qmt.present(qmt.component(q, comp))
		   else
			   q = qmt.presentDecl(q)
		   var title = (comp != null ? comp + " of " : "") + mmt.currentURI
		   this.showQuery(q, title)
	},
	/* sends a query to the server and shows the presented result */
	showQuery : function(query, title) {
      qmt.exec(query,
   	   function(result){
   		  try {
			    var pres = result.firstChild.firstChild.firstChild;
			    var contentNode = inlineBox.create(mmt.target, title);
			    $(contentNode).append(pres);
   		  } catch (err) {
			    var errorNode = inlineBox.create(mmt.target, "error");
			    $(errorNode).append(result.firstChild);
   		  }
   	   }
   	);
	},

	/* Helper Functions  */
	setVisib : function(prop, val){
		var root = mmt.focusIsMath ? mmt.focus : mmt.focus.parentNode;
		if (val)
			$(root).find('.' + prop).removeMClass(prop + '-hidden');
		if (!val)
			$(root).find('.' + prop).addMClass(prop + '-hidden');
	},

	visibSubmenu : function(prop){
	   var me = this;
	   var sub
		return {
			"on" : function(){me.setVisib(prop,true)},
			"off" : function(){me.setVisib(prop,false)},
		};
	},

	visibMenu : function(){
	    return {"visibility" : {
			"reconstructed types" :  this.visibSubmenu('reconstructed'),
			"implicit arguments" : this.visibSubmenu('implicit-arg'),
			"redundant brackets" : this.visibSubmenu('brackets'),
		}}
	},
	
	addComment: function() {
	    var query = mmt.currentURI;
	    var title = mmt.currentURI.split('/')
	    var commNode = inlineBox.create(mmt.target, "New comment: " + title.slice(-1)[0]);
	    var first = document.createElement('div');
	    $(first).append("<form id=\"form-id\" method=\"post\">" +
	        "<input class=\"comm\" id=\"user-id\" type=\"text\" " +
	        "placeholder=\"Your name\" name=\"user\" required>" +
	        "<textarea  id=\"comm-id\" name=\"comment\"" +
	        " placeholeder=\"say something\" class=\"form-control\" rows=\"3\"></textarea>" +
	        "<input class=\"btn btn-info comm pull-right\" " +
	        "id=\"btn-id\"type=\"submit\" value=\"Submit\"></form>")
	    $(commNode).append(first);
	    mmt.ajaxAppendBox(query, commNode);
	    $(first).find("#btn-id").click(function() {
	    	var data = {
	        user: $(first).find("#user-id").val(),
	        comment: $(first).find("#comm-id").val()
	        }
	        var toSend = JSON.stringify(data);
	    	$(first).find("#form-id").submit(function() {
	        var url = ":submit_comment?" + mmt.currentURI
	        $.ajax({
	        	url: url,
	            type: 'POST',
	            data: toSend,
	            contentType: 'text',
                success: function(data) {
                	var replace = "<div><h3 style=\"color: blue\">" +
                    				"Thank you for your comment!</h3></div>"
                    $(first).replaceWith(replace)
                		}
            		});
	        return false;
	        	});
	    	});
		}

	/*
	showAlignments: function() {
		var get = mmt.currentURI;
		var cont = ":align?" + mmt.currentURI;
		var title = mmt.currentURI.split('/')
		var alignNode = inlineBox.create(mmt.target, "Alignments for symbol " + title.slice(-1)[0]);
		mmt.ajaxAppendBox(get, alignNode);
		$.ajax({
			url: cont,
	        type: 'GET',
	        contentType: 'text',
	        success: function(data) {
	        	if(data.length === 0) {
	        		var resp = "<div><p>No alignments for this symbol so far!</p></div>"
	            	$(alignNode).append(resp)
	            } else {
	            	var miz = "<div><table><tr><td class=\"miz\"></td><td>Mizar</td></tr></table></div>"
	                var hol = "<div><table><tr><td class=\"hol\"></td><td>HOLLight</td></tr></table></div></br>"
	                var json = JSON.parse(data)
	            	me.tree(json, alignNode)
	            	$(alignNode).append(hol)
	            	$(alignNode).append(miz)		                	 
	            	}   	
	        	}
	   		});
		},
    // auxiliary method called by showAlignments
	tree: function(treeData, parentNode) {
		//This uses the D3 library to create SVG dynamically from JSON data retrieved from the server.
		//It is a proof of concept and hard-codes Mizar and HOL Light. 
		var margin = {top: 50, right: 120, bottom: 20, left: 120},
		 width = 600 - margin.right - margin.left,
		 height = 300 - margin.top - margin.bottom;
		 
		var i = 0;
		var colors =[ ["Mizar", "#0080FF"],
					   ["HOLLight", "#AED5FC"],
					   ["MMT", "lightgray"] ];
		
		var tree = d3.layout.tree()
		 .size([height, width]);

		var diagonal = d3.svg.diagonal()
		 .projection(function(d) { return [d.x, d.y]; });

		var svg = d3.select(parentNode).append("svg")
		 .attr("width", width + margin.right + margin.left)
		 .attr("height", height + margin.top + margin.bottom)
		 .append("g")
		 .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

		root = treeData[0];
		  
		update(root);

		function update(source) {
		  var nodes = tree.nodes(root).reverse(),
		   links = tree.links(nodes);
		  nodes.forEach(function(d) { d.y = d.depth * 200; });
		  var node = svg.selectAll("g.node")
		   .data(nodes, function(d) { return d.id || (d.id = ++i); });
		  var nodeEnter = node.enter().append("g")
		  .append("a").attr("xlink:xlink:href", (function(d) {return d.address}))
		   .attr("class", "node")		   
		   .attr("transform", function(d) { 
			   console.log("dx  = " + d.x +" dy = "+d.y)
			return "translate(" + d.x + "," + d.y + ")"; });

		  nodeEnter.append("circle")
		   .attr("r", 20)
		   .style("fill", function(d) {  if (d.note === "mizar")  var r = '#0080FF' 
			   								else if(d.note === "hol") r = '#AED5FC'
			   								else r = 'lightgray';
		   									return r;});

		  nodeEnter.append("text")
		   .attr("text-anchor", "middle")
		   .text(function(d) { return d.name ; })
		   .style("fill-opacity", 5);

		  var link = svg.selectAll("path.link")
		   .data(links, function(d) { return d.target.id; });

		  link.enter().insert("path", "g")
		   .attr("class", "link")
		   .attr("d", diagonal);
		}
	},*/
};

JOBAD.modules.register(interactiveViewing);
