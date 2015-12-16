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


	contextMenuEntries: function(targetArray, JOBADInstance) {
		var target = targetArray[0];  //for some reason jobad passes [target] instead of target
		mmt.setCurrentPosition(target);
		var res = this.visibMenu();
      if (mmt.focusIsMath) {
			var me = this;
			res["infer type"] = function(){me.showComputationResult("i", "inferred")};
			res["simplify"] = function(){me.showComputationResult("s", "simplified")};
 		    var folded = $(mmt.focus).closest('.math-folded');
            if (folded.length !== 0)
               res['unfold'] = function(){folded.removeMClass('math-folded');};
            else
               res['fold'] = function(){$(mmt.focus).addMClass('math-folded');};
		}
		if (mmt.currentURI !== null) {
			var me = this;
			/*res["set active theory"] = function(){mmt.setActiveTheory(mmt.currentURI);};*/
			res["show declaration"] = function(){me.showComp(null);};
			res["show graph"] = function() {
				var svgURI = ":svg?" + mmt.currentURI;
				var title =  mmt.currentURI.split('/');
				var contentNode = mmt.createInlineBox(mmt.target, title.slice(-1)[0], '50%');
				mmt.ajaxAppendBox(svgURI, contentNode);
			};
			res["show URI"] = function(){alert(mmt.currentURI);};
/*			res["comment"] = function(){me.addComment()};*/
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
			   var contentNode = mmt.createInlineBox(mmt.target, title);
			   $(contentNode).append(pres);
   			} catch (err) {
			   var errorNode = mmt.createInlineBox(mmt.target, "error");
			   $(errorNode).append(result.firstChild);
   			}
   	  });
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
	    var commNode = mmt.createInlineBox(mmt.target, "New comment: " + title.slice(-1)[0]);
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
		var alignNode = mmt.createInlineBox(mmt.target, "Alignments for symbol " + title.slice(-1)[0]);
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


