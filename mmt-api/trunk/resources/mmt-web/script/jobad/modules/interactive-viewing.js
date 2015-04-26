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
		target = targetArray[0];  //for some reason jobad passes [target] instead of target
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
			res["show declaration"] = function(){me.showComp(null);};
			res["show URI"] = function(){alert(mmt.currentURI);};
			res["set active theory"] = function(){mmt.setActiveTheory(mmt.currentURI);};
			res["show graph"] = function() {
				var svgURI = ":svg?" + mmt.currentURI;
				var title =  mmt.currentURI.split('/');
				var contentNode = mmt.createInlineBox(target, title.slice(-1)[0], '50%');
				mmt.ajaxAppendBox(svgURI, contentNode);		
				};			
			res["show alignments"] = function() {
				var  cont =  ":align?" + mmt.currentURI;
				var title =  mmt.currentURI.split('/')
				var alignNode = mmt.createInlineBox(target, "Alignments for symbol " + title.slice(-1)[0]);
				mmt.ajaxAppendBox(cont, alignNode);		
				};
			res["comment"] = function() {
			    // query to create the comment box
			    var query = ":comment_box?" + mmt.currentURI;
			    var title = mmt.currentURI.split('/')
			    var commNode = mmt.createInlineBox(target, "New comment: " + title.slice(-
			        1)[0]);
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
			    $("#btn-id").click(function() {
			        var data = {
			            user: $("#user-id").val(),
			            comment: $("#comm-id").val()
			        }
			        var toSend = JSON.stringify(data);
			        $("#form-id").submit(function() {
			            var url = ":submit_comment?" + mmt.currentURI
			            $.ajax({
			                url: url,
			                type: 'POST',
			                data: toSend,
			                contentType: 'text',
			                success: function(data) {
			                    var replace =
			                        "<div><h3 style=\"color: blue\">" +
			                        "Thank you for your comment!</h3></div>"
			                    $(first).replaceWith(
			                        replace)
			                }
			            });

			            return false;
			        });

			    });

			};		// res["get OMDoc"] = mmt.openCurrentOMDoc();
		}
		return res;
	},
	
	/* functions for context menu items */
	
	/* highlights all occurrences of the current URI */
	showOccurs : function (){
		$('mo').filter(function(index){
		    return this.getAttribute('jobad:href') == mmt.currentURI;
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
			   var contentNode = mmt.createInlineBox(target, title);
			   $(contentNode).append(pres);
   			} catch (err) {
			   var errorNode = mmt.createInlineBox(target, "error");
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
		return {
			"show" : function(){me.setVisib(prop,true)},
			"hide" : function(){me.setVisib(prop,false)},
		};
	},
	
	visibMenu : function(){
	    return {
			"reconstructed types" :  this.visibSubmenu('reconstructed'),
			"implicit arguments" : this.visibSubmenu('implicit-arg'),
			"redundant brackets" : this.visibSubmenu('brackets'),
		}
	},
};

JOBAD.modules.register(interactiveViewing);


