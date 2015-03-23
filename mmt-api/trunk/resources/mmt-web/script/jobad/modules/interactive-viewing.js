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
				var contentNode = mmt.createInlineBox(target, mmt.currentURI);
				mmt.ajaxAppendBox(svgURI, contentNode);		
				};
			res["Alignments"] = function() {
				var  cont =  ":align?" + mmt.currentURI;
				var alignNode = mmt.createInlineBox(target, "Alignments for bool");
				mmt.ajaxAppendBox(cont, alignNode);		
				};
		
		//res["get OMDoc"] = mmt.openCurrentOMDoc();
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


