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
		
			res["infer type"] = me.inferType;
			res["simplify"] = me.simplify;
		   var folded = $(mmt.focus).closest('.math-folded');
         if (folded.length !== 0)
            res['unfold'] = function(){folded.removeMClass('math-folded');};
         else
            res['fold'] = function(){$(mmt.focus).addMClass('math-folded');};
		}
		if (mmt.currentURI !== null) {
			var me = this;
			res["type"] = function(){me.showComp('type');};
			res["show definition"] = function(){me.showComp('definition');};
			res["show URI"] = function(){alert(mmt.currentURI);};
			res["set active theory"] = function(){mmt.setActiveTheory(mmt.currentURI);};
			res["SVG"] = function() {
				
			var preSVG = target.attributes.item(1).value;
			var svgURI = preSVG.split("#")[0];
			svgURI = ":svg?" + svgURI;
			
		    //var svgURI = ":svg?" + mmt.currentURI;
			var contentNode = mmt.createInlineBox(target, mmt.currentURI);
			mmt.ajaxAppendBox(svgURI, contentNode);
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
	
	/* sends type inference query to server for the currentComponent and currentPosition */
	inferType : function (){
		var query = qmt.present(qmt.infer(
		    qmt.subobject(qmt.component(qmt.literalPath(mmt.currentElement), mmt.currentComponent), mmt.currentPosition),
		    uris.lf
		));
		qmt.exec(query,
				  function(result) {
					  try {
						  var pres = result.firstChild.firstChild.firstChild;
						  var contentNode = mmt.createInlineBox(target, 'type');
						  $(contentNode).append(pres);
					  } catch(err) { // probably result is an error report
						  var errorNode = mmt.createInlineBox(target, 'error');
						  $(errorNode).append(result.firstChild);
					  }
				  }
				 );
	},
	
	/* sends a simplification query to server for the currentComponent and currentPosition */
	simplify : function (){
		
		var query = qmt.present(qmt.simplify(
		    qmt.subobject(qmt.component(qmt.literalPath(mmt.currentElement), mmt.currentComponent), mmt.currentPosition),
		    uris.lf
		));
		
		qmt.exec(query,
				  function(result) {
					  try {
						  var pres = result.firstChild.firstChild.firstChild;
						  var contentNode = mmt.createInlineBox(target, 'simplified');
						  $(contentNode).append(pres);
					  } catch(err) { // probably result is an error report
						  var errorNode = mmt.createInlineBox(target, 'error');
						  $(errorNode).append(result.firstChild);
					  }
				  }
				 );
	},
	
	
	
	/* shows a component of the current MMT URI in a dialog */
	showComp : function(comp) {
	   
		   var query = qmt.present(qmt.component(qmt.literalPath(mmt.currentURI), comp));
		   qmt.exec(query,
			   	     function(result){
			   			try{
						   var pres = result.firstChild.firstChild.firstChild;
						   var contentNode = mmt.createInlineBox(target, comp + " -> " + mmt.currentURI);
						   $(contentNode).append(pres);
			   			} catch (err)
			   			{
			   				
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


