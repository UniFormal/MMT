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
	   target = targetArray[0]  //for some reason jobad passes [target] instead of target
		mmt.setCurrentPosition(target);
		var res = this.visibMenu();
      if (mmt.focusIsMath) {
			var me = this;
			res["infer type"] = me.inferType;
			if (mmt.currentURI !== null) {
				res["show type"] = this.showComp('type');
				res["show definition"] = this.showComp('definition');
				res["(un)mark occurrences"] = this.showOccurs;
				res["show URI"] = function(){alert(mmt.currentURI)};
				res["open in new window"] = function() {mmt.openCurrent();};
				//res["get OMDoc"] = mmt.openCurrentOMDoc();
			}
			return res;
		} else if ($(target).hasClass('folder') || mmt.focusIsMath) {
			return res;
		} else {
			return false;
		}
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
		var query = qmt.present(qmt.type(
		    qmt.subobject(qmt.component(qmt.individual(mmt.currentElement), mmt.currentComponent), mmt.currentPosition),
		    uris.lf
		));
		qmt.exec(query,
				  function(result) {
					  try {
						  var pres = result.firstChild.firstChild.firstChild;
						  mmt.setLatinDialog(pres, 'type');
					  } catch(err) { // probably result is an error report
						  mmt.setLatinDialog(result.firstChild, 'type');
					  }
				  }
				 );
	},
	
	/* shows a component of the current MMT URI in a dialog */
	showComp : function(comp) {
	   return function(){
		   var query = qmt.present(qmt.component(qmt.individual(mmt.currentURI), comp));
		   qmt.exec(query,
			   	     function(result){mmt.setLatinDialog(result.firstChild.firstChild.firstChild, mmt.currentURI);}
				      );
		}
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


