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
	

	contextMenuEntries: function(target, JOBADInstance) {
		mmt.focus = target;
		mmt.focusIsMath = ($(mmt.focus).closest('math').length !== 0);
		var res = this.visibMenu();

		if (mmt.focusIsMath) {
			mmt.setCurrentPosition(target);
			mmt.focus = mmt.getSelectedParent(target)
			var me = this;
			res["infer type"] = function() {me.inferType()};
	  		return res;
			if (target.hasAttribute("jobad:href")) {
				mmt.currentURI = target.getAttribute('jobad:href');
				res["show type"] =  this.showComp('type');
				res["show definition"] =  this.showComp('definition');
				res["(un)mark occurrences"] =  this.showOccurs();
				res["open in new window"] = mmt.openCurrent();
				res["show URI"] =  alert(currentURI);
				res["get OMDoc"] = mmt.openCurrentOMDoc();
			}
		} else if ($(target).hasClass('folder') || mmt.focusIsMath) {
			return res;
		} else {
			return false;
		}
	},
	
	/* Second Menu Dependencies */

	/** highlights all occurrences of the current URI */
	showOccurs : function (){
		var occs = $('mo').filterMAttr('jobad:href', currentURI).toggleMClass('math-occurrence')
	},
	

	/** sends type inference query to server for the currentComponent and currentPosition */
	inferType : function (){
		var query = mmt.Qpresent(mmt.QtypeLF(mmt.Qsubobject(mmt.Qcomponent(mmt.Qindividual(mmt.currentElement), mmt.currentComponent), mmt.currentPosition)));
		mmt.execQuery(query,
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
	
	/** shows a component of the current MMT URI in a dialog */
	showComp : function (comp) {
		var query = mmt.Qpresent(mmt.Qcomponent(mmt.Qindividual(mmt.currentURI), comp));
		execQuery(query,
				  function(result){mmt.setLatinDialog(result.firstChild.firstChild.firstChild, comp);}
				 );
	},
	
	
	/* Helper Functions  */
	/** These seem innacurate now looking at the mathml. e.g. brackets are not mo but mfenced*/
	setVisib : function(prop, val){
		var root = mmt.focusIsMath ? mmt.getSelectedParent(mmt.focus) : mmt.focus.parentNode;
		console.log(root);
		if (val == 'true')
			$(root).find('.' + prop).removeClass(prop + '-hidden');
		if (val == 'false')
			$(root).find('.' + prop).addClass(prop + '-hidden');
	},
	
	quoteSetVisib : function(prop, val){
		var me = this;
		return function(){ me.setVisib(prop,val) };
	},
	
	visibSubmenu : function(prop){
		return {
			"show" : this.quoteSetVisib(prop, true),
			"hide" : this.quoteSetVisib(prop, false)
		};
	},
	
	visibMenu : function(){
	    return {
			"reconstructed types" :  this.visibSubmenu('reconstructed'),
			"implicit arguments" : this.visibSubmenu('implicit-arg'),
			"implicit binders" : this.visibSubmenu('implicit-binder'),
			"redundant brackets" : this.visibSubmenu('brackets'),
		}
	},		
};

JOBAD.modules.register(interactiveViewing);


