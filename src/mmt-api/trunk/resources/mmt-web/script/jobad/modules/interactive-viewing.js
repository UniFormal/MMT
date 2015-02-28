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
			
			res["type"] = this.showComp('type');
			res["show definition"] = this.showComp('definition');
			res["Show graph inline"] = function() {
				var targetParent = target.parentElement.parentElement;
				var svgDiv = document.createElement('div');
				svgDiv.id = 'svgDiv';
				$(targetParent).append(svgDiv);
				var button = document.createElement('button');
				button.id = 'btn';
				
				$(svgDiv).append(button);
				$('#btn').text('Hide Graph');
				$('#btn').click(function() {
					$( "#svgDiv" ).remove();
				});
				
				var x = document.getElementsByTagName('svg');
				console.log(x);

				/*var button2 = document.createElement('button2');
				button2.id = 'btn2';
				$(targetParent).append(button2);
				$('#btn2').text('Drag button');*/
				
				var box = $(targetParent).parents().eq(2);
				console.log(box[0]);
				box[0].style.overflow = 'scroll';
				box[0].style.resize = 'both';
				box[0].style.width = 'auto';
				box[0].style.height = 'auto';

				
				var preSVG = target.attributes.item(1).value;
				var svgURI = preSVG.split("#")[0];
				svgURI = ":svg?" + svgURI;
				mmt.ajaxAppend(svgURI, 'svgDiv');
			};

		res["Show graph in new window"] = function(){
						var preSVG = target.attributes.item(1).value;
						var svgURI = preSVG.split("#")[0];
						svgURI = ":svg?" + svgURI;
						window.open(svgURI);
						};
	
		res["show URI"] = function(){alert(mmt.currentURI)};
		res["set active theory"] = function(){mmt.setActiveTheory(mmt.currentURI);};
		res["Show in Mizar"] = function(){alert("This is will be the Mizar concept based on alignments")};
		res["Show in HOLLight"] = function(){alert("This will be the HOLLight concept based on alignments")};
        
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
						  mmt.setLatinDialog(pres, 'type');
					  } catch(err) { // probably result is an error report
						  mmt.setLatinDialog(result.firstChild, 'type');
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
						  mmt.setLatinDialog(pres, 'simplified');
					  } catch(err) { // probably result is an error report
						  mmt.setLatinDialog(result.firstChild, 'simplified');
					  }
				  }
				 );
	},
	
	createInlineBox: function(origin, body) {
		var behind = $(origin).parent('.inlineBox');
		
	},
	
	/* shows a component of the current MMT URI in a dialog */
	showComp : function(comp) {
	   return function(){
		   var query = qmt.present(qmt.component(qmt.literalPath(mmt.currentURI), comp));
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


