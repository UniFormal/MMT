var hovering = {
	/* JOBAD Interface  */ 
	info: {
		'identifier' : 'kwarc.mmt.hovering',
		'title' : 'MMT hovering Service',
		'author': 'MMT developer team',
		'description' : 'The main service handling hovering for MMT documents',
		'version' : '1.0',
		'dependencies' : [],
		'hasCleanNamespace': false
	},

	selectSource: function(target) {
 		if (target.hasAttribute(mmtattr.source)) {
		   var sourceRef = target.getAttribute(mmtattr.source);
		   if (sourceRef[0] == '#') {
            var matches = sourceRef.match(/#(\d+)\.\d+\.\d+:(\d+)\.\d+\.\d+/);
            if (matches.length == 3) {
               var start = parseInt(matches[1]);
               var end   = parseInt(matches[2]);
               var ip = $('#inputbox')[0];
               ip.selectionStart = start;
               ip.selectionEnd = end+1;
            }
         }
	   }
	},

	hoverText: function(targetObj, JOBADInstance) {
		//hover on element with symref: show and select the smallest proper superexpression
		var target = targetObj[0];
		if (targetObj.closest('svg').length !== 0)
		   return false; // hoverText in svg confuses JOBAD
		var onMath = targetObj.closest('math').length !== 0;
		if (target.hasAttribute(mmtattr.symref)) {
			if (onMath) {
			   var mr = targetObj.parents().filter(function(){return this.hasAttribute(mmtattr.position)});
			   var select = (mr.length == 0) ? target : mr[0];
			   mmt.setSelected(select);
			   this.selectSource(select);
			}
			return target.getAttribute(mmtattr.symref);
		}
		// hover on bracketed expression: select expression
		if (mmt.getTagName(target) == 'mfenced') {
			mmt.setSelected(target);
			this.selectSource(target);
			return true;
		}
		// hover on variable: select declaration
		if (target.hasAttribute(mmtattr.varref)) {
			var v = $(target).parents('math').find('*').filter(function() {
                return $(this).attr(mmtattr.position) == targetObj.attr(mmtattr.varref);
			})
			mmt.setSelected(v[0]);
			this.selectSource(v[0]);
			return true;
		}
		return false;
	},
	
}

JOBAD.modules.register(hovering);
