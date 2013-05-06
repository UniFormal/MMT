var hovering = {
	/* JOBAD Interface  */ 
	info: {
		'identifier' : 'kwarc.mmt.hovering',
		'title' : 'MMT hovering Service',
		'author': 'MMT developer team',
		'description' : 'The main service handling hovering for MMT documents',
		'version' : '1.0',
		'dependencies' : [],
		'hasCleanNamespace': true
	},


	hoverText: function(target, JOBADInstance) {
		//handling clicks on parts of the document - active only for elements that have jobad:href
		if (target.hasAttribute('jobad:href')) {			
			var mr = $(target).closest('mrow');
			var select = (mr.length == 0) ? target : mr[0];
			mmt.setSelected(select);
			return target.attr('jobad:href');
		}
		// bracketed expression
		if (mmt.getTagName(target) == 'mfenced') {
			mmt.setSelected(target);
			return true;
		}
		// variable declaration
		if (target.hasAttribute('jobad:varref')) {
			var v = $(target).parents('mrow').children().filter(function() {
                return $(this).attr('jobad:mmtref') == target.attr('jobad:varref');
			})
			mmt.setSelected(v[0]);
			return true;
		}
		// maybe return false
		return true;
	},

}

JOBAD.modules.register(hovering);
