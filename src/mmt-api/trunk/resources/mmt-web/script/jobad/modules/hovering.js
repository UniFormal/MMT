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
 		if (target.hasAttribute('jobad:mmtsrc')) {
		   var sourceRef = target.getAttribute('jobad:mmtsrc');
		   if (sourceRef[0] == '#') {
            var matches = sourceRef.match(/#(\d+)\.\d+\.\d+-(\d+)\.\d+\.\d+/);
            if (matches.length == 3) {
               var start = parseInt(matches[1]);
               var end   = parseInt(matches[2]);
               var ip = $('#inputbox')[0];
               ip.selectionStart = start;
               ip.selectionEnd = end;
            }
         }
	   }
	},

	hoverText: function(target, JOBADInstance) {
		//hover on OMS: show jobad:href and select the smallest proper superexpression
		if (target.hasAttribute('jobad:href')) {			
			var mr = $(target).closest('mrow');
			var select = (mr.length == 0) ? target : mr[0];
			mmt.setSelected(select);
			this.selectSource(select);
			return target.attr('jobad:href');
		}
		// hover on bracketed expression: select expression
		if (mmt.getTagName(target) == 'mfenced') {
			mmt.setSelected(target);
			this.selectSource(target);
			return true;
		}
		// hover on variable: select declaration
		if (target.hasAttribute('jobad:varref')) {
			var v = $(target).parents('mrow').find('mrow').filter(function() {
                return $(this).attr('jobad:mmtref') == target.attr('jobad:varref');
			})
			mmt.setSelected(v[0]);
			this.selectSource(v[0]);
			return true;
		}
		return false;
	},
	
}

JOBAD.modules.register(hovering);
