var navigation = {
	info: {
		'identifier' : 'kwarc.mmt.navigation',
		'title' : 'MMT Navigation Service',
		'author': 'MMT developer team',
		'description' : 'The navigation service for browsing MMT repositories',
		'version' : '1.0',
		'dependencies' : [],
		'hasCleanNamespace': false
	},
	
	init: function(JOBADInstance) {
		var query = window.location.search.substring(1);
		if (query != "")
		   this.navigate(query);
	},

	navigate: function(uri) {
		// main div
		var url = mmt.adaptMMTURI(uri, '', true);
		mmt.ajaxReplaceIn(url, 'main');
		var bcurl = '/:breadcrumbs?' + uri;
		mmt.ajaxReplaceIn(bcurl, 'breadcrumbs');
		if (uri.split("?").length == 2)
			mmt.setActiveTheory(uri);
	    // setSVGPanRoot($('svg')[0]);
	},
	
	navigateServer: function(uri) {
	   url = '/:admin?navigate ' + uri;
	   $.ajax({'url': url});
	},

	leftClick: function(target, JOBADInstance) {
	   //handling clicks on parts of the document
        /* TODO is this code ever used?
        if(target.hasAttribute('data-mmt-link')) {
			var uri = target.attr('data-mmt-link');
			this.navigate(uri);
		}
		if(target.hasAttribute('loadable')) {
			var elem = target.parent().get(0);
			var ref = mmt.load(elem);
			$(ref).find('span').attr('foldable', 'true');
			$(elem).replaceWith(ref);
		}
		if(target.hasAttribute('data-mmt-flattenable')) {
			var elem = target.parent().get(0);
			var loaded = mmt.load(elem);
			var fc = $(elem).children().filterMClass('flat-container');
			fc.children().replaceWith(loaded);
			fc.toggle();
		}
		if(target.hasAttribute('foldable')) {
			var content = $(target).parent().find('table').toggle();				
		}
		if (target.hasClass("loadable")) {
			var elem = target.get(0); 
			var ref = mmt.load(elem);
			$(elem).replaceWith(ref);
		}*/

		if (target.hasAttribute(mmtattr.symref)) {
			var uri = target.attr(mmtattr.symref);
			this.navigate(uri);
		}

		mmt.unsetSelected();	
		return true;
	},

	contextMenuEntries: function(targetArray, JOBADInstance) {
	   target = targetArray[0];
		var res = {};
		var me = this;
		if (target.hasAttribute(mmtattr.symref)) {
		   res["local navigation"] = function() {
		         var r = target.getAttribute(mmtattr.symref);
		         me.navigate(r);
		   };
		   res["remote navigation"] = function() {
		         var r = target.getAttribute(mmtattr.symref);
		         me.navigateServer(r);
		   };
         res["open in new window"] = function() {mmt.openCurrent();};
		}
      return res;
   },
};

JOBAD.modules.register(navigation);

