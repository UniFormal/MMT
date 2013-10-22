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
		$('#currentstyle').text(mmt.notstyle.split("?").pop());
		var query = window.location.search.substring(1);
		if (query != "")
		   this.navigate(query);
	},

	navigate: function (uri) {
		// main div
		var url = mmt.adaptMMTURI(uri, '', true);
		mmt.ajaxReplaceIn(url, 'main');
		// breadcrumbs
		var bcurl = '/:breadcrumbs?' + uri;
		mmt.ajaxReplaceIn(bcurl, 'breadcrumbs');
	},
	
	navigateServer: function(uri) {
	   url = '/:admin?navigate ' + uri
	   $.ajax({'url': url});
	},

	leftClick: function(target, JOBADInstance) {
	   //handling clicks on parts of the document
      if(target.hasAttribute('mmtlink')) {
			var uri = target.attr('mmtlink');
			console.log(uri);
			this.navigate(uri);
		}
		if(target.hasAttribute('loadable')) {
			var elem = target.parent().get(0);
			var ref = mmt.load(elem);
			$(ref).find('span').attr('foldable', 'true');
			$(elem).replaceWith(ref);
		}
		if(target.hasAttribute('jobad:flattenable')) {
			var elem = target.parent().get(0);
			var loaded = mmt.load(elem);
			var fc = $(elem).children().filterMClass('flat-container');
			fc.children().replaceWith(loaded);
			fc.toggle()
		}
		if(target.hasAttribute('foldable')) {
			var content = $(target).parent().find('table').toggle();				
		}
		if (target.hasAttribute('jobad:href')) {
			var mr = $(target).closest('mrow');
			var select = (mr.length == 0) ? target[0] : mr[0];
			return true; // do nothing for now
		}
		mmt.unsetSelected();	
		return true;	//we did stuff also
	},

	contextMenuEntries: function(targetArray, JOBADInstance) {
	   target = targetArray[0];
		var res = {};
		var me = this;
		if (target.hasAttribute('jobad:href')) {
		   res["remote navigation"] = function() {
		         var r = target.getAttribute('jobad:href');
		         me.navigateServer(r);
		   };
		}
      return res;
   },
};

JOBAD.modules.register(navigation);

