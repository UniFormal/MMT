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
		// mySvg = '/:svg?' + uri;
		//mmt.ajaxPutSVG(mySvg,'constant\ toggleTarget');
		// breadcrumbs
		var bcurl = '/:breadcrumbs?' + uri;
		mmt.ajaxReplaceIn(bcurl, 'breadcrumbs');
                // initialize SVG pan
                setSVGPanRoot($('svg')[0]);
	},
	
	navigateServer: function(uri) {
	   url = '/:admin?navigate ' + uri;
	   $.ajax({'url': url});
	},

	leftClick: function(target, JOBADInstance) {
	   //handling clicks on parts of the document
      if(target.hasAttribute('mmtlink')) {
			var uri = target.attr('mmtlink');
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
			fc.toggle();
		}
		if(target.hasAttribute('foldable')) {
			var content = $(target).parent().find('table').toggle();				
		}
		if (target.hasClass("loadable")) {
			var elem = target.get(0); 
			var ref = mmt.load(elem);
			$(elem).replaceWith(ref);
		}
		if (target.hasAttribute("jobad:href")) {
			$.ajax({
				'type':'post',
				'url':'/:immt/query',
				'contentType':'application/json',
				'processData':false,
				'data': '{ ' + 
 					'"subject":"' + target.attr("jobad:href") + '",' +
  					'"relation":"isDefinedBy",' + 
  					'"return":"ihtml"' + 
				'}',
				'dataType':'html',
				'success':function(data) {
					target.popover({'content':data, 'placement':'top', html:true});				
				}
			});						
		}

		if (target.hasAttribute('jobad:href')) {
			var uri = target.attr('jobad:href');
			this.navigate(uri);
		}

		mmt.unsetSelected();	
		return true;	//we did stuff also
	},

	contextMenuEntries: function(targetArray, JOBADInstance) {
	   target = targetArray[0];
		var res = {};
		var me = this;
		if (target.hasAttribute('jobad:href')) {
		   res["local navigation"] = function() {
		         var r = target.getAttribute('jobad:href');
		         me.navigate(r);
		   };
		   res["remote navigation"] = function() {
		         var r = target.getAttribute('jobad:href');
		         me.navigateServer(r);
		   };
         res["open in new window"] = function() {mmt.openCurrent();};
		}
      return res;
   },
};

JOBAD.modules.register(navigation);

