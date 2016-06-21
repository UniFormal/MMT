var planetaryNavigation = {
	info: {
		'identifier' : 'kwarc.mmt.planetary.navigation',
		'title' : 'MMT Navigation Service in Planetary',
		'author': 'MMT developer team',
		'description' : 'The navigation service for browsing MMT repositories in Planetary',
		'version' : '1.0',
		'dependencies' : [],
		'hasCleanNamespace': false
	},
  

    leftClick: function(target, JOBADInstance) {
    	if(target.hasAttribute('loadable')) {
			var elem = target.parent().get(0);
			var uri = $(elem).attr('jobad:load');
			var uriEnc = this.encode(uri);
		    window.location.search = "?q=" + uriEnc;
			return true;
		}
	    return false;
    },


    contextMenuEntries: function(target, JOBADInstance) {
		if (target.hasAttribute('jobad:href')) {			
			var mr = $(target).closest('mrow');
			var select = (mr.length == 0) ? target : mr[0];
			mmt.setSelected(select);
			var uri = target.attr('jobad:href');
			console.log(uri);
		 	var me = this;
			return {
				'Go To Declaration': function() {me.planetaryOpen(uri)},
			}
		}
		return false;
	},

	planetaryOpen : function(uri) {
	  uriSegs = uri.split("?");
	  if (uriSegs.length < 2) {//module or document path 
	    window.location.search = "?q=" + this.encode(uri);
      } else { //symbol or fragment path
		var modUri = uriSegs[0] + "?" + uriSegs[1];
		window.location.search = "?q=" + this.encode(modUri);        
      }
	},


    encode : function(uri) {
		var rawEncoded = encodeURIComponent(uri);
		//mirroring drupal in not escaping slashes
		return rawEncoded.replace(/%2F/g, "/");
    },
}


JOBAD.modules.register(planetaryNavigation);
