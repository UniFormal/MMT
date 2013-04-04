/** Global Utils */
$.fn.hasAttribute = function(name) {  
	return (typeof this.attr(name) !== 'undefined' && this.attr(name) !== false);
};

var latin = {
	/* state fields */
	// focus: holds a reference to the object that was clicked by the user
	focus : null,
	// focus: true if focus is within a math object
	focusIsMath : false,    
	notstyle : 'http://cds.omdoc.org/styles/lf/mathml.omdoc?twelf',  // hard-coding a default style for LF content
	
	/* JOBAD Interface  */ 
	
	info: {
		'identifier' : 'kwarc.latin.main',
		'title' : 'Main LATIN Service',
		'author': 'Kwarc',
		'description' : 'The main service for browsing LATIN repository',
		'version' : '1.0',
		'dependencies' : [],
		'hasCleanNamespace': false
	},
	
	/**
	 * adaptMMTURI - convert MMTURI to URL using current catalog and possibly notation style
	 * act: String: action to call on MMTURI
	 * present: Boolean: add presentation to action
	 */
	
	
	adaptMMTURI : function (uri, act, present) {
		var arr = uri.split("?");
		var doc = (arr.length >= 1) ? arr[0] : "";
		var mod = (arr.length >= 2) ? arr[1] : "";
		var sym = (arr.length >= 3) ? arr[2] : "";
		if (present && this.notstyle !== null)
			var pres = "_present_" + this.notstyle;
		else
			var pres = '';
		return '/:mmt?' + doc + '?' + mod + '?' + sym + '?' + act + pres;
	},
	
    init : function(JOBADInstance) {
		//updateVisibility(document.documentElement);
		$('#currentstyle').text(this.notstyle.split("?").pop());
		var query = window.location.search.substring(1);
		this.latin_navigate(query);
	},
	
	ajaxReplaceIn : function (url, targetid) {
		function cont(data) {
			var targetnode = $('#' + targetid).children('div');
			targetnode.replaceWith(data.firstChild);
		}
		$.ajax({ 'url': url,
				 'dataType': 'xml',
				 'success': cont
			   });
	},
	
	latin_navigate : function (uri) {
		// main div
		var url = this.adaptMMTURI(uri, '', true);
		this.ajaxReplaceIn(url, 'main');
		// cross references
		/* var refurl = catalog + '/:query/incoming?' + uri;
		   ajaxReplaceIn(refurl, 'crossrefs');
		   $('#crossrefs').jstree({
		   "core" : {"animation": 0},
		   "themes" : {"theme" : "classic", "icons" : false},
		   "plugins" : ["html_data", "themes", "ui", "hotkeys"]
		   }); */
		// breadcrumbs
		var bcurl = '/:breadcrumbs?' + uri;
		this.ajaxReplaceIn(bcurl, 'breadcrumbs');
	},
	
	load : function (elem) {
		var url = this.adaptMMTURI(elem.getAttribute('jobad:load'), '', true);
		var res = null;
		$.ajax({ 'url': url,
				 'dataType': 'xml',
				 'async': false,
				 'success': function cont(data) {res = data;}
			   });
		//proxyAjax('get', url, '', cont, false, 'text/xml');
		elem.removeAttribute('jobad:load');
		return res.firstChild;
	},
	
	leftClick: function(target, JOBADInstance) {
	   	//handling clicks on parts of the document - active only for elements that have jobad:href
		if(target.hasAttribute('loadable')) {
			var elem = target.parent().get(0);
			var ref = this.load(elem);
			$(ref).find('span').removeAttr('onclick');
			$(ref).find('span').attr('foldable', 'true');
			$(elem).replaceWith(ref);
		}
		if(target.hasAttribute('foldable')) {
			var content = $(target).parent().find('table').toggle();				
		}
		if (target.hasAttribute('jobad:href')) {
			var mr = $(target).closest('mrow');
			var select = (mr.length == 0) ? target[0] : mr[0];
			this.setSelected(select);
			return true;
		}
		// highlight bracketed expression
		if (this.getTagName(target[0]) == 'mfenced') {
			this.setSelected(target[0]);
			return true;
		}
		// highlight variable declaration
		if (target.hasAttribute('jobad:varref')) {
			/* var v = $(target).parents('mrow').children().filter('[jobad:xref=' +  target.attr('jobad:varref') + ']');
			   this.setSelected(v[0]);*/
			alert("Unsupported");
			return true;
		}
		
		this.unsetSelected();	
		return true;	//we did stuff also
	},
	
	
	hoverText: function(target, JOBADInstance) {
		//handling clicks on parts of the document - active only for elements that have jobad:href
		
		if (target.hasAttribute('jobad:href')) {
			var mr = $(target).closest('mrow');
			var select = (mr.length == 0) ? target : mr[0];
			this.setSelected(select);
			return target.attr('jobad:href');
		}
		// bracketed expression
		if (this.getTagName(target) == 'mfenced') {
			this.setSelected(target);
			return true;
		}
		// variable declaration
		if (target.hasAttribute('jobad:varref')) {
			var v = $(target).parents('mrow').children().filter('[jobad:xref=' +  target.attr('jobad:varref') + ']');
			this.setSelected(v[0]);
			return true;
		}
		// maybe return false
		return true;
	},
	
	contextMenuEntries: function(target, JOBADInstance) {
		this.focus = target;
		this.focusIsMath = ($(this.focus).closest('math').length !== 0);
		var res = this.visibMenu();
		
		if (this.isSelected(target)) {
			//setCurrentPosition(target);		
			res["infer type"] = this.inferType();
	  		return res;
		} else if ($(target).hasClass('folder') || this.focusIsMath) {
			return res;
		} else {
			return false;
		}
	},
	
	
	/* Helper Functions  */
	getSelectedParent : function (elem){
		var s = $(elem).parents().andSelf().filter('.math-selected');
		if (s.length == 0)
			return elem;
		else
			return s[0];
	},
	
	setVisib : function(prop, val){
		var root = this.focusIsMath ? this.getSelectedParent(this.focus) : this.focus.parentNode;
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
			//		"edit" : edit(),
		}
	},
	
	unsetSelected : function(){
		$('.math-selected').removeClass('math-selected');
	},
	
	isSelected : function(target) {
		target.hasClass("math-selected");
	},
	
	setSelected : function(target){
		this.unsetSelected();
		$(target).addClass('math-selected');
	},
	
	
	/**
	 * getTagPrefix - function that returns the tag prefix of a given element
	 *
	 * @param object : reference to the element whose tag prefix should be determined
	 * @returns returnPrefix : a string value denoting the tag prefix of the given element
	 */
	getTagPrefix : function(object)
	{
		var returnPrefix = ""; //default prefix value
		var tagName = object.tagName;
		var regExpPrefix = /\w*:/;
		returnPrefix = tagName.match(regExpPrefix);
		return returnPrefix;
	},
	
	/**
	 * getTagName - function that returns the tag name of a given element
	 *
	 * @param object : reference to the element whose tag name should be determined
	 * @returns returnTagName : a string value denoting the tag name of the given element
	 */
	getTagName : function(object)
	{
		var returnTagName = ""; //default return value
		if (object == null || object.tagName === undefined) {
			return null;
		}
		var tagNameOriginal = object.tagName;
		var index = tagNameOriginal.indexOf(":", 0);
		returnTagName = tagNameOriginal.substring(index+1);
		return returnTagName;
	}
	
};

JOBAD.modules.register(latin);


