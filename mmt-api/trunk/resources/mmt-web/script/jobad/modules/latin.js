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
	
	/* these are auxiliary variables used to communicate information about the current focus from the context menu entries to the methods; they are not passed as an argument to avoid encoding problems */
	//URI of the symbol clicked on
	currentURI : null,
	//URI of the OMDoc ContentElement that generated the math object clicked on
	currentElement : null,
	//name of the component of currentElement that generated the math object clicked on
	currentComponent : null,
	//position of the subobject clicked on within its math object
	currentPosition : null,
	
	setCurrentPosition : function (elem){
		var math = $(elem).closest('math');
		this.currentElement = math.attr('jobad:owner');
		this.currentComponent = math.attr('jobad:component');
		this.currentPosition = this.getSelectedParent(elem).getAttribute('jobad:xref');
	},
	

	/* JOBAD Interface  */ 
	
	info: {
		'identifier' : 'kwarc.latin.main',
		'title' : 'Main LATIN Service',
		'author': 'MMT developer team',
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
		this.navigate(query);
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
	
	navigate : function (uri) {
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
        if(target.hasAttribute('mmtlink')) {
			var uri = target.attr('mmtlink');
			console.log(uri);
			this.navigate(uri);
		}
		if(target.hasAttribute('loadable')) {
			var elem = target.parent().get(0);
			var ref = this.load(elem);
			$(ref).find('span').removeAttr('onclick'); //hack, should be removed in the mmt/style
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
			/* var v = $(target).parents('mrow').children().filter('[jobad:mmtref=' +  target.attr('jobad:varref') + ']');
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
			var v = $(target).parents('mrow').children().filter(function() {
                           return $(this).attr('jobad:mmtref') == target.attr('jobad:varref');
			})
			this.setSelected(v[0]);
			return true;
		}
		// maybe return false
		return true;
	},
	
	contextMenuEntries: function(target, JOBADInstance) {
		this.focus = target;
		console.log(this.focus);
		this.focusIsMath = ($(this.focus).closest('math').length !== 0);
		var res = this.visibMenu();

		if (this.focusIsMath) {
			this.setCurrentPosition(target);
			this.focus = this.getSelectedParent(target)
			res["infer type"] = this.inferType();
	  		return res;
			if (target.hasAttribute("jobad:href")) {
				this.currentURI = target.getAttribute('jobad:href');
				res["show type"] =  this.showComp('type');
				res["show definition"] =  this.showComp('definition');
				res["(un)mark occurrences"] =  this.showOccurs();
				res["open in new window"] = this.openCurrent();
				res["show URI"] =  alert(currentURI);
				res["get OMDoc"] = this.openCurrentOMDoc();
			}
		} else if ($(target).hasClass('folder') || this.focusIsMath) {
			return res;
		} else {
			return false;
		}
	},
	
	/* Second Menu Dependencies */
	/** opens current URI in a new window as OMDoc */
	openCurrentOMDoc : function () {
		var url = this.adaptMMTURI(currentURI, 'xml', false);  
		window.open(url, '_blank', '', false);
	},

	/** opens current MMT URI in a new window */
	openCurrent : function () {
		var url = this.adaptMMTURI(currentURI, '', true);
		window.open(url, '_blank', '', false);
	},

	/** highlights all occurrences of the current URI */
	showOccurs : function (){
		var occs = $('mo').filterMAttr('jobad:href', currentURI).toggleMClass('math-occurrence')
	},
	
	// helper function to produce xml attributes: key="value"
	XMLAttr : function (key, value) {return ' ' + key + '="' + value + '"';},
	// helper function to produce xml elements: <tag>content</tag> or <tag/>
	XMLElem : function (tag, content) {return XMLElem1(tag, null, null, content);},
	// helper function to produce xml elements with 1 attribute: <tag key="value">content</tag> or <tag key="value"/>
	XMLElem1 : function (tag, key, value, content) {
		var atts = (key == null) ? "" : this.XMLAttr(key,value);
		var begin = '<' + tag + atts;
		if (content == null) {
			return begin + '/>';
		} else {
			return begin + '>' + content + '</' + tag + '>';
		}
	},
	
	//helper functions to build queries (as XML strings)
	Qindividual : function (p) {return this.XMLElem1('individual', 'uri', p);},
	Qcomponent : function (o, c) {return this.XMLElem1('component', 'index', c, o);},
	Qsubobject : function (o, p) {return this.XMLElem1('subobject', 'position', p, o);},
	Qtype : function (o,meta) {return this.XMLElem1('type', 'meta', meta, o);},
	QtypeLF : function (o) {return this.Qtype(o, 'http://cds.omdoc.org/foundations?LF');},
	Qpresent : function (o) {return this.XMLElem1('present', 'style', this.notstyle, o);},

	/** sends type inference query to server for the currentComponent and currentPosition */
	inferType : function (){
		var query = this.Qpresent(this.QtypeLF(this.Qsubobject(this.Qcomponent(this.Qindividual(this.currentElement), this.currentComponent), this.currentPosition)));
		this.execQuery(query,
				  function(result) {this.setLatinDialog(result.firstChild.firstChild.firstChild, 'type');}
				 );
	},
	
	/** shows a component of the current MMT URI in a dialog */
	showComp : function (comp) {
		var query = this.Qpresent(this.Qcomponent(this.Qindividual(this.currentURI), comp));
		execQuery(query,
				  function(result){this.setLatinDialog(result.firstChild.firstChild.firstChild, comp);}
				 );
	},
	
	execQuery : function (q, cont) {
		$.ajax({
			url:'/:query', 
			type:'POST',
			data:q,
			processData:false,
			contentType:'text/xml',
			success:cont,
		});
	},
	
	/*
	  There are some small UI problems left to fix:
	  - context menu accessed from within lookup window should be on top of lookup window, currently underneath
	  - lookup window should not move when scrolling vertically
	  - title bar should be thinner
	  - title bar should only show the cd and name component, but not the cdbase of the symbol href (full href should be shown as @title)
	*/
	setLatinDialog : function (content, title){
		var dia = $("#latin-dialog");
		if (dia.length == 0) {
			this.dialog_init();
  			var dia = $("#latin-dialog");
  		}
		dia.dialog('option', 'title', title);
		dia[0].replaceChild(content, dia[0].firstChild);
		dia.dialog('open');
	},
	
	dialog_init : function (){
		//create and initialize the dialog
		var div = document.createElement('div');
		div.setAttribute("id", "latin-dialog");
		document.body.appendChild(div);
		var span = document.createElement('span');
		div.appendChild(span)
		$('#latin-dialog').dialog({ autoOpen: false});
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


