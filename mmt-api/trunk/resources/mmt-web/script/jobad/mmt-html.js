/* Utility functions and state provided for MMT/OMDoc-based html documents */

/** Global Utils */
$.fn.hasAttribute = function(name) {  
	return (typeof this.attr(name) !== 'undefined' && this.attr(name) !== false);
};

var mmt = {
	/* state fields */
  	// focus: holds a reference to the object that was clicked by the user
	focus : null,
	// focus: true if focus is within a math object
	focusIsMath : false,    
	notstyle : 'http://cds.omdoc.org/styles/lf/mathml.omdoc?twelf',  //for now hard-coding a default style for LF content
   	
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
		console.log(elem);
		var math = $(elem).closest('math');
		//console.log(math.wrap("<span></span>").parent().html());
		this.currentElement = math.attr('jobad:owner');
		this.currentComponent = math.attr('jobad:component');
		this.currentPosition = this.getSelectedParent(elem).getAttribute('jobad:mmtref');
		
		console.log(this.currentElement);
		console.log(this.currentComponent);
		console.log(this.currentPosition);
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

	getSelectedParent : function (elem){
		var s = $(elem).parents().andSelf().filter('.math-selected');
		if (s.length == 0)
			return elem;
		else
			return s[0];
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
	getTagPrefix : function(object) {
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
	getTagName : function(object) {
		var returnTagName = ""; //default return value
		if (object == null || object.tagName === undefined) {
			return null;
		}
		var tagNameOriginal = object.tagName;
		var index = tagNameOriginal.indexOf(":", 0);
		returnTagName = tagNameOriginal.substring(index+1);
		return returnTagName;
	},


};

