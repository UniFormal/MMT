/* Utility functions and state provided for MMT/OMDoc-based html documents */

// the following functions $.fn.f add functionality to jQuery and can be used as $(...).f

// contrary to the built-in jQuery analogues, these work for 'pref:name' attributes and math elements
// do not replace calls to these function with the jQuery analogues!

$.fn.hasAttribute = function(name) {  
	return (typeof this.attr(name) !== 'undefined' && this.attr(name) !== false);
};

/* helper function for the methods below: gets the classes of an element as an array */
function getClassArray(elem) {
   var classes = (elem.hasAttribute('class')) ? elem.getAttribute('class') : "";
   return classes.split(/\s+/);
}

/* add a class cl to all matched elements */
$.fn.addMClass = function(cl){
   this.each(function(){
      if (this.hasAttribute('class'))
         $(this).attr('class', $(this).attr('class') + ' ' + cl);   
      else
         this.setAttribute('class', cl);
   });
   return this;
}
/* remove a class cl from all matched elements */
$.fn.removeMClass = function(cl){
   this.each(function(){
      var classes = getClassArray(this);
      var newclasses = classes.filter(function(elem){return (elem !== cl) && (elem !== "")});
      var newclassesAttr = newclasses.join(' ');
      if (newclassesAttr == "")
         $(this).removeAttr('class');
      else
         this.setAttribute('class', newclassesAttr);
   });
   return this;
}
/* toggle class cl in all matched elements */
$.fn.toggleMClass = function(cl){
   this.each(function(){
      var classes = getClassArray(this);
      if (classes.indexOf(cl) == -1)
         $(this).addMClass(cl);
      else
         $(this).removeMClass(cl);
   });
   return this;
}
/* keep elements that have class cl */
$.fn.filterMClass = function(cl){
   return this.filter(function(){
      var classes = getClassArray(this);
      return (classes.indexOf(cl) !== -1)
   });
}
// end $.fn.f functions


/* some common URIs */
var uris = {
   lf : "http://cds.omdoc.org/urtheories?LF",
   mathmlstyle : "http://cds.omdoc.org/styles/omdoc/mathml.omdoc?html5",
};

var mmt = {
	/* these are auxiliary variables used to communicate information about the current focus from the context menu entries to the methods; they are not passed as an argument to avoid encoding problems */
  	// focus: holds a reference to the object that was clicked by the user
	focus : null,
	// focus: true if focus is within a math object
	focusIsMath : false,    
	//jobad:href of the object clicked on (if any)
	currentURI : null,
	//URI of the OMDoc ContentElement that generated the math object clicked on
	currentElement : null,
	//name of the component of currentElement that generated the math object clicked on
	currentComponent : null,
	//position of the subobject clicked on within its math object
	currentPosition : null, 
	
	/* set focus, focusIsMath, currentURI, currentElement, currentComponent, currentPosition according to elem */
	setCurrentPosition : function(elem){
	   var math = $(elem).closest('math')
		this.focusIsMath = (math.length !== 0);
		if (this.focusIsMath) {
		   this.focus = this.getSelectedParent(elem);
	   	this.currentElement = math.attr('jobad:owner');
		   this.currentComponent = math.attr('jobad:component');
		   this.currentPosition = this.focus.getAttribute('jobad:mmtref');
		} else {
		   this.focus = elem
	   	this.currentElement = null;
		   this.currentComponent = null;
		   this.currentPosition = null;
		}
		if (elem.hasAttribute("jobad:href")) {
			mmt.currentURI = elem.getAttribute('jobad:href');
		} else {
		   mmt.currentURI = null;
		}
	},
	
	notstyle : uris.mathmlstyle,

   /*
	* Converts a relative to an absolute url if the base url is set.
    * Necessary when used within another application to connect with external mmt server (e.g. in planetary)
    */
    makeURL : function(relUrl) {
		if ((typeof mmtUrl) != 'undefined') {
			return mmtUrl + relUrl; //compute absolute uri to external mmt server
		} else { 
			return relUrl;
		}	
	},

	/*
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
        var relativeURL = '/:mmt?' + doc + '?' + mod + '?' + sym + '?' + act + pres;
		return this.makeURL(relativeURL);
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
	   if (elem.hasAttribute('jobad:load')) {
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
      }
	},
	

	/** opens current URI in a new window as OMDoc */
	openCurrentOMDoc : function () {
		var url = this.adaptMMTURI(this.currentURI, 'xml', false);  
		window.open(url, '_blank', '', false);
	},
	
	/** opens current MMT URI in a new window */
	openCurrent : function () {
        console.log(this);
		var url = this.adaptMMTURI(this.currentURI, '', true);
		window.open(url, '_blank', '', false);
	},
	

	/*
	  There are some small UI problems left to fix:
	  - context menu accessed from within lookup window should be on top of lookup window, currently underneath
	  - lookup window should not move when scrolling vertically
	  - title bar should be thinner
	  - title bar should only show the cd and name component, but not the cdbase of the symbol href (full href should be shown as @title)
	*/
	setLatinDialog : function (content, title){
        console.log("got here");
		var dia = $("#latin-dialog");
		dia.dialog('option', 'title', title);
		dia[0].replaceChild(content, dia[0].firstChild);
		dia.dialog('open');
	},
	
	getSelectedParent : function (elem){
		var s = $(elem).parents().andSelf().filterMClass('math-selected');
		if (s.length == 0)
			return elem;
		else
			return s[0];
	},
	
	unsetSelected : function(){
		$('.math-selected').removeMClass('math-selected');
	},
	
	isSelected : function(target) {
		$(target).filterMClass("math-selected").length !== 0;
	},
	
	setSelected : function(target){
		this.unsetSelected();
		$(target).addMClass('math-selected');
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

// helper functions to build XML elements as strings (used by qmt)
var XML = {
   // helper function to produce xml attributes: key="value"
	attr : function (key, value) {return ' ' + key + '="' + value + '"';},
	// helper function to produce xml elements: <tag>content</tag> or <tag/>
	elem : function (tag, content) {return this.elem1(tag, null, null, content);},
	// helper function to produce xml elements with 1 attribute: <tag key="value">content</tag> or <tag key="value"/>
	elem1 : function (tag, key, value, content) {
		var atts = (key == null) ? "" : this.attr(key,value);
		var begin = '<' + tag + atts;
		if (content == null) {
			return begin + '/>';
		} else {
			return begin + '>' + content + '</' + tag + '>';
		}
	},
};

// functions to build and run QMT queries
var qmt = {
   // helper functions to build queries (as XML strings)
	individual : function (p) {return XML.elem1('individual', 'uri', p);},
	component : function (o, c) {return XML.elem1('component', 'index', c, o);},
	subobject : function (o, p) {return XML.elem1('subobject', 'position', p, o);},
	type : function (o,meta) {return XML.elem1('type', 'meta', meta, o);},
	present : function (o) {return XML.elem1('present', 'style', mmt.notstyle, o);},

	/* executes a QMT query (as constructed by helper functions) via ajax and runs a continuation on the result */
    exec : function (q, cont) {
	   var qUrl = mmt.makeURL('/:query');
		$.ajax({
			url:qUrl, 
			type:'POST',
			data:q,
		    dataType : 'xml',
			processData:false,
			contentType:'text/plain',
			success:cont,
		});
	},
};
