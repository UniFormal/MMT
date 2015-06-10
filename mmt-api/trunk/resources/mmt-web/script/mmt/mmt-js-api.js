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
};

/* add a class cl to all matched elements */
$.fn.addMClass = function(cl){
   this.each(function(){
      if (this.hasAttribute('class'))
         $(this).attr('class', $(this).attr('class') + ' ' + cl);   
      else
         this.setAttribute('class', cl);
   });
   return this;
};
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
};
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
};
// end $.fn.f functions

/* some common URIs */
var uris = {
   lf : "http://cds.omdoc.org/urtheories?LF",
};

/* special attribute names, must be in sync with info.kwarc.mmt.api.presentation.HTMLAttributes */
var mmtattr = function(){
   var prefix = "data-mmt-";
   return {
	   symref: prefix + "symref",
	   varref: prefix + "varref",
	   source: prefix + "source",
	   owner: prefix + "owner",
	   component: prefix + "component",
	   position: prefix + "position"
   }
}();

var mmt = {
	/* these are auxiliary variables used to communicate information about the 
	 * current focus from the context menu entries to the methods; they are not 
	 * passed as an argument to avoid encoding problems */
    // holds a reference to the clicked object
	target: null,
  	// focus: holds a reference to the selected object that was clicked by the user
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
		this.target = target;
	    var math = $(elem).closest('math')
		this.focusIsMath = (math.length !== 0);
		if (this.focusIsMath) {
		   this.focus = this.getSelectedParent(elem);
	   	this.currentElement = math.attr(mmtattr.owner);
		   this.currentComponent = math.attr(mmtattr.component);
		   this.currentPosition = this.focus.getAttribute(mmtattr.position);
		} else {
		   this.focus = elem
	   	this.currentElement = null;
		   this.currentComponent = null;
		   this.currentPosition = null;
		}
		if (elem.hasAttribute(mmtattr.symref)) {
			mmt.currentURI = elem.getAttribute(mmtattr.symref);
		} else if ($(elem).parent().hasAttribute("xlink:href")) {
			// in SVG graphs, the parent carries the link, attribute currently for legacy SVG
			mmt.currentURI = $(elem).parent().attr("xlink:href");
		} else {
			mmt.currentURI = null;
		}
	},
	
	/* the active theory is used for operations that must be executed relative to a theory, e.g., parsing */
	getActiveTheory : function() {
	   return $('#parseForm #activetheory').val();
	},
	/* sets the active theory
	  @param uri any MMT URI (symbol part is ignored if present; no action if document URI)
	*/
   setActiveTheory : function(uri) {
      var arr = this.splitMMTURI(uri);
      if (arr[1] != "") {
         var thy = arr[0] + '?' + arr[1]
         this.activeTheory = thy;
         $('#parseForm #activetheory').val(thy);
		  $('#searchForm #theory').val(thy);

	  }
   },
   
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
    * splits a URI into the (doc, mod, sym) triple; omitted parts are returned as ""
    */
   splitMMTURI : function(uri) {
   	var arr = uri.split("?");
		var doc = (arr.length >= 1) ? arr[0] : "";
		var mod = (arr.length >= 2) ? arr[1] : "";
		var sym = (arr.length >= 3) ? arr[2] : "";
		return [doc, mod, sym];
	},
	
	/*
	 * adaptMMTURI - convert MMTURI to URL using current catalog and possibly notation style
	 * act: String: additional action to call after "get uri"
	 * present: Boolean: add presentation to action
	 */
	adaptMMTURI : function (uri, act, present) {
		var arr = this.splitMMTURI(uri);
		if (present) {
		   var pres = " present html";
		}
		else
			var pres = '';
		var relativeURL = '/:mmt?get ' + arr[0] + '?' + arr[1] + '?' + arr[2] + ' ' + act + pres + " respond";
		return this.makeURL(relativeURL);
	},
	
	/**
	    * @param url the URL to load from
	    * @param targetid the XML id of the element, where it appends
	    */
	ajaxAppendBox : function (url, targetnode, async) {
			   function cont(data) {
				   var serializer = new XMLSerializer();
				   var xmlString = serializer.serializeToString(data);
				   $(targetnode).append(xmlString);
				}
				if (async == null) async = true;
				$.ajax({  'type': "GET",
					      'url': url,      
						  'dataType': 'xml',
						  'async': async,
						  'success': cont,						  
					   });
			},
			
   /**
    * @param url the URL to load from
    * @param targetid the XML id of the element, whose child to replace with the loaded node
    */
   ajaxReplaceIn : function (url, targetid, async) {
		function cont(data) {
			var targetnode = $('#' + targetid).children();
			
			// adapt code to ignore the graph 
			var cont = data.firstChild;
			$(cont).find(".graph").remove();
			targetnode.replaceWith(cont);

		}
		if (async == null) async = true;
		$.ajax({ 'url': url,
				 'dataType': 'xml',
				 'async': async,
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
	
	sideBarClick : function(event,p) {
	      if (event.detail == 1) navigation.navigate(p);
	      else if (event.detail == 2) {
	         if (graphWindow == null) {
	        	 openGraph(p);
	         }
	         else{
	        	 graphWindow.navigateGraph(p); 
	         }
	      }
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
		dia.dialog('option', 'title', title);
		dia[0].replaceChild(content, dia[0].firstChild);
		dia.dialog('open');
	},
	/**
	 * Creates and inserts an empty inline box after some ancestror of origin.
	 * The ancestor is the closest inlineBoxSibling or #main if none.
	 * The created box can be dragged and resized.
	 * 
	 * @param origin the node to which the inline box belongs
	 * @param title the title of the box
	 * @param width optional string parameter setting the width of the box
	 * @returns the body of the inline box, to which further content can/should be added 
	 */
	createInlineBox: function(origin, title, width) {
	    var tmp = $(origin).closest(".inlineBoxSibling");
	    var container = document.createElement('div');
	    var outerDiv = document.createElement('div');
	    var innerDiv = document.createElement('div');
	    $(container).addClass("container-fluid")
	    $(outerDiv).addClass("container-fluid")
	    $(container).append(outerDiv)
	    $(outerDiv).append(innerDiv)
	    var heightParent;
	    var targetParent;
	    if (tmp.length === 0) {
	    	
	        var list = $("#main").children();
	        if (list.length === 0) {
	        	targetParent = $("#main");
	            $(container).insertAfter("#main");            
	        }
	        else {
	        	$(container).insertBefore(list[0]);
	        }        
	    }
	    else {
	    	targetParent = tmp;
	        $(targetParent).append(container);
	    }
	    var heightParent = $(targetParent).height();
	    if (typeof width == 'string') {
	        $(innerDiv).width(width)
	    }
	    
	    var btnDiv = document.createElement('div');
	    var titleDiv = document.createElement('div');
	    var contentDiv = document.createElement('div');
	    var button_hide = document.createElement('button');
	    var button_close = document.createElement('button');
	    $(innerDiv).addClass("panel panel-default");
	    $(innerDiv).addClass("bigDiv");
	    $(titleDiv).addClass("bg-primary")
	    $(btnDiv).addClass("panel-heading");
	    $(contentDiv).addClass("contDiv");
	    $(titleDiv).append(btnDiv);
	    $(innerDiv).append(titleDiv);
	    $(innerDiv).append(contentDiv);
	    $(btnDiv).append("<b>" + title + "</b");
	    $(btnDiv).append(button_close);
	    $(btnDiv).append(button_hide);
	    $(button_hide).addClass("btn btn-info btn-xs pull-right")
	    $(button_close).addClass("btn btn-info btn-xs pull-right")
	    $(button_hide).append(
	        "<span class=\" minus glyphicon glyphicon-minus-sign\" aria-hidden=\"true\"></span>"
	    );
	    $(button_close).append(
	        "<span class=\"glyphicon glyphicon-remove-sign\" aria-hidden=\"true\"></span>"
	    )
	    //TODO use snap options to improve dragging behavior
	    $(outerDiv).draggable({
	        handle: titleDiv,
	        cursor: "move"
	    });
	    $(innerDiv).resizable({
	        minHeight: 50,
	        minWidth: 250
	    })
	    var h = $(titleDiv).height();
	    var H = $(innerDiv).height();
	   
	    $(button_close).click(function() {
	        var temp = $(button_close).closest(".panel")[0];
	        $(temp).remove();
	    });
	    
	    var minus = true
	    var newH = 0
	    $(button_hide).click(function() {
	    	var H ;
	        if (minus) {
	        	H = $(innerDiv).height();
		    	newH = H
	        	var h = $(titleDiv).height();
	    	    $(contentDiv).hide();
		    	$(innerDiv).height(h)		        
	            $(".minus").switchClass("glyphicon-minus-sign",
	                "glyphicon-plus-sign");
	            minus = false
	        }
	        else {
	        	$(innerDiv).height(newH)
	        	$(contentDiv).show();	        
	            $(".minus").switchClass("glyphicon-plus-sign",
	                "glyphicon-minus-sign");
	            minus = true
	        }
	    });
	    // detect if element is dragged
	    var isDragging = false;
	    $(titleDiv)
	    .mousedown(function() {
	        $(window).mousemove(function() {
	            isDragging = true;
		    	$(container).height(0);
	            $(window).unbind("mousemove");
	        });
	    })
	    .mouseup(function() {
	        var wasDragging = isDragging;
	        isDragging = false;
	        $(window).unbind("mousemove");
	    });
	    
	    // scroll to the new inline box
	    var el = $(contentDiv);
	    var elOffset = el.offset().top;
	    var elHeight = el.height();
	    var windowHeight = $(window).height();
	    var offset;
	    if (elHeight < windowHeight) {
	        offset = elOffset - ((windowHeight / 2) - (elHeight / 2));
	    }
	    else {
	        offset = elOffset;
	    }
	    var speed = 400;
	    $('html, body').animate({
	        scrollTop: offset
	    }, speed);
	    return contentDiv;
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
	// helper function to produce xml elements with 0-2 attributes:
	// <tag key1="value1" key2="value">content</tag>
	// all arguments except tag can be null
	elem : function (tag, content, key1, value1, key2, value2) {
		var att1 = (key1 == null) ? "" : this.attr(key1,value1);
		var att2 = (key2 == null) ? "" : this.attr(key2,value2);
		var atts = att1 + att2;
		var begin = '<' + tag + atts;
		if (content == null) {
			return begin + '/>';
		} else {
			return begin + '>' + content + '</' + tag + '>';
		}
	},
};

var qmtAux = {
	// returns a binary convenience function that returns a QMT query expression for a unary atomic function
	// the second argument is an MMT URI that the unary function is parametrized by 
	// defaultParam: a function returning the default value of the second argument of the returned function
	extensionFunction : function(name, defaultParam) {
	   return function(o, param) {
	      var p = (param == null) ? ((defaultParam == null) ? mmt.getActiveTheory() : defaultParam()) : param; 
	      return XML.elem('function', o, 'name', name, 'param', p);
	   };
   },
};

// functions to build and run QMT queries
var qmt = {
   // helper functions to build queries (as XML strings)
	literalPath : function (p) {return XML.elem('literal', null, 'uri', p);},
	literalString : function (p) {return XML.elem('literal', p);},
	bound      : function(i) {return XML.elem('bound', null, 'index', i);},
	component  : function (o, c) {return XML.elem('component', o, 'index', c);},
	subobject  : function (o, p) {return XML.elem('subobject', o, 'position', p);},
	tuple       : function(os) {return XML.elem('tuple', os);},
	projection  : function(o, i) {return XML.elem('projection', o, 'index', i);},
	let         : function(v, i) {return XML.elem('let', v + i);},
	parse       : qmtAux.extensionFunction('parse'),
	infer       : qmtAux.extensionFunction('infer'),
	simplify    : qmtAux.extensionFunction('simplify'),
	analyze     : qmtAux.extensionFunction('analyze'),
	present     : qmtAux.extensionFunction('present', function(){return "html";}),
	presentDecl : qmtAux.extensionFunction('presentDecl', function(){return "html";}),

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

//functions to build and run MMT actions
var action = {
	// helper functions to build actions (as strings)
    build: function(a,t,p) {return "build " + a + " " + t + (p == null? "" : " " + p);},
	exit: "exit",

	/* executes an action (as constructed by helper functions) via ajax and runs a continuation on the result */
	exec : function(a, cont) {
		$.ajax({
			url: mmt.makeURL('/:action') + "?" + a,
            dataType : 'text',
			success:cont,
		});
	},

 };

$(function(){$('#latin-dialog').dialog({ autoOpen: false})});

/** function called by generated interaction elements */
var interaction = {
   /** click on a togglable element */
   toggleClick: function(elem,label){
      var cls = label != null ? label : 'toggleTarget';
      $(elem).parent().closest('div').find('.' + cls).toggle();
   },
   /** click on a search result */
   resultClick: function(p){
      navigation.navigate(p);
   },
}


