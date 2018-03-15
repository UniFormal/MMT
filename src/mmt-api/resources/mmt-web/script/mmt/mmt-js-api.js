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

/* special attribute names, must be kept in sync with info.kwarc.mmt.api.presentation.HTMLAttributes, see there for documentation */
var mmtattr = function(){
   var prefix = "data-mmt-";
   var jax = "data-mmtjax-";
   return {
      symref: prefix + "symref",
      varref: prefix + "varref",
      source: prefix + "source",
      owner: prefix + "owner",
      component: prefix + "component",
      position: prefix + "position",
      href: prefix + "href",
      load: prefix + "load",
      toggleTarget: prefix + "toggle",
	  theory: jax + "theory",
	  inFormat: jax + "informat",
	  outFormat: jax + "outformat"
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
   // focus: true if focus is within an svg object
   focusIsSVG : false,
   //mmtattr.href of the object clicked on (if any)
   currentURI : null,
   //URI of the OMDoc ContentElement that generated the math object clicked on
   currentElement : null,
   //name of the component of currentElement that generated the math object clicked on
   currentComponent : null,
   //position of the subobject clicked on within its math object
   currentPosition : null,

   /* set focus, focusIsMath, currentURI, currentElement, currentComponent, currentPosition according to elem */
   setCurrentPosition : function(elem){
      this.target = elem;
      var math = $(elem).closest('math');
      this.focusIsMath = (math.length !== 0);
      var svg = $(elem).closest('svg')
      this.focusIsSVG = svg.length !== 0;
      if (this.focusIsMath) {
         this.focus = this.getSelectedParent(elem);
         this.currentElement = math.attr(mmtattr.owner);
		 this.setActiveTheory(this.currentElement);
         this.currentComponent = math.attr(mmtattr.component);
		 var pos = this.focus.getAttribute(mmtattr.position);
         this.currentPosition = pos == null ? "" : pos; // empty attributes are suppressed by HTML rendering
      } else {
         this.focus = elem
         this.currentElement = null;
         this.currentComponent = null;
         this.currentPosition = null;
      }
      var uriAttrs = [mmtattr.symref, mmtattr.href, mmtattr.load]
      var noUriFound = uriAttrs.every(function(attr) {
         if (elem.hasAttribute(attr)) {
            mmt.currentURI = elem.getAttribute(attr);
            return false;
         } else
            return true;
      })
      if (noUriFound) {
         // special case for labels in SVG, where the parent carries the link
         var par = elem.parentNode;
         if (par.hasAttribute(mmtattr.symref)) {
            mmt.currentURI = par.getAttribute(mmtattr.symref);
         } else {
            mmt.currentURI = null;
         }
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
         return relUrl.substring(1);
      }
   },

   /*
    * splits a URI into the (doc, mod, sym) triple; omitted parts are returned as ""
    */
   splitMMTURI : function(uri, includeEmpty) {
      if (typeof includeEmpty === "undefined"){
        includeEmpty = true;
      }

      var arr = uri.split("?");
      var doc = (arr.length >= 1) ? arr[0] : "";
      var mod = (arr.length >= 2) ? arr[1] : "";
      var sym = (arr.length >= 3) ? arr[2] : "";
      var arr = [doc, mod, sym];
      if (!includeEmpty) {
         while (arr.length > 0 && arr.slice(-1)[0] == "") arr.pop();
      }
      return arr;
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
         var cont = data.firstChild;
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
      if (elem.hasAttribute(mmtattr.load)) {
         var url = this.adaptMMTURI(elem.getAttribute(mmtattr.load), '', true);
         var res = null;
         $.ajax({ 'url': url,
                'dataType': 'xml',
                'async': false,
                'success': function cont(data) {res = data;}
               });
         elem.removeAttribute(mmtattr.load);
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
      var url = this.currentURI;
      window.open("/?"+url, '_blank', '', false);
   },

   sideBarClick : function(event,p) {
         if (event.detail == 1) interactiveViewing.navigate(p);
         else if (event.detail == 2) {
            if (graphWindow == null || typeof graphWindow == "undefined") {
             openGraph(p);
            }
            else{
             graphWindow.navigateGraph(p);
            }
         }
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
   }
};

// helper functions to build XML elements as strings (used by qmt)
var XML = {
   // helper function to produce xml attributes: key="value"
   attr : function (key, value) {return ' ' + key + '="' + value + '"';},

   // helper function to produce xml elements with as many attributes as needed
   // <tag key1="value1" key2="value">content</tag>
   // all arguments except tag can be null
   elem : function (tag, content/*, key1, value1, key2, value2, ... */) {
      var atts = '';

      // add all the key / value pairs
      var key, value;
      for(var i = 2; i < arguments.length; i+=2) {
        key = arguments[i];
        value = arguments[i + 1];
        atts += (key == null)?"":this.attr(key, value);
      }

      // write the begin and ending tags
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
    queryFunctionApply : function(name, defaultParam) {
      return function(o, param) {
         var p = (param == null) ? ((defaultParam == null) ? mmt.getActiveTheory() : defaultParam()) : param;
         return XML.elem('function', o, 'name', name, 'param', p);
      };
    },

    // a function that always returns the same constant
    c : function(n){return function(){return n};},

    // identity function
    i : function(e){return e;}

};

// functions to build and run QMT queries
var qmt = {
    // Query
    I               : function(q, h){return XML.elem('i', q, 'hint', h);},
    Bound           : function(n){return XML.elem("bound", null, "index", n);},
    Component       : function(o,i){return XML.elem("component", o, "index", i);},
    SubObject       : function(o,p){return XML.elem("subobject", o, "position", p);},
    Related         : function(t,b){return XML.elem("related", t+b);},
    Literal         : function(l){return XML.elem("literal", l);},
    Literals        : function(ls){return XML.elem("literals", ls.join(""));},
    Let             : function(n, v, i){return XML.elem("let", v+i, "name", n);},
    Singleton       : function(e){return XML.elem("singleton", e);},
    Paths           : function(c){return XML.elem("uris", null, "concept", c);},
    Unifies         : function(w){return XML.elem("unifies", w);},
    Closure         : function(of){return XML.elem("closure", of);},
    Union           : function(l,r){return XML.elem("union", l + r);},
    BigUnion        : function(n,d,s){return XML.elem("bigunion", d+s, "name", n);},
    Intersection    : function(l,r){return XML.elem("intersection", l+r);},
    Difference      : function(l,r){return XML.elem("difference", l+r);},
    Comprehension   : function(n,d,p){return XML.elem("comprehension", d+p, "name", n)},
    Tuple           : function(cs){return XML.elem("tuple", cs.join(""));},
    Projection      : function(t,i){return XML.elem("projection", t, "index", i);},

    // ComponentKey
    TypeComponent           : qmtAux.c("type"),
    DefComponent            : qmtAux.c("def"),
    DomComponent            : qmtAux.c("domain"),
    CodComponent            : qmtAux.c("codomain"),
    ParamsComponent         : qmtAux.c("params"),
    PatternBodyComponent    : qmtAux.c("pattern-body"),
    MetaDataComponent       : qmtAux.c("metadata"),
    OtherComponent          : function(s){return "ext-"+s;},

    // Position
    Position    : function(l){return l.join("_");},

    // RelationExp
    Sequence    : function(qs){return XML.elem("sequence", qs.join(""));},
    Choice      : function(qs){return XML.elem("choice", qs.join(""));},
    Transitive  : function(r){return XML.elem("transitive", r);},
    Reflexive   : function(){return XML.elem("reflexive");},
    Inverse     : function(r){return XML.elem("inverse", r);},
    ToObject    : function(r){return XML.elem("toobject", null, "relation", r);},
    ToSubject   : function(r){return XML.elem("tosubject", null, "relation", r);},

    // Unary
    IsDocument  : qmtAux.c("document"),
    IsTheory    : qmtAux.c("theory"),
    IsView      : qmtAux.c("view"),
    IsStyle     : qmtAux.c("style"),
    IsStructure : qmtAux.c("structure"),
    IsConstant  : qmtAux.c("constant"),
    IsPattern   : qmtAux.c("pattern"),
    IsInstance  : qmtAux.c("instance"),
    IsConAss    : qmtAux.c("conass"),
    IsStrAss    : qmtAux.c("strass"),
    IsNotation  : qmtAux.c("notation"),
    CustomUnary : qmtAux.i,


    // Binary
    DependsOn       : qmtAux.c("DependsOn"),
    HasMeta         : qmtAux.c("HasMeta"),
    Includes        : qmtAux.c("Includes"),
    HasDomain       : qmtAux.c("HasDomain"),
    HasCodomain     : qmtAux.c("HasCodomain"),
    IsInstanceOf    : qmtAux.c("IsInstanceOf"),
    RefersTo        : qmtAux.c("RefersTo"),
    Declares        : qmtAux.c("Declares"),
    IsAliasFor      : qmtAux.c("IsAliasFor"),
    IsAlignedWith   : qmtAux.c("IsAlignedWith"),
    CustomBinary    : qmtAux.i,

    // BaseType
    Object  : function(o){return XML.elem("object", o);},
    XML     : function(x){return XML.elem("xml", x);},
    String  : function(s){return XML.elem("string", s)},
    URI     : function(u){return XML.elem("uri", null, "path", u);},

    // Prop
    IsA         : function(e){return XML.elem("isa", e);},
    PrefixOf    : function(s,l){return XML.elem("prefixof", s+l);},
    IsIn        : function(e,f){return XML.elem("isin", e+f);},
    IsEmpty     : function(e){return XML.elem("isempty", e);},
    Equal       : function(l,r){return XML.elem("equal", l+r);},
    And         : function(l,r){return XML.elem("and", l+r);},
    Or          : function(l,r){return XML.elem("or", l+r);},
    Not         : function(e){return XML.elem("not", e);},
    Forall      : function(n,d,f){return XML.elem("forall", d+f, "name", n);},
    Exists      : function(n,d,f){return XML.elem("exists", d+f, "name", n);},

    // other helpers
    literalPath     : function(p){return qmt.Literal(qmt.URI(p));},
    literalString   : function(s){return qmt.Literal(qmt.String(s));},

    // queryFunctionApply
    parse       : qmtAux.queryFunctionApply('parse'),
    infer       : qmtAux.queryFunctionApply('infer'),
    simplify    : qmtAux.queryFunctionApply('simplify'),
    align       : qmtAux.queryFunctionApply('align'),
    analyze     : qmtAux.queryFunctionApply('analyze'),
    present     : qmtAux.queryFunctionApply('present', function(){return "html";}),
    presentDecl : qmtAux.queryFunctionApply('presentDecl', function(){return "html";}),

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
   }
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
   }
};

var inlineBox = {
   /**
    * Inserts an empty inline box after the closest .inlineBoxSibling ancestor of 'origin'.
    * #main is used if no such ancestor exists.
    * The created box can be dragged and resized.
    *
    * @param origin the node to which the inline box belongs
    * @param title the title of the box
    * @param width optional string parameter setting the width of the box
    * @returns the body of the inline box, to which further content can/should be added
    */
   create: function(origin, title, width) {
       // we append the box after inlBoxSib
       var inlBoxSib = $(origin).closest(".inlineBoxSibling");
       // the box exists inside a container
       var container = document.createElement('div');
       var box = document.createElement('div');
       $(box).addClass("inlinebox toggle-root");
       $(container).append(box);
       // append the box
       if (inlBoxSib.length === 0) {
           // fall back if no inlBoxSib found
           var list = $("#main").children();
           if (list.length === 0) {
             $(container).insertAfter("#main");
           }
           else {
             $(container).insertBefore(list[0]);
           }
       } else {
           $(inlBoxSib).append(container);
       }
       if (typeof width == 'string') {
           $(box).width(width);
       }
       // the inner div contains titleDiv and contentDiv; the latter will be returned
       // the titleDiv contains btnDiv, which contains the control buttons
       var titleDiv = document.createElement('div');
       $(titleDiv).addClass("inlinebox-title")
       $(titleDiv).append("<span>" + title + "</span>");
       $(titleDiv).attr(mmtattr.toggleTarget, "inlinebox-content")
       $(box).append(titleDiv);
       var contentDiv = document.createElement('div');
       $(contentDiv).addClass("inlinebox-content");
       $(box).append(contentDiv);
       var btnDiv = document.createElement('div');
       $(btnDiv).addClass("inlinebox-buttons");
       $(titleDiv).append(btnDiv);
       var button_drag = document.createElement('span');
       $(button_drag).addClass("inlinebox-button-drag")
       $(btnDiv).append(button_drag);
       var button_close = document.createElement('span');
       $(button_close).addClass("inlinebox-button-close")
       $(btnDiv).append(button_close);
       // add click handlers for the buttons
       $(button_close).click(function() {
           $(container).remove();
       });
       var dragged = false;
       $(box).draggable({
           handle: btnDiv,
           cursor: "move",
           // remove container on first drag
           start: function() {
             if (!dragged) {
                $(container).height(0);
                dragged = true;
             }
           }
       });
       // scroll to the new inline box
       box.scrollIntoView(false);
       // return the content div
       return contentDiv;
   },
};

var svgHelper = {
   multiplyDim: function(dim, by) {
      var ms = dim.match(/([\.\d]+)(\D*)/);
      var num = parseFloat(ms[1]);
      var unit = ms[2];
      return (num*by).toString() + unit;
   },
   zoom: function(svg, by) {
      var h = $(svg).attr('height');
      $(svg).attr('height', this.multiplyDim(h, by));
      var w = $(svg).attr('width');
      $(svg).attr('width', this.multiplyDim(w, by));
   },
};

var mmteval = {
  exec: function(server, theory, text, inFormat, outFormat, continuation) {
   var query = "theory=" + theory + "&inFormat=" + inFormat + "&outFormat=" + outFormat;
    $.ajax({
      'url': server + ":content/eval?" + query,
      'type': "post",
      'contentType': "text/plain",
      'processData': false,
      'data': text,
      'success': continuation,
    });
  }
}


