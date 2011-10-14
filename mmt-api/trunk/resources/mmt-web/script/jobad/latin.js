/**
 * The LATIN implementation of the JOBAD service class and functions
 * @author: Florian Rabe, based on previous code by Catalin David
 */

// scheme + authority of the server
var catalog;
// notation style, null if none
var notstyle = 'http://cds.omdoc.org/foundations/lf/mathml.omdoc?twelf';  // hard-coding a default style for LF content

function setStyle(style) {
   notstyle = style;
   $('#currentstyle').text(style);
}
/**
 * adaptMMTURI - convert MMTURI to URL using current catalog and possibly notation style
 * act: String: action to call on MMTURI
 * present: Boolean: add presentation to action
 */
function adaptMMTURI(uri, act, present){
	var arr = uri.split("?");
	var doc = (arr.length >= 1) ? arr[0] : "";
	var mod = (arr.length >= 2) ? arr[1] : "";
	var sym = (arr.length >= 3) ? arr[2] : "";
	if (present && notstyle !== null)
	   var pres = "_present_" + notstyle;
	else
	   var pres = '';
	return catalog + '/:mmt?' + doc + '?' + mod + '?' + sym + '?' + act + pres;
}

function load(elem) {
   var url = adaptMMTURI(elem.getAttribute('jobad:load'), '', true);
   var res = null;
   function cont(data) {res = data;}
   proxyAjax('get', url, '', cont, false, 'text/xml');
   elem.removeAttribute('jobad:load');
   return res.firstChild;
}
function flatClick(elem) {
   var cont = $(elem).children('.flat-container');
   if (elem.hasAttribute('jobad:load')) {
      var m = load(elem);
      cont.append(m);
      cont.toggle();
   } else {
      cont.toggle();
   }
}
function remoteClick(elem) {
   var ref = load(elem);
   $(elem).replaceWith(ref);
}

function ajaxReplaceIn(url, targetid) {
   function cont(data) {
         var targetnode = $('#' + targetid).children('div');
         targetnode.replaceWith(data.firstChild);
   }
   $.ajax({ 'url': url,
            'dataType': 'xml',
            'success': cont
        });
}
function latin_navigate(uri) {
		// main div
		var url = adaptMMTURI(uri, '', true);
		ajaxReplaceIn(url, 'main');
		// cross references
		/* var refurl = catalog + '/:query/incoming?' + uri;
		ajaxReplaceIn(refurl, 'crossrefs');
      $('#crossrefs').jstree({
              "core" : {"animation": 0},
              "themes" : {"theme" : "classic", "icons" : false},
              "plugins" : ["html_data", "themes", "ui", "hotkeys"]
      }); */
		// breadcrumbs
		var bcurl = catalog + '/:breadcrumbs?' + uri;
		ajaxReplaceIn(bcurl, 'breadcrumbs');
}

function browserInit() {
   // path (; at end of path is ignored; maybe use window.location.href and parse properly)
   var path = window.location.pathname;
   // query
   var query = window.location.search.substring(1);
   // will be the requested MMT URI
   var uri = null;
   catalog = window.location.protocol + '//' + window.location.host;
	var i = query.indexOf('present_', 0);
	if (i !== -1) {
      uri = query.substring(0,i-1);
	   setStyle(query.substring(i+8));
	} else {
	   uri = query;
	   setStyle(notstyle);
   }
   if ((path.indexOf("/xhtml") != 0) && (path !== "/;") && (path !== "/")) {
      uri = path + '?' + uri;
   }
   latin_navigate(uri);
}

/**
 * parses and evaluates the formula F inside a jobad:conditional attribute
 * F ::= and(F,F) | or(F,F) | not(F) | p = V | p < V | p > V  
 * @param {Object} str - F (string)
 * @param {Object} arr - enviroment mapping symbols p to values V (boolean, string, or integer)
 */
function cond_parse(str, arr){
	if (str.substr(0, 3) == "and") {
		var p = str.substring(4, str.length - 1);
		var bracket = 0, poz = 0;
		for (var i = 0; i < p.length; i++) {
			if (p.charAt(i) == '(') 
				bracket++;
			else 
				if (p.charAt(i) == ')') 
					bracket--;
				else 
					if (p.charAt(i) == ',' && bracket == 0) {
						poz = i;
						break;
					}
		}
		return (cond_parse(p.substring(0, poz), arr) && cond_parse(p.substring(poz + 1), arr));
	}
	else 
		if (str.substr(0, 2) == "or") {
			var p = str.substring(3, str.length - 1);
			var bracket = 0, poz = 0;
			for (var i = 0; i < p.length; i++) {
				if (p.charAt(i) == '(') 
					bracket++;
				else 
					if (p.charAt(i) == ')') 
						bracket--;
					else 
						if (p.charAt(i) == ',' && bracket == 0) {
							poz = i;
							break;
						}
			}
			return (cond_parse(p.substring(0, poz), arr) || cond_parse(p.substring(poz + 1), arr));
		}
		else 
			if (str.substr(0, 3) == "not") {
				var p = str.substring(4, str.length - 1);
				return (!(cond_parse(p, arr)));
			}
			else 
				if (str.substr(0, 4).toLowerCase() == "true") {
					return true;
				}
				else 
					if (str.substr(0, 5).toLowerCase() == "false") {
						return false;
					}
					else
					   // atomic formulas
						if (str.indexOf('=') != -1) {
							var prop = str.split('=')[0];
							var val = str.split('=')[1];
							if (arr[prop] == null) 
								return false; // undefined formulas go to false, should log a warning here
							else 
								if (arr[prop] == val) 
									return true;
								else 
									return false;
						}
						else 
							if (str.indexOf('<') != -1) {
								var prop = str.split('<')[0];
								var val = str.split('<')[1];
								if (arr[prop] == null) 
									return false;
								else 
									if (arr[prop] < val) 
										return true;
									else 
										return false;
							}
							else 
								if (str.indexOf('>') != -1) {
									//same as before
									var prop = str.split('>')[0];
									var val = str.split('>')[1];
									if (arr[prop] == null) 
										return false;
									else 
										if (arr[prop] > val) 
											return true;
										else 
											return false;
								}
}

// for initialization: wrap all jobad:conditional elements in mactions
function createMactions(root) {
   $(root).find('[jobad:conditional]').filter(function(){
      return (getTagName(this.parent) !== 'maction');
   }).each(function(){
      createMactionElement(null, 'conditional', this);
   });
}

var visibKeys = ["implicit", "reconstructed", "elevel"];
function setKeys(elems, con){
   elems.each(function(index, elem){
      visibKeys.forEach(function(k){
         if (elem.hasAttribute('jobad:' + k))
            con[k] = elem.getAttribute('jobad:' + k);
      });
   });
}

// update visibility status of all elements below target
function updateVisibility(target){
   //build the context: all properties set in ancestors, false by default
   var context = new Array();
   visibKeys.forEach(function(k){context[k] == false});
   //parents is inner->outer, but andSelf inverts
	var ancestors = $(target).parents().andSelf();
   setKeys(ancestors, context);

   //get all maction@conditional descendants, select the first child that is visible, hide if none
	var mactions = $(target).find("maction[actiontype='conditional']");
   mactions.each(function(index, maction) {
      var ancs = $(target).parentsUntil(target).andSelf();
      var cont = clone(context);
      setKeys(ancs, cont);
      var found = false;
      var show = 1;
		$(maction).children().each(function(i, child) {
		    var c = $(child).attr('jobad:conditional');
		    if (c == undefined || cond_parse(c, cont)) {
		       found = true;
		       return false;
		    } else {
		       show++;
		       return true;
		    }
		});
		if (! found) {
  			var ms = document.createElementNS(NS_MATHML, 'mspace');
			maction.appendChild(ms);
      }
		$(maction).attr('selection', show);
	});

   //handle bracket elision: find mfenced with jobad:brackets 
	var mfenceds = $(target).find('mfenced').hasMAttr('jobad:brackets');
	mfenceds.each(function(index, mf){
	   var mfenced = $(mf);
      var ancs = $(target).parentsUntil(target).andSelf();
      var cont = clone(context);
      setKeys(ancs, cont);
	   var visible = cond_parse(mfenced.attr('jobad:brackets'), cont);
		if (visible) {
			mfenced.attr('open', mfenced.attr('jobad:open'));
         mfenced.attr('close', mfenced.attr('jobad:close')); 
		} else {
         mfenced.attr('open', '');
         mfenced.attr('close', '');
      }
	});
}

var latin = clone(Service);

/**
 * Initialize the latin service
 */
latin.init = function(){
	updateVisibility(document.documentElement);
	browserInit();
}

function unsetSelected(){
   $('.math-selected').removeMClass('math-selected');
}
function setSelected(target){
   unsetSelected();
   $(target).addMClass('math-selected');
}
function getSelectedParent(elem){
   var s = $(elem).closest('.math-selected');
   if (s.length == 0)
      return elem;
   else
      return s[0];
}
function isSelected(elem){
   return $(elem).closest('.math-selected').length != 0;
}

latin.leftClick = function(target){
	//handling clicks on parts of the document - active only for elements that have jobad:href
	if (target.hasAttribute('jobad:href')) {
		var mr = $(target).closest('mrow');
		var select = (mr.length == 0) ? target : mr[0];
		setSelected(select);
		return true;
	}
	// highlight bracketed expression
	if (getTagName(target) == 'mfenced') {
		setSelected(target);
		return true;
	}
	// highlight variable declaration
	if (target.hasAttribute('jobad:varref')) {
	   var v = $(target).parents('mrow').children().filterMAttr('jobad:xref', target.getAttribute('jobad:varref'));
		setSelected(v[0]);
		return true;
	}
	unsetSelected();
	return false;
}

latin.hoverText = function(target){
	if (target.hasAttribute('jobad:href')) {
		return target.getAttribute('jobad:href');
	} else
	   return null;
}

/* these are auxiliary variables used to communicate information about the current focus from the context menu entries to the methods; they are not passed as an argument to avoid encoding problems */
//URI of the symbol clicked on
var currentURI = null;
//URI of the OMDoc ContentElement that generated the math object clicked on
var currentElement = null;
//name of the component of currentElement that generated the math object clicked on
var currentComponent = null;
//position of the subobject clicked on within its math object
var currentPosition = null;

function setCurrentPosition(elem){
   var math = $(elem).closest('math');
   currentElement = math.attr('jobad:owner');
   currentComponent = math.attr('jobad:component');
   currentPosition = getSelectedParent(elem).getAttribute('jobad:xref');
}

function quoteSetVisib(prop, val){
   return "setVisib('" + prop + "','" + val + "')";
}
function visibSubmenu(prop) {
   return [["show", quoteSetVisib(prop, 'true')],
		      ["hide", quoteSetVisib(prop, 'false')]
	        ];
}
var visibMenu = [
   ["reconstructed types", '', visibSubmenu('reconstructed')],
   ["implicit arguments", '', visibSubmenu('implicit-arg')],
   ["implicit binders", '', visibSubmenu('implicit-binder')],
   ["redundant brackets", '', visibSubmenu('brackets')],
];
latin.contextMenuEntries = function(target){
   if (isSelected(target)) {
      setCurrentPosition(target);
      return [
         ["infer type", "inferType()"],
      ];
	} else if (target.hasAttribute("jobad:href")) {
		currentURI = target.getAttribute('jobad:href');
		return [
         ["show type", "showComp('type')"],
         ["show definition", "showComp('definition')"],
         ["(un)mark occurrences", "showOccurs()"],
         ["open in new window", "openCurrent()"],
         ["show URI", "alert('" + currentURI + "')"],
         ["get OMDoc", "openCurrentOMDoc()"],
      ];
	} else if ($(target).hasClass('folder') || focusIsMath)
		return visibMenu;
   else
      return [];
}

function setVisib(prop, val){
   var root = focusIsMath ? getSelectedParent(focus) : focus.parentNode;
   if (val == 'true')
      $(root).find('.' + prop).removeMClass(prop + '-hidden');
   if (val == 'false')
      $(root).find('.' + prop).addMClass(prop + '-hidden');
}

/** opens current URI in a new window as OMDoc */
function openCurrentOMDoc(){
   var url = adaptMMTURI(currentURI, 'xml', false);  
   window.open(url, '_blank', '', false);
}
/** opens current MMT URI in a new window */
function openCurrent(){
	var url = adaptMMTURI(currentURI, '', true);
	window.open(url, '_blank', '', false);
}
/** highlights all occurrences of the current URI */
function showOccurs(){
   var occs = $('mo').filterMAttr('jobad:href', currentURI).toggleMClass('math-occurrence')
}
/** shows a component of the current MMT URI in a dialog */
function showComp(comp){
	var target = adaptMMTURI(currentURI, 'component_' + comp, true);
	if(comp == 'definition')
		proxyAjax('get', target, '', continuationDef, false, 'text/xml');
	if(comp == 'type')
		proxyAjax('get', target, '', continuationType, false, 'text/xml');
}

// helper function to produce xml attributes
function XMLAttr(key, value) {return ' ' + key + '="' + value + '"';}
// helper function to produce xml elements
function XMLElem(tag, content) {return XMLElem1(tag, null, null, content);}
// helper function to produce xml elements with 1 attribute
function XMLElem1(tag, key, value, content) {
  var atts = (key == null) ? "" : XMLAttr(key,value);
  var begin = '<' + tag + atts;
  if (content == null) {
    return begin + '/>';
  } else {
    return begin + '>' + content + '</' + tag + '>';
  }
}

//helper functions to build queries (as XML strings)
function Qindividual(p) {return XMLElem1('individual', 'uri', p);}
function Qcomponent(o, c) {return XMLElem1('component', 'index', c, o);}
function Qsubobject(o, p) {return XMLElem1('subobject', 'position', p, o);}
function Qtype(o) {return XMLElem('type', o);}

/** sends type inference query to server for the currentComponent and currentPosition */
function inferType(){
   var query = Qtype(Qsubobject(Qcomponent(Qindividual(currentElement), currentComponent), currentPosition));
   $.ajax({
       url:'/:query?elem_obj', 
       type:'POST',
       data:query,
       processData:false,
       contentType:'text/xml',
       success:function(data){setLatinDialog(data.firstChild.firstChild, 'type');},
   });
}

function continuationDef (data) {	continuation(data,'definition');}
function continuationType (data) {	continuation(data,'type');}
function continuation(data, comp){
   var split = currentURI.split("?");
   var name = split[split.length - 1];
   var title = name + ((comp == 'type') ? ' : ' : ' = ');
	setLatinDialog(data.firstChild, title);	
}

/*
There are some small UI problems left to fix:
- context menu accessed from within lookup window should be on top of lookup window, currently underneath
- lookup window should not move when scrolling vertically
- title bar should be thinner
- title bar should only show the cd and name component, but not the cdbase of the symbol href (full href should be shown as @title)
 */
function setLatinDialog(content, title){
	var dia = $("#latin-dialog");
	if (dia.length == 0) {
	   dialog_init();
  	   var dia = $("#latin-dialog");
  	}
	dia.dialog('option', 'title', title);
	dia[0].replaceChild(content, dia[0].firstChild);
	dia.dialog('open');
}

function dialog_init(){
	//create and initialize the dialog
	var div = document.createElement('div');
	div.setAttribute("id", "latin-dialog");
	document.body.appendChild(div);
	var span = document.createElement('span');
	div.appendChild(span)
	$('#latin-dialog').dialog({ autoOpen: false});
}

// register service
var latinMod = ['latin', '/script/jobad/services/latin.js', latin, ""];
loadedModules.push(latinMod);

// initialize display
$(latin.init);
