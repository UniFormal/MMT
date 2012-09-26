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
    $('#currentstyle').text(style.split("?").pop());
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
	return '/:mmt?' + doc + '?' + mod + '?' + sym + '?' + act + pres;
}

function load(elem) {
   var url = adaptMMTURI(elem.getAttribute('jobad:load'), '', true);
   var res = null;
   function cont(data) {res = data;}
   proxyAjax('get', url, '', cont, false, 'text/xml');
   elem.removeAttribute('jobad:load');
   return res.firstChild;
}


function edit() {
    var path = currentElement
    var comp = currentComponent
    arr = path.split("?");
    var mod = "";
    var url = "";
    if (arr.length >= 2) {
    	url = "/:mmt?" + arr[0] + "?" + arr[1] + "?" ;
    	if (arr.length == 3) {
    		url += arr[2];
    		if (comp != "" && comp != "name") {
    			url += " component " + comp
    		}
    	}
    	url += "?text";    
    	function cont(data) {
    		console.log(path);
    		console.log(comp);
    		var math = null;
    		$("math").each(function(i,v) {
    			if($(v).attr("jobad:owner") == path && $(v).attr("jobad:component") == comp) {
    				math = $(v).parent();
    			} 
    		});
    		
    		if (math == null) {
    			return false;
    		}
    		var spres = data.split("\n"); 
    		var rows = spres.length;
    		var columns = 20;
    		var i;
    		for (i = 0; i < spres.length; ++i) {
    			if (spres[i].length > columns)
    				columns = spres[i].length
    		}
    	    
    		math.html('<textarea rows="' + rows +'" cols="' + columns + '\">' + data + '</textarea>');
    	    console.log(math);
	        console.log(math.parent());
	        math.parent().append('<button id="save" style="width:20px;height:20px" onClick=compileText("' + path + '", "true")> Save </button>');   			
 //   			"<div class=\"parser-info\" style=\"padding-bottom:5px;\"></div>" + 
 //   			"<button id=\"compile\" onClick=compileText(\"" + path + "\",\"false\") class=\"ui-button ui-widget ui-state-default\"" +
 // 			"role=\"button\">Compile</button>" + 
 //  			"<button id=\"save\" onClick=compileText(\"" + path + "\",\"true\") disabled=\"true\" class=\"ui-button ui-widget ui-state-disabled ui-state-default ui-corner-all ui-button-text-only\"" +
 //   			"role=\"button\" aria-disabled=\"false\">Save</button>" + 
 //   			"<div class=\"parser-response\" style=\"padding-top:5px;\"></div> + "
 //				"");
    		
 			math.parent().find("button").button({
 				icons: {
 					primary: "ui-icon-disk"
 				},
 				text: false
 			});
 			
    		var textarea = math.find("textarea")[0];
    		
    		mycm = CodeMirror.fromTextArea(textarea);
    		mycm.setOption('onChange', function() {
    				var save = $("#save");
    				save.attr("disabled", "disabled");
    				save.addClass("ui-state-disabled")
    		});
    	}
    	$.ajax({
    			'type' : 'get',
    			'url' : url,
    			'dataType' : 'text',
    			'success' : cont,
    			'async' : false
    	});	
    }
}

var mycm;
var invalidPaths = [];

function compileText(mod, save) {
    mycm.save();
    var id = "div#" + RegExp.escape(mod);   
    var text = $(id + " textarea").val()//.replace(/[#]/g, "\\$&");
    var checkedPchanges = $(id + " :checkbox").map(function(value, index) {
	if ($(this).attr("checked")) {
	    return $(this).attr("value");
	}
    }).get().join("\n");
    var cont = function(data) {
//	console.log(data);
	if (data.success == "true") {
	    var saveBtn = $("#save");
	    saveBtn.removeAttr("disabled");
	    saveBtn.removeClass("ui-state-disabled");
	}
	if (save == "true") {
	    $(id + " div.parser-info").html("");
	    if (data.success == "true") {
		invalidPaths = data.pres;
		markInvalids();
		$(id + " div.parser-response").html("<i>Commit Successful</i><br/");
	    } else {
		$(id + " div.parser-response").html("<i>Commit Failed -- Please Try Again</i><br/>");
	    }
	} else {
	    if (data.success == "true") {
		if (data.info.length > 0) {//there are pragmatic changes
		    var pChanges = $.map(data.info, function(value,index) {
			return "<input type=\"checkbox\" name=\"pchange\" value=\"" + value + "\">" + value +"</input>";
		    }).join("<br/>");		    
		    var info = "<i>Found Pragmatic Changes:</i><br/>" + pChanges
		    $(id + " div.parser-info").html(info);
		}
		var pres = "<i>Compile Successful : </i><br/>" + 
		    "<table cellpadding=\"5px\">" + data.pres + "</table>";
		$(id + " div.parser-response").html(pres);
	    } else {
		var pres = "<i>Compile Failed : </i><br/>" + 
		    "<table cellpadding=\"5px\">" + data.pres + "</table>";
		$(id + " div.parser-response").html(pres);
	    }
	}
    }
    $.ajax({
	'type' : 'post',
	'url' : "/:parse?" + mod,
	'data' : {'text' : text, 'save' : save, "pchanges" : checkedPchanges}, 
	'success' : cont
    });
}

RegExp.escape = function(text) {
    return text.replace(/[!\"#$%&'()*+,.\/:;<=>?@[\]^`{|}~]/g, "\\$&");
}

function markInvalids() {
    $("span").each(function(i2,v2) {
	if($(v2).attr("jobad:href")) {
	    $(v2).css("color", "black");
	}
    });
    $.each(invalidPaths, function(i,v) {
	$("span").each(function(i2,v2) {
	    if($(v2).attr("jobad:href") == v) {
		$(v2).css("color", "red");
	    }
	});
    });
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
    markInvalids();
}

function ajaxReplaceIn(url, targetid) {
   function cont(data) {
       var targetnode = $('#' + targetid).children('div');
       targetnode.replaceWith(data.firstChild);
       markInvalids();
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
    var bcurl = '/:breadcrumbs?' + uri;
    ajaxReplaceIn(bcurl, 'breadcrumbs');
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
      return (geTagName(this.parent) !== 'maction');
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
	//updateVisibility(document.documentElement);
    $('#currentstyle').text(notstyle.split("?").pop());
    var query = window.location.search.substring(1);
    latin_navigate(query);
}

function unsetSelected(){
   $('.math-selected').removeMClass('math-selected');
}
function setSelected(target){
   unsetSelected();
   $(target).addMClass('math-selected');
}
function getSelectedParent(elem){
   var s = $(elem).parents().andSelf().filterMClass('math-selected');
   if (s.length == 0)
      return elem;
   else
      return s[0];
}
function isSelected(elem){
   return $(elem).parents().andSelf().filterMClass('math-selected').length != 0;
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
   //handling clicks on parts of the document - active only for elements that have jobad:href
	if (target.hasAttribute('jobad:href')) {
		var mr = $(target).closest('mrow');
		var select = (mr.length == 0) ? target : mr[0];
		setSelected(select);
		return target.getAttribute('jobad:href');
	}
	// bracketed expression
	if (getTagName(target) == 'mfenced') {
		setSelected(target);
		return false;
	}
	// variable declaration
	if (target.hasAttribute('jobad:varref')) {
	   var v = $(target).parents('mrow').children().filterMAttr('jobad:xref', target.getAttribute('jobad:varref'));
		setSelected(v[0]);
		return false;
	}
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
   ["edit", "edit()"],

];
latin.contextMenuEntries = function(target){
   if (isSelected(target)) {
      setCurrentPosition(target);
      return [
         ["infer type", "inferType()"]
      ].concat(visibMenu);
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
// helper function to produce xml attributes: key="value"
function XMLAttr(key, value) {return ' ' + key + '="' + value + '"';}
// helper function to produce xml elements: <tag>content</tag> or <tag/>
function XMLElem(tag, content) {return XMLElem1(tag, null, null, content);}
// helper function to produce xml elements with 1 attribute: <tag key="value">content</tag> or <tag key="value"/>
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
function Qtype(o,meta) {return XMLElem1('type', 'meta', meta, o);}
function QtypeLF(o) {return Qtype(o, 'http://cds.omdoc.org/foundations/lf/lf.omdoc?lf');}
function Qpresent(o) {return XMLElem1('present', 'style', notstyle, o);}

/** sends type inference query to server for the currentComponent and currentPosition */
function inferType(){
   var query = Qpresent(QtypeLF(Qsubobject(Qcomponent(Qindividual(currentElement), currentComponent), currentPosition)));
   execQuery(query,
     function(result){setLatinDialog(result.firstChild.firstChild.firstChild, 'type');}
   );
}
/** shows a component of the current MMT URI in a dialog */
function showComp(comp){
   var query = Qpresent(Qcomponent(Qindividual(currentURI), comp));
   execQuery(query,
     function(result){setLatinDialog(result.firstChild.firstChild.firstChild, comp);}
   );
}

function execQuery(q, cont) {
  $.ajax({
       url:'/:query', 
       type:'POST',
       data:q,
       processData:false,
       contentType:'text/xml',
       success:cont,
   });
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
