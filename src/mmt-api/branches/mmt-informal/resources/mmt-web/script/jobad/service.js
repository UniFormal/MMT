/**
 * The JOBAD generic service class
 * @author Florian Rabe, based on previous code by Catalin David
 */

/** @global-field focus: holds a reference to the object that was clicked by the user */
var focus = null;
/** @global-field focus: true if focus is within a math object */
var focusIsMath = false;

// if console is not defined, e.g., Firebug console is not enabled or Non-Firefox browser
if (typeof console == 'undefined') {
    var console = {};
    console.log = function(msg){
        return;
    };
    console.warn = function(msg){
        return;
    };
}

/** 
 * nsResolver - function that resolves which prefix corresponds to which namespace
 *
 * @param prefix : a string value of the prefix to be resolved
 * @returns the corresponding namespace or null if prefix undefined
 */
// used (only) by folding
function nsResolver(prefix){
    var ns = {
        'xhtml': 'http://www.w3.org/1999/xhtml',
        'mathml': 'http://www.w3.org/1998/Math/MathML',
        'm': 'http://www.w3.org/1998/Math/MathML',
        'openmath': 'http://www.openmath.org/OpenMath',
        'jobad': 'http://omdoc.org/presentation'
    };
    return ns[prefix] || null;
}

/**
 * Clones an object into another object (useful for prototypal inheritance)
 * @param {Object} object - the object to be cloned
 */
 // seems like a strange way of cloning, not sure if it works correctly
function clone(object){
    function F(){
    }
    F.prototype = object;
    return new F;
}

/**
 * Service - the main data type
 */
var Service = {
    init: function(){
        console.log("Service init");
        return null;
    },
    keyPressed: function(key){
        console.log("Service keyPressed");
        return null;
    },
    leftClick: function(target){
        console.log("Service leftClick");
        return null;
    },
    /* returns context menu entries as array [[entry_1, function_1], ..., [entry_n, function_1]], function_i is a string containing JavaScript code */
    contextMenuEntries: function(target){
        console.log("Service contextMenuEntries");
        return null;
    },
    hoverText: function(target){
        //console.log("Service hoverText"); // too annoying
        return null;
    }
};

/** the array of services; every services registers itself by adding itself to this array */ 
var loadedModules = new Array();

var cmenu;
//creating the context menu
$(document).ready(function(){
	$('body').append('<ul id="JOBADcontextMenu" class="jqcontextmenu"></ul>');
    cmenu = $(document).addcontextmenu('JOBADcontextMenu');
    cmClear();
    if (location.hash != "") {
        //handle highlighting
        var t = document.getElementById(location.hash.split('#')[1]);
        if (t) {
            handled_light(t);
        }
    }
});

function handled_light(t){
    try {
        $(t).effect('highlight', {}, 5000);
    } 
    catch (err) {
        highlight(t);
        setTimeout(function(){
            lowlight(t);
        }, 5000);
    }
}

// highlight by changing background color
function highlight(target){
    $(target).attr('mathbackground', 'silver'); // Firefox 2 and MathML spec
    $(target).attr('style', 'background:silver'); // Firefox 3
}

// undo highlight
function lowlight(target){
    $(target).removeAttr('mathbackground');
    $(target).removeAttr('style'); // assuming there are no other styles
}

/** helper function for the methods below: gets the classes of an element as an array */
function getClassArray(elem) {
   var classes = (elem.hasAttribute('class')) ? elem.getAttribute('class') : "";
   return classes.split(/\s+/);
}

// the following functions $.fn.f add functionality to jQuery and can be used as $(...).f
// contrary to the built-in jQuery analogues, these work for 'pref:name' attributes and math elements
/** add a class cl to all matched elements */
$.fn.addMClass = function(cl){
   this.each(function(){
      if (this.hasAttribute('class'))
         $(this).attr('class', $(this).attr('class') + ' ' + cl);   
      else
         this.setAttribute('class', cl);
   });
   return this;
}
/** remove a class cl from all matched elements */
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
/** toggle class cl in all matched elements */
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
/** keep elements that have class cl */
$.fn.filterMClass = function(cl){
   return this.filter(function(){
      var classes = getClassArray(this);
      return (classes.indexOf(cl) !== -1)
   });
}
/** keep elements that have attribute attr */
$.fn.hasMAttr = function(attr) {
    return this.filter(function() {
        return this.getAttribute(attr) !== undefined;
    });
};
/** keep elements that have attribute attr=value */
$.fn.filterMAttr = function(attr, value) {
    return this.filter(function() {
        return this.getAttribute(attr) == value;
    });
};

//bind all document events to the respective functions
function keyPress(key){
    for (x in loadedModules) {
	console.log(loadedModules[x])
        //loadedModules[x][2].keyPressed(key);
    }
}
//document.onkeypress = keyPress;

/**
 * cmClear() - clears the context Menu
 */
function cmClear(){
    var b = document.getElementById('JOBADcontextMenu');
    //console.log(b);
    while (b.hasChildNodes()) {
        b.removeChild(b.childNodes[0]);
    }
}

//a is an array with elements of form ['name','fun', [_subitems_like_'name','fun',[subitems]_]] 
function expandCMsubItems(a, subitem){
    var resin = [];
    
    for (var i = 0; i < a.length; i++) {
        var l2 = document.createElement('li');
        var url = document.createElement('a');
        url.setAttribute('class', 'JOBADcmenu');
        url.setAttribute('onclick', a[i][1]);
        url.setAttribute('title', a[i][1]);
        $(url).html(a[i][0]);
        l2.appendChild(url);
        
        //if it has subitems, then the result if definitely a ul
        if (a[i][2] && a[i][2].length > 0) {
            l2.appendChild(expandCMsubItems(a[i][2], true)[0]);
        }
        resin[i] = l2;
    }
    if (subitem) {
        var ul2 = document.createElement('ul');
        for (i = 0; i < resin.length; i++) 
            ul2.appendChild(resin[i]);
        return [ul2];
    }
    else {
        return resin;
    }
}

function click(e){
    if (e.which == 3) {//we have a right click
        focus = e.target;
        focusIsMath = ($(focus).closest('math').length !== 0);
        xpos = e.clientX;
        ypos = e.clientY;
        //clear the contextMenu
        cmClear();
        //retrieve new entries and add them to the context menu - add class='JOBADcmenu' to <a> elements + onclick=a[x][1]   
        for (x in loadedModules) {
            var a = loadedModules[x][2].contextMenuEntries(e.target);
            if (a !== null) {
                var cm = document.getElementById('JOBADcontextMenu');
                var res = expandCMsubItems(a, false);
                for (var y = 0; y < res.length; y++) 
                    cm.appendChild(res[y]);
            }
        }
    }
    else //FIXME - if it is a selection, don't call the leftClick event
         if (e.which == 1) {
            //left click
            //if it's outside of the contextMenu
            if ($(e.target).closest('#JOBADcontextMenu').length > 0) 
                return;
            if (window.getSelection().focusNode != null) { //if outside a click
                if (window.getSelection().getRangeAt(0).startOffset == window.getSelection().getRangeAt(0).endOffset || $(e.target).parents('svg').length > 0) //normal click, no selection involved or SVG
                    for (x in loadedModules) {
                        if (loadedModules[x][2]) {
                            var res = loadedModules[x][2].leftClick(e.target);
                            if (res != null) 
                                break;
                        }
                    }
            }
        }
}

document.onclick = click;

function hover(e){
    for (x in loadedModules) {
        if (loadedModules[x][2] != null) 
            //test null? do_nothing : e.title = val
            var res = loadedModules[x][2].hoverText(e.target);
        if (res != null) {
            $(e.target).attr('title', res);
            break;
        }
    }
}

document.onmouseover = hover;

// TODO: remove these functions
function proxyGet(URL, data, fun){
    if (typeof proxyURL == 'undefined') {
        //normal jQuery GET request
        $.get(URL, data, fun);
    }
    else {
        //send request to proxy
		$.get(proxyURL, {'url':URL, 'data':data, 'method':'get'}, fun);
    }
}

function proxyPost(URL, data, fun){
    if (typeof proxyURL == 'undefined') {
        //normal jQuery POST request
        $.post(URL, data, fun);
    }
    else {
        //send request to proxy
		$.get(proxyURL, {'url':URL, 'data':data, 'method':'post'}, fun);
    }
}

function proxyAjax(Atype, Aurl, Adata, Asuccess, Aasync, Aaccept){
    if (typeof proxyURL == 'undefined') {
        $.ajax({
            'type': Atype,
            'url': Aurl,
            'data': Adata,
            'dataType': 'xml',
            'success': Asuccess,
            'async': Aasync,
            'beforeSend' : function(xhr){
        		if(Aaccept)
        			xhr.setRequestHeader("Accept", Aaccept);
        	}
        });
    }
    else {
        //send request to proxy
		$.ajax({
			'type': 'POST',
			'url': proxyURL, 
			'data': {'url':Aurl, 'data':Adata, 'method':Atype, 'header':Aaccept}, 
			'success': Asuccess,
			'async': Aasync
		});
    }
}