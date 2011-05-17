/**
 * The JOBAD generic service class
 * @author Florian Rabe, based on previous code by Catalin David
 */
//not sure if we need all these variables. will keep track of them.

/** @constant-field xmlns.ML: declaring the shorthand notation to be used for the MathML namespace */
//$.xmlns.ML = "http://www.w3.org/1998/Math/MathML";

//TODO: use  window.getSelection().getRangeAt(0).startContainer.parentNode to get the current selected text's parent

/** @global-field focus: holds a reference to the object that was clicked by the user. Initialized to null */
var focus = null;

/** @global-field xpos: holds the value of the x coordinate of the registered event. Initialized to zero. */
var xpos = 0;
/** @global-field ypos: holds the value of the y coordinate of the registered event. Initialized to zero. */
var ypos = 0;

/** @global-field populated: holds a value if the context dialog is populated or not. Initialized to false. */
var populated = false;
/** @global-field gContext: holds a value to the general context of the last loaded service */
var gContext = null;
var gHost = null;

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

//Now bind all document events to the respective functions
function keyPress(key){
    for (x in loadedModules) {
        loadedModules[x][2].keyPressed(key);
    }
}

document.onkeypress = keyPress;

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

///**
// *  retrieved from http://stackoverflow.com/questions/1482832/javascript-how-to-get-all-elements-that-are-highlighted
// * @param {Object} range
// * @param {Object} node
// */
//function rangeIntersectsNode(range, node){
//    var nodeRange;
//    if (range.intersectsNode) {
//        return range.intersectsNode(node);
//    }
//    else {
//        nodeRange = node.ownerDocument.createRange();
//        try {
//            nodeRange.selectNode(node);
//        } 
//        catch (e) {
//            nodeRange.selectNodeContents(node);
//        }
//        
//        return range.compareBoundaryPoints(Range.END_TO_START, nodeRange) == -1 &&
//        range.compareBoundaryPoints(Range.START_TO_END, nodeRange) == 1;
//    }
//}
//
///**
// * retrieved from http://stackoverflow.com/questions/1482832/javascript-how-to-get-all-elements-that-are-highlighted
// * @param {Object} win - the scope of searching
// */
//function getSelectedElementTags(win){
//    var range, sel, elmlist, treeWalker, containerElement;
//    sel = win.getSelection();
//    if (sel.rangeCount > 0) {
//        range = sel.getRangeAt(0);
//    }
//    
//    if (range) {
//        containerElement = range.commonAncestorContainer;
//        if (containerElement.nodeType != 1) {
//            containerElement = containerElement.parentNode;
//        }
//        
//        treeWalker = win.document.createTreeWalker(containerElement, NodeFilter.SHOW_ELEMENT, function(node){
//            return rangeIntersectsNode(range, node) ? NodeFilter.FILTER_ACCEPT : NodeFilter.FILTER_REJECT;
//        }, false);
//        
//        elmlist = [treeWalker.currentNode];
//        while (treeWalker.nextNode()) {
//            elmlist.push(treeWalker.currentNode);
//        }
//        
//        console.log(elmlist);
//    }
//}

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
        //var selection = getSelectedElementTags(window);
        focus = e.target;
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