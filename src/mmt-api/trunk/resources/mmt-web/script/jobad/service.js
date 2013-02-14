/**
 * The JOBAD generic service class
 * @author Florian Rabe, based on previous code by Catalin David
 */


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

//creating the context menu
$(document).ready(function(){
	$('body').append('<ul id="JOBADcontextMenu" class="jqcontextmenu"></ul>');
   $(document).addcontextmenu('JOBADcontextMenu');
   cmClear();
});


//bind all document events to the respective functions
function keyPress(key){
    for (x in loadedModules) {
        loadedModules[x][2].keyPressed(key);
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
