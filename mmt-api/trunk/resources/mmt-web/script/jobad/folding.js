/**
 * implementation of the JOBAD service class that realizes folding of math expressions
 * @author: Catalin David
 */
var folding = clone(Service);

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

folding.contextMenuEntries = function(target){

    //is it in ML? or just document level?
    if (checkMathMLEncapsulation('math', target)) {
        //code taken from foldingServiceSupportCheck and adapted
        var returnValue = false; //default value
        var obj = target; // replace with target OR current selection
        var isInMaction = checkMathMLEncapsulation("maction", obj);
        if (isInMaction == true) {
            if ($(obj).closest("maction").attr("actiontype") == "folding") { //already folded
                if ($(obj).closest("maction").attr('selection') == 1) { // unfolded version. return fold
                    return [['<u>F</u>old', 'foldIn()']];
                }
                else {//folded version -- return unfold
                    var result = document.evaluate('//mathml:maction[@actiontype="folding"]', $(target).closest("maction")[0], nsResolver, XPathResult.ORDERED_NODE_SNAPSHOT_TYPE, null);//FIXME ask for maction only
                    if (result.snapshotLength > 1) 
                        return [['Un<u>f</u>old', 'foldOut()'], ['Unfold <u>A</u>ll', 'unfoldAll()']];
                    return [['Un<u>f</u>old', 'foldOut()']];
                }
            }
            else {
                return [['<u>F</u>old', 'foldIn()']];
            }
        }
        else {
            return [['<u>F</u>old', 'foldIn()']];
        }
        return null;
    }
    else {
        //In case we want folding of paragraphs or other non-math elements, here is the place to put it
        return null;
    }
};

folding.keyPressed = function(key){
    //if context menu is open and the key pressed is f
    if (jquerycontextmenu.visible == true) {
        if (key.charCode == "70") {
            $.each($('.JOBADcmenu'), function(k, v){
                //if folding is available as an option fold/unfold and close menu
                if ($(v).html() == '<u xmlns="http://www.w3.org/1999/xhtml">F</u>old') {
                    foldIn();
                    jquerycontextmenu.hidebox($, $('.jqcontextmenu'));
                }
                if ($(v).html() == 'Un<u xmlns="http://www.w3.org/1999/xhtml">F</u>old') {
                    foldOut();
                    jquerycontextmenu.hidebox($, $('.jqcontextmenu'));
                }
            });
        }
        if (key.charCode == "65") {
            $.each($('.JOBADcmenu'), function(k, v){
                if ($(v).html() == 'Unfold <u xmlns="http://www.w3.org/1999/xhtml">A</u>ll') {
                    unfoldAll();
                    jquerycontextmenu.hidebox($, $('.jqcontextmenu'));
                }
            });
        }
    }
}

/**
 * foldIn - function that folds in the selected object
 */
function foldIn(){
    var obj = focus;
    var isInMaction = checkMathMLEncapsulation("maction", obj);
    
    if (isInMaction == true && $(obj).closest("maction").attr("actiontype") == "folding") {
        if ($(obj).closest("maction").attr('selection') == 1) { // unfolded version. fold
            $(obj).closest("maction").attr('selection', '2');
            return null;
        }
        else {//folded version -- unfold
            $(obj).closest("maction").attr('selection', '1');
            return null;
        }
    }
    //else -- it is not in an maction. Default behavior: get the closest mrow/mfrac/etc put it in maction, and select the folded version
    var container = getFirstMrowOrEquivalent(obj);
    if (getTagName(container) != 'math') { //so, we got an mrow/mfrac w/e, put the thing in an maction
        var ma = createMactionElement("…", "folding", container);
        ma.parentNode.setAttribute("selection", "2");
    }
    else {//the folded child is a direct child of math -- take everything in math, put it in an MROW, put the mrow in an MACTION, replace the content of MATH with MACTION 
        var obj = container.cloneNode(true);
        //put everything in an MROW		
        var mr = document.createElementNS(NS_MATHML, 'mrow');
        for (var i = 0; i < obj.childNodes.length; i++) {
            mr.appendChild(obj.childNodes[i].cloneNode(true));
        }
        //then put it in an MACTION type=folding
        var ma = document.createElementNS(NS_MATHML, 'maction');
        ma.setAttribute('actiontype', "folding");
        ma.setAttribute('selection', '2');
        ma.appendChild(mr);
        appendMathMLChild("mi", "...", ma);
        //delete all the existing nodes
        while (container.childNodes.length > 0) 
            container.removeChild(container.childNodes[0]);
        //append the maction to the MATH element
        container.appendChild(ma);
    }
}

/**
 * foldOut - function that folds out the selected object
 */
function foldOut(){
    var object = focus;
    var isInMaction = checkMathMLEncapsulation("maction", object);
    while ((getTagName(object) != "math") && (isInMaction)) {
        if ($(object).closest("maction").attr("actiontype") == "folding") {
            $(object).closest("maction").attr("selection", "1");
            return;
        }
        else {
            object = $(object).closest("maction").parent().get(0);
            isInMaction = checkMathMLEncapsulation("maction", object);
            // this should be sufficient, but it didn't work so I added the following code
            if (isInMaction == false) {
                return;
            }
        }
    }
}


function unfoldAll(){
    var target = $(focus).closest("maction")[0];
    var result = document.evaluate('.//mathml:maction[@actiontype="folding"] | .', target, nsResolver, XPathResult.ORDERED_NODE_SNAPSHOT_TYPE, null);
    for (i = 0; i < result.snapshotLength; i++) {
        var res = result.snapshotItem(i);
        if ($(res).attr('selection') == "2") {
            $(res).attr('selection', "1");
        }
    }
}

var foldingMod = ['folding', '/script/jobad/folding.js', folding, ""];
loadedModules.push(foldingMod);
