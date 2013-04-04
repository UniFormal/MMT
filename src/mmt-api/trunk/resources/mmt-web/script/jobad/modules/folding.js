var folding = {
	info: {
		'identifier' : 'kwarc.latin.editing',
		'title' : 'LATIN Editing Service',
		'author': 'Kwarc',
		'description' : 'The editing service for LATIN-MMT content',
		'version' : '1.0',
		'dependencies' : [],
		'hasCleanNamespace': false
	},
	
	contextMenuEntries : function(target, JOBADInstance) {	
		//is it in ML? or just document level?
		var me = this;
		if (checkMathMLEncapsulation('math', target)) {
			//code taken from foldingServiceSupportCheck and adapted
			var returnValue = false; //default value
			var obj = target; // replace with target OR current selection
			var isInMaction = checkMathMLEncapsulation("maction", obj);
			if (isInMaction == true) {
				if ($(obj).closest("maction").attr("actiontype") == "folding") { //already folded
					if ($(obj).closest("maction").attr('selection') == 1) { // unfolded version. return fold
						return {
							'Fold' : function() {me.foldIn(target)}
						};
					}
					else {//folded version -- return unfold
						var result = document.evaluate('//mathml:maction[@actiontype="folding"]', $(target).closest("maction")[0], nsResolver, XPathResult.ORDERED_NODE_SNAPSHOT_TYPE, null);//FIXME ask for maction only
						if (result.snapshotLength > 1) 
							return {
								'Unfold' : function() {me.foldOut(target);}, 
								'Unfold All' : function() {me.unfoldAll(target);}
							};
						return {
							'Unfold' : function() {me.foldOut(target);}
						};
					}
				}
				else {
					return {
						'Fold' : function() {me.foldIn(target);}
					};
				}
			}
			else {
				return {
					'Fold' :  function() {me.foldIn(target);}
				};
			}
			return true;
		}
		else {
			//In case we want folding of paragraphs or other non-math elements, here is the place to put it
			return true;
		}
	},


	/**
	 * foldIn - function that folds in the selected object
	 */
	foldIn : function (focus){
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
	},

	
	/**
	 * foldOut - function that folds out the selected object
	 */
	foldOut : function (focus){
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
	},
	
	
	unfoldAll : function (focus){
		var target = $(focus).closest("maction")[0];
		var result = document.evaluate('.//mathml:maction[@actiontype="folding"] | .', target, nsResolver, XPathResult.ORDERED_NODE_SNAPSHOT_TYPE, null);
		for (i = 0; i < result.snapshotLength; i++) {
			var res = result.snapshotItem(i);
			if ($(res).attr('selection') == "2") {
				$(res).attr('selection', "1");
			}
		}
	},

	
};

JOBAD.modules.register(folding);

