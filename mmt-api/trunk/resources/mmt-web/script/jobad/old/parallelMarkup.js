/* 
     $Id: parallelMarkup.js 1686 2010-02-07 13:30:12Z cdavid $
     $HeadURL: https://svn.omdoc.org/repos/jomdoc/src/prj/jobad/trunk/core/script/modules/utility/parallelMarkup.js $

     Copyright (c) KWARC, Jacobs University Bremen, 2009
     http://kwarc.info

     This library is free software; you can redistribute it and/or
     modify it under the terms of the GNU Lesser General Public
     License as published by the Free Software Foundation; either
     version 3 of the license, or (at your option) any later version.

     For other licensing options, please contact Michael Kohlhase
     <m.kohlhase@jacobs-university.de>.

     This library is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
     Lesser General Public License for more details.

     You should have received a copy of the GNU Lesser General Public
     License along with this library; if not, write to the Free Software
     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

/**
* 
* JOBAD - parallelMarkup.js module
*
* @author: Jana Giceva
* @created: 4 April 2009
* @description: parallel markup support
*
*/

//var Parallel = new actionElement('parallel', 'Enable Parallel', 'Enable/Disable Parallel Info', 'toggleParallel()');
//var parallel = new actionArrayElement(Parallel, "parallelMarkup", parallelMarkupServiceSupportCheck);
//actionsArray.addActionElement(parallel);

/**
 * parallelMarkupServiceSupportCheck - a callback function that checks if the parallel markup service is applicable 
 * to the given object.
 *
 * @param object : reference to a mathematical object to be investigated
 * @returns returnValue : a boolean set to true if the parallel markup service is applicable to the object or set to
 * false otherwise 
 */
var parallelMarkupServiceSupportCheck = function(object)
{
    var returnValue = true; //default value
    var result = getContentElement(object);
    if(!result) //fix by Cata: == "ND"
    {
        returnValue = false;
    }
    return returnValue;
}

/**
 * getContentElement - function that returns the content element of a certain given element
 *
 * @param element : math element, whose content element should be returned
 * @returns returnValue : the content element corresponding to the given mathml element
 * @throws undefined if the content element is undefined for the given element
 */
function getContentElement(element)
{
    var symbolRef = $(element).attr("xref");
    var returnValue;//fix by Cata: = "ND";
    if(symbolRef != null)
    {
        symbolRef = symbolRef.substring(1);
        /*
        * document.getElementByID() function seems not to be working
        * with elements from namespaces other than xhtml, thus is commented
        * and replaced with an XPath expression with the same purpose.
        */
        //var returnElement = document.getElementById(symbolRef);
        var xpath = "//*[@id='" + symbolRef + "']";
        var returnElement = document.evaluate(xpath, document, nsResolver, XPathResult.FIRST_ORDERED_NODE_TYPE, null);
        returnValue = returnElement.singleNodeValue;
    } else if (getTagName(element) == "mo") {
	// FIXME dirty workaround for when we are on an <mo> that does not have @xref set -- https://jomdoc.omdoc.org/ticket/72  -- Christoph, 2009-08-08
	var parentContent = getContentElement(element.parentNode);
	if (parentContent !== undefined) {
	    returnValue = parentContent.firstElementChild; // = the OMS in the head
	}
    }
    return returnValue;
}

/**
 * getCommonAncestor - funcation that returns the common ancestor of given two elements
 *
 * @param elem1 : first of the given mathml elements
 * @param elem2 : second of the given mathml elements
 * @returns rangeAncestor : container element that encapsulates the region between and including
 * the two given mathml elements
 */
function getCommonAncestor(elem1, elem2)
{
  var range = document.createRange();
  range.setStart(elem1, 0);
  range.setEnd(elem2, 0);
  var rangeAncestor = range.commonAncestorContainer;
  return rangeAncestor;
}

/**
 * getContentRange_old - function that returns the common ancestor of the corresponding content elements
 * of given certain mathml elements
 *
 * @param elem1 : first of the given mathml elements
 * @param elem2 : second of the given mathml elements
 * @return contentResult : reference to the element encapsulating both of the content elements
 * corresponding to the two given elements
 * @throws undefined if the content element for one of the given mathml element does not exist
 */
function getContentRange_old(elem1, elem2)
{
    var el1 = getContentElement(elem1);
    var el2 = getContentElement(elem2);
    if(!el1 || !el2) //fix by Cata: ((el1 === "ND") || (el2 == "ND"))
    {
        serviceNotAvailable();
    }
    else
    {
        var contentResult = getCommonAncestor(el1, el2);
    }
    return contentResult;
}

/**
 * getContentRange - function that returns the content element of the common ancestor of 
 * given two mathml elements
 *
 * @param elem1 : first of the given mathml elements
 * @param elem2 : second of the given mathml elements
 * @returns contentAncestor : reference to the content element of the common ancestor of elem1, and elem2
 */
function getContentRange(elem1,elem2)
{
    var commonAncestor = getCommonAncestor(elem1, elem2);
    var contentAncestor = getContentElement(commonAncestor);
    return contentAncestor;
}

/**
 * getParallelName - function that returns the value of the name attribute of the content element
 * corresponding to the given element
 *
 * @param element : given mathml element, whose content name is to be determined
 * @returns symbolName : a string value of the name attribute of the content element
 * @throws undefined : if the content element of the given element does not exist
 */
function getParallelName(element)
{
    var contentElement = getContentElement(element);
    var symbolName = "";
    if(contentElement !== undefined) //fix by Cata: != "ND"
    {
        symbolName = $(contentElement).attr("name");
    }
    return symbolName;
}
