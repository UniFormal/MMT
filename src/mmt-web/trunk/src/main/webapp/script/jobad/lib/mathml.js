/*
     $Id: mathml.js 1686 2010-02-07 13:30:12Z cdavid $
     $HeadURL: https://svn.omdoc.org/repos/jomdoc/src/prj/jobad/trunk/core/script/modules/utility/mathml.js $

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
* JOBAD - mathml.js module
*
* @author: Jana Giceva
* @created: 21 Jan 2009
* @description: utility functions for MathML elements
*
*/

/** @constant-field : string denoting the address of the mathml namespace */
var NS_MATHML = "http://www.w3.org/1998/Math/MathML";
/** @constant-field: list of valid names of MathML grouping elements */
var validMathMLTagNameList = ['mrow','msqrt','maction','mfrac','mfenced','mroot','math'];


/**
 * wrapMathMLElement - function that wraps a certain mathml object into another mathml element
 *
 * @param wrapperTagName : string name of the element to become the container
 * @param focusObject : reference to the object that needs to be encapsulated
 * @returns returnMaction : reference to the wrapper element
 */
function wrapMathMLElement(wrapperTagName, focusObject)
{
  var object = focusObject;

  // wrap the given object
  var content = object.cloneNode(true);
  var wrapper = document.createElementNS( NS_MATHML, wrapperTagName);
  wrapper.appendChild(content);
  
  // replace the object by the wrapped object
  var parent = object.parentNode;
  parent.insertBefore(/* insert */ wrapper, /* before */ object);
  parent.removeChild(object);

  // reset the focus to the wrapped element
  focus = content;
  return wrapper;
}

/**
 * appendMathMLChild - function that appends a mathml child element to a given mathml element
 *
 * @param childTagName : string name of the tag of the child element to be created
 * @param childTagContent : string containing the content of the child element to be created
 * @param object : reference of the mathml element to which the child is to be appended
 */
function appendMathMLChild( childTagName, childTagContent, object )
{
  var newChild = document.createElementNS( NS_MATHML, childTagName);
  var content = document.createTextNode( childTagContent );
  newChild.appendChild(content);
  object.appendChild(newChild);
}

/**
 * getFirstMrowOrEquivalent - function that returns the first mrow (or equivalent wrt to the valid tagname list) ancestor element
 *
 * @param object - reference of the mathml element whose ancestor is to be selected
 * @returns object - reference of the mathml element with a valid tag name that encapsulates the given element
 */
function getFirstMrowOrEquivalent(object)
{
  var tagName =  getTagName(object);
  while (!validMathMLTagNameCheck(tagName) && (tagName != null))
  {
    object = object.parentNode;
    tagName = getTagName(object);
  }
  return object;
}

/** 
 * createMactionElement - function that dynamically creates an maction mathml element
 *
 * @param content : string denoting the content of the maction element to be created
 * @param actionType : string denoting the actionType to be used for the value of the maction 
 * element actiontype attribute
 * @param object : reference to the element that will be encapsulated by an maction element
 * @returns object : reference to the newly created maction element. 
 */

function createMactionElement(content, actionType, object)
{
  if(mrowOrEquivalentCheck(object))
  {
    object = getFirstMrowOrEquivalent(object);
    object = wrapMathMLElement("maction", object);
    object.setAttribute("actiontype",actionType);
    //@fixme see ticket 489!
    if (actionType == "folding")
    {
        object.setAttribute("selection","1");
        var child = document.createElementNS( NS_MATHML, "mrow");
        object.appendChild(child);
        object = object.lastChild;
        appendMathMLChild("mi",content, object);
    }
    else if(actionType == "unitConversion")
    {
        object.setAttribute("selection", "2");
        object.appendChild(content);
    }
	else{
		object.setAttribute('selection', '1');
		if(content != null){
			object.appendChild(content);
		}
	}
  }
  return object;
}


/**
 * mrowOrEquivalentCheck - function that checks if the tagname of the selected object is a 
 * valid mathml tag name or not
 *
 * @returns returnValue : a boolean that is set to true if the tagname is a valid mathml
 * tagname and to false otherwise
 */
function mrowOrEquivalentCheck(object)
{
    //var object = focus;
    var returnValue = false;
    for(i in validMathMLTagNameList)
    {
		returnValue = returnValue || checkMathMLEncapsulation(validMathMLTagNameList[i], object);
    }
    return returnValue;
}

/**
 * validMathMLTagNameCheck - function that checks if the given tagName is a valid mathml tag name
 *
 * @param messageName : a string value of the tagname that needs to be checked
 * @returns result : a boolean set to true if the tagname is a valid mathml tag name and false
 * otherwise.
 */
function validMathMLTagNameCheck(messageName)
{
  result = false;
  for (i in validMathMLTagNameList)
  {
    if(validMathMLTagNameList[i] == messageName)
    {
      result = true;
      break;
    }
  }
  return result;
}

/**
 * checkMathMLEncapsulation - function that checks if a given mathml element is a descendant of an
 * element with a specific tag name.
 *
 * @param ancestorTagName : string denoting the tag name that should be matched with the ancestors
 * of the given object
 * @param object : a reference to the element that needs to be checked if it has an ancestor with
 * a tag name that matches the given tagName
 * @returns result : a boolean whose value is set to true if the object is encapsulated by an element
 * with that specific tagname and to false otherwise.
 */
function checkMathMLEncapsulation( ancestorTagName, object)
{
  var result = false;
  var tagName = getTagName(object);
  while(tagName != null)
  {
    if(tagName == ancestorTagName)
    {
      result = true;
      break;
    }
    else
    {
      object = object.parentNode;
      tagName = getTagName(object);
    }
  }
  return result;
}

/**
 * getTagPrefix - function that returns the tag prefix of a given element
 *
 * @param object : reference to the element whose tag prefix should be determined
 * @returns returnPrefix : a string value denoting the tag prefix of the given element
 */
function getTagPrefix(object)
{
    var returnPrefix = ""; //default prefix value
    var tagName = object.tagName;
    var regExpPrefix = /\w*:/;
    returnPrefix = tagName.match(regExpPrefix);
    return returnPrefix;
}

/**
 * getTagName - function that returns the tag name of a given element
 *
 * @param object : reference to the element whose tag name should be determined
 * @returns returnTagName : a string value denoting the tag name of the given element
 */
function getTagName(object)
{
    var returnTagName = ""; //default return value
    if (object == null || object.tagName === undefined) {
        //console.log("Warning: reached document level in function getTagName");
		return null;
    }
    var tagNameOriginal = object.tagName;
    var index = tagNameOriginal.indexOf(":", 0);
    returnTagName = tagNameOriginal.substring(index+1);
    return returnTagName;
}

