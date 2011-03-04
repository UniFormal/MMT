/* 
 $Id: html.js 1686 2010-02-07 13:30:12Z cdavid $
 $HeadURL: https://svn.omdoc.org/repos/jomdoc/src/prj/jobad/trunk/core/script/modules/utility/html.js $
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
 * JOBAD - html.js module
 *
 * @author: Jana Giceva
 * @created: 21 Jan 2009
 * @description: functions to be utilized to add/remove html elements.
 *
 */
/**
 * addElement - function that adds an html child to a specific element, specifying the element
 * type, id, title attribute and the inner html content.
 *
 * @param elementID : string value of the ID of the "parent" element
 * @param newElement : string value of the child element tag name
 * @param idAttrVal : string value for the id attribute of the child element
 * @param titelAttrVal : string value for the title attibute of the child element
 * @param onMouseHover : string value for the action that is to be performed when cursor is to hover
 * over the child element
 * @param htmlContent : sting value for the content of the child element to be created
 */
function addElement(elementID, newElement, idAttrVal, titleAttrVal, onMouseHover, htmlContent){
    var parentEl = document.getElementById(elementID);
    var newEl = document.createElement(newElement);
    newEl.setAttribute('id', idAttrVal);
    if (onMouseHover !== '') {
        newEl.setAttribute('onmouseover', onMouseHover);
    }
    else {
        newEl.setAttribute('title', titleAttrVal);
    }
    newEl.innerHTML = htmlContent;
    parentEl.appendChild(newEl);
}

/**
 * removeElement - function that removes an html element
 *
 * @param elementID : string value of the ID of the element to be removed
 */
function removeElement(elementID){
    var elementEl = document.getElementById(elementID);
	if(elementEl)
    	elementEl.parentNode.removeChild(elementEl);
}
/**
 * removejscssfile - function that removes a JS / CSS script dynamically
 * @param {Object} filename
 * @param {Object} filetype
 */
function removejscssfile(filename, filetype){
    var targetelement = (filetype == "js") ? "script" : (filetype == "css") ? "link" : "none"; //determine element type to create nodelist from
    var targetattr = (filetype == "js") ? "src" : (filetype == "css") ? "href" : "none"; //determine corresponding attribute to test for
    var allsuspects = document.getElementsByTagName(targetelement);
    for (var i = allsuspects.length; i >= 0; i--) { //search backwards within nodelist for matching elements to remove
        if (allsuspects[i] && allsuspects[i].getAttribute(targetattr) != null && allsuspects[i].getAttribute(targetattr).indexOf(filename) != -1) 
            allsuspects[i].parentNode.removeChild(allsuspects[i]) //remove element by calling parentNode.removeChild()
    }
}
