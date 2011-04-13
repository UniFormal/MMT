/* 
     $Id: other.js 1686 2010-02-07 13:30:12Z cdavid $
     $HeadURL: https://svn.omdoc.org/repos/jomdoc/src/prj/jobad/trunk/core/script/modules/utility/other.js $

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
* JOBAD - other.js module
*
* @author: Jana Giceva
* @created: 21 Jan 2009
* @description: helping functions
*
*/

/**
 * underConstruction - function that alerts and informs the user that the feature is under construction
 */
function underConstruction()
{
    alert("Under Construction. Sorry for the inconvenience!");
}

/** 
 * serviceNotAvailable - function that alerts and informs the user that the service requested is 
 * currently not available.
 */
function serviceNotAvailable()
{
    alert("The selected action is not available. Sorry for the inconvenience!");
}

/**
 * shortcutUnavailable - function that alerts and informs the user that the shortcut pressed is unavailable
 * and giving short hints of what may be the cause, or how to fix it.
 */
function shortcutUnavailable()
{
    alert("Dear user, in order to use the shortcut, you would first have to right click on a mathematical object and then please use the corresponding key combination to activate the desired action");
}

/** 
 * toggleParallel - function that changes the value of the global field paralle from true to false and vise-versa
 */
function toggleParallel()
{
    if(parallel) {parallel = false;} else {parallel = true;}
}

/**
 * getAllAttributeLessSematicsElements - function that returns a list of all the semantics elements that have
 * no attributes.
 *
 * @returns array : list of all semantics elements that have no attributes
 */
function getAllAttributeLessSemanticsElements()
{
    var i = 0;
    var el =  null;
    var array = new Array();
    var all = document.getElementsByTagName("semantics");
    while (i < all.length)
    {
        el = all[i];
        if(!el.hasAttributes())
        {
            array.push(el);
        }
        i++;
    }
    return array;
}
