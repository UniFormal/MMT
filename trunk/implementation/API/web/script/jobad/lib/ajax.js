/* 
     $Id: ajax.js 1686 2010-02-07 13:30:12Z cdavid $
     $HeadURL: https://svn.omdoc.org/repos/jomdoc/src/prj/jobad/trunk/core/script/modules/utility/ajax.js $

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
* JOBAD - ajax.js module
*
* @author: Jana Giceva
* @created: 15 April 2009
* @description: functions to be utilized to communicate with the server.
*
*/

/**
 * httpGetConnectionText - function that established a Get connection with the server expecting for a Text response
 *
 * @param connectionAddress : string value of the address to which the connection is to be established
 * @param parameters : list of (parameter, value) pairs to be sent to the server once the connection is established
 * @param responseHandlingFunction : a callback function that is responsible for handling the response received 
 * from the server
 * @param responseHandlingParameters : a list of parameters to be used by the responseHandlingFunction function
 */
function httpGetConnectionText(connectionAddress, parameters, responseHandlingFunction, responseHandlingParameters)
{
    var xmlHttp = new XMLHttpRequest();
    xmlHttp.onreadystatechange = function()
    {
        if(xmlHttp.readyState==4)
        {
            var response = xmlHttp.responseText;
            responseHandlingParameters["response"] = response;
            handleResponse(responseHandlingFunction,responseHandlingParameters);
        }
    }
    xmlHttp.open("GET",connectionAddress + "?" + parameters,true);
    xmlHttp.send(null);
}

/**
 * httpGetConnectionXml - function that established a Get connection with the server expecting for an Xml response
 *
 * @param connectionAddress : string value of the address to which the connection is to be established
 * @param headers : list of (header, value) pairs for the request
 * @param responseHandler : a callback function that is responsible for handling the response received 
 * from the server
 * @param responseHandleParams : a list of parameters to be used by the responseHandler function
 */
function httpGetConnectionXml(connectionAddress, headers, responseHandler, responseHandleParams)
{
    var xmlHttp = new XMLHttpRequest();
    xmlHttp.onreadystatechange = function()
    {
        if(xmlHttp.readyState==4)
        {
            responseHandleParams["response"] = xmlHttp.responseXML;
            handleResponse(responseHandler, responseHandleParams);
        }
    }
    xmlHttp.open("GET", connectionAddress, true);
    for (h in headers) {
       xmlHttp.setRequestHeader(headers[h][0], headers[h][1]);
    }
    xmlHttp.send(null);
}

/**
 * httpPostConnectionText - function that established a Post connection with the server expecting for a Text response
 *
 * @param connectionAddress : string value of the address to which the connection is to be established
 * @param parameters : list of (parameter, value) pairs to be sent to the server once the connection is established
 * @param responseHandlingFunction : a callback function that is responsible for handling the response received 
 * from the server
 * @param responseHandlingParameters : a list of parameters to be used by the responseHandlingFunction function
 */
function httpPostConnectionText(connectionAddress, parameters, responseHandlingFunction, responseHandlingParameters)
{
    var xmlHttp = new XMLHttpRequest();
    xmlHttp.overrideMimeType('text/xhtml+xml');
    xmlHttp.open("POST", connectionAddress, true);
    xmlHttp.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
    xmlHttp.setRequestHeader("Content-length", 2);
    xmlHttp.setRequestHeader("Connection", "close");

    xmlHttp.onreadystatechange = function()
    {
        if(xmlHttp.readyState == 4)
        {
            if(xmlHttp.status == 200)
            {
                responseHandlingParameters["response"] = xmlHttp.responseText;
                handleResponse(responseHandlingFunction, responseHandlingParameters);
            }
        }
    }
    xmlHttp.send(parameters);
}

/**
 * httpPostConnectionXml - function that established a Post connection with the server expecting for a Xml response
 *
 * @param connectionAddress : string value of the address to which the connection is to be established
 * @param parameters : list of (parameter, value) pairs to be sent to the server once the connection is established
 * @param responseHandlingFunction : a callback function that is responsible for handling the response received 
 * from the server
 * @param responseHandlingParameters : a list of parameters to be used by the responseHandlingFunction function
 */
function httpPostConnectionXml(connectionAddress, parameters, responseHandlingFunction, responseHandlingParameters)
{
    var xmlHttp = new XMLHttpRequest();
    xmlHttp.overrideMimeType('text/xhtml+xml');
    xmlHttp.open("POST", connectionAddress, true);
    xmlHttp.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
    xmlHttp.setRequestHeader("Content-length", 2);
    xmlHttp.setRequestHeader("Connection", "close");

    xmlHttp.onreadystatechange = function()
    {
        if(xmlHttp.readyState == 4)
        {
            if(xmlHttp.status == 200)
            {
                responseHandlingParameters["response"] = xmlHttp.responseXML;
                handleResponse(responseHandlingFunction, responseHandlingParameters);
            }
        }
    }
    xmlHttp.send(parameters);
}

/**
 * handleResponse - function that handles the response received from the server
 *
 * @param responseHandlingFunction : a callback function that is responsible for handling the response received 
 * from the server
 * @param responseHandlingParameters : a list of parameters to be used by the responseHandlingFunction function
 * @throws an alert in case the response obtained from the server indicates an error.
 */
function handleResponse(responseHandlingFunction, responseHandlingParameters)
{
    var response = responseHandlingParameters["response"];
    if(response == "error")
    {
        underConstruction();
    }
    else
    {
        responseHandlingFunction(responseHandlingParameters);
    }
}
