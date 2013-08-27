/*******************************************************************************
 * This file is part of tiscaf.
 * 
 * tiscaf is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Foobar is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with tiscaf.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/
package tiscaf

import scala.collection.mutable.ListBuffer

/** This HApp gives to the user some utility methods to easily write
 *  RESTful services.
 *
 *  @author Lucas Satabin
 *
 */
class HRestApp extends HApp {

  /** This method takes care of the dispatch based on the
   *  HTTP verb of the request.
   */
  final def resolve(req : HReqData) = {
    val handlers = req.method match {
      case HReqType.Get => getHandler
      case HReqType.PostData | HReqType.PostMulti | HReqType.PostOctets =>
        postHandler
      case HReqType.Delete => deleteHandler
      case _               => throw new RuntimeException("Unknown request type")
    }

    val splitted = splitPath(req.uriPath)

    // find the first handler for this request
    handlers.find(_.isDefinedAt(splitted, req)) match {
      case Some(handler) =>
        Some(handler(splitted, req))
      case _ => None
    }

  }

  private val postHandler =
    ListBuffer.empty[PartialFunction[(List[String], HReqData), HLet]]
  private val getHandler =
    ListBuffer.empty[PartialFunction[(List[String], HReqData), HLet]]
  private val deleteHandler =
    ListBuffer.empty[PartialFunction[(List[String], HReqData), HLet]]

  def post(handler : PartialFunction[(List[String], HReqData), HLet]) {
    postHandler += handler
  }

  def get(handler : PartialFunction[(List[String], HReqData), HLet]) {
    getHandler += handler
  }

  def delete(handler : PartialFunction[(List[String], HReqData), HLet]) {
    deleteHandler += handler
  }

  private def splitPath(path : String) =
    path.split("/").toList

}
