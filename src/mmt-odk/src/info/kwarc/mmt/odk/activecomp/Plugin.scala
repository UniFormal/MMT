package info.kwarc.mmt.odk.activecomp

import info.kwarc.mmt.api.utils.{JSONObject, JSONString}
import info.kwarc.mmt.api.web._
import tiscaf.{HLet}

import scala.collection.immutable.List

class Plugin extends ServerExtension("activecomp") {
  /**
    * handles the HTTP request
    *
    * @param path  the PATH from above (excluding CONTEXT)
    * @param query the QUERY from above
    * @param body  the body of the request
    * @return the HTTP response
    *
    *         Implementing classes can and should use Server.XmlResponse etc to construct responses conveniently.
    *
    *         Errors thrown by this method are caught and sent back to the browser.
    */
  def apply(request: ServerRequest): HLet = {
    request.path match {
      case List("actions") => handleActions(request.body)
      case Nil => handleComputation(request.body)
    }
  }

  /* handles listing all active computations */
  def handleActions(body : Body): HLet = {
    Server.JsonResponse(JSONObject.fromList(activeComputation.all.map(ac => (JSONString(ac.key), JSONString(ac.desc)))))
  }

  /** handles an active computation */
  def handleComputation(body: Body): HLet = {
    // read the objective and the context
    val objective = body.asJSON.asInstanceOf[JSONObject]

    // get the math element we are talking about
    val context = objective("context").get.asInstanceOf[JSONObject]
    val math = scala.xml.XML.loadString(context("math").get.asInstanceOf[JSONString].value)

    // read the values for the computation into an Active Computation Context
    val values = ACContext.fromJSON(objective("values").get)

    // parse the action we want to make
    val action = activeComputation.parse(objective("action").get.asInstanceOf[JSONString].value).get

    // and compute
    val response = action(math, values)

    // and return some json
    Server.JsonResponse(response.toJSON)
  }
}