package info.kwarc.mmt.api.flexiformal

import info.kwarc.mmt.api._
import web._
import ontology._
import tiscaf._
import scala.concurrent._
import scala.util.parsing.json._

class FlexiformalServerPlugin extends ServerExtension("immt") {
  def apply(uriComps: List[String], query: String, body: Body): HLet = uriComps match {
    case "query" :: _ => 
      getQueryResponse
    case _ => 
      println(uriComps)
      Server.errorResponse("Invalid request: " + uriComps.mkString("/"))
  }
  /*
   * subject
   * relation
   * return-type
   */
  private def getQueryResponse : HLet = new HLet {
     def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = try {
       val reqBody = new Body(tk)
       val reqS = reqBody.asString
       val params = JSON.parseRaw(reqS) match {
         case Some(j : JSONObject) => j.obj
         case _ => throw ServerError("Invalid JSON " + reqS)
       }
       log("Received immt query request : " + params.toString)
       val subjectS = params.get("subject").getOrElse(throw ServerError("No subject found")).toString
       val relationS = params.get("relation").getOrElse(throw ServerError("No relation found")).toString
       val returnS = params.get("return").getOrElse(throw ServerError("No return type found")).toString
       
       val subject = Path.parse(subjectS)
       val relation = Binary.parse(relationS)
       val resultSet = controller.depstore.getObjects(subject, relation)
       val pres = controller.extman.getPresenter("ihtml").getOrElse(throw ServerError("No presenter found"))
       val rb = new presentation.StringBuilder
             
       val resultNodes = resultSet foreach {p =>
         controller.get(p) match {
           case s : StructuralElement =>
             pres(s)(rb)
           case _ => 
         }
//         <result path = {p.path.toPath} position={p.fragment.toString} />
       }
       val response = "<div>" + rb.get + "</div>"
       log("Sending Response: " + response)
       Server.XmlResponse(response).aact(tk)
     }
   }
}
