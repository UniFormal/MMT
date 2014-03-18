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
       println(subjectS + " " +  relationS + " " +returnS)
       val resultSet = controller.depstore.getObjects(subject, relation)
       val pres = controller.extman.getPresenter("planetary").getOrElse(throw ServerError("No presenter found"))
       val rb = new presentation.StringBuilder
       val resultNodes = resultSet foreach {p =>
         controller.get(p) match {
           case s : StructuralElement =>
             pres(s)(rb)
           case _ => 
         }
//         <result path = {p.path.toPath} position={p.fragment.toString} />
       }
      
       val html = if (resultSet.isEmpty) {
         "<div class=\"error\"> No Results Found </div>" 
       } else {
         "<div>" + rb.get + "</div>"
       }
       val response_header = <div class="modal-header">
          <button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>
          <h4 class="modal-title" id="myModalLabel">{subject.last}</h4>
         </div>
       val response = response_header.toString + 
         "<div class=\"modal-body\" style=\"overflow:auto\"> " + html + " </div>"
       log("Sending Response: " + response)
       Server.XmlResponse(response).aact(tk)
     }
   }
}
