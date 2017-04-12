package info.kwarc.mmt.api.guidedTours

import info.kwarc.mmt.api._
import documents._
import ontology._
import frontend._
import web._
import tiscaf._
import scala.concurrent._

class GuidedToursPlugin extends ServerExtension("guidedTours") {
 
  def apply(request: ServerRequest) : HLet = {
    try {
      request.path match {
        case "getTour" :: _ => getTour
      }
    } catch {
      case e : Exception => Server.TextResponse("Error: " + e.getMessage + e.getStackTraceString)
    }
  }
  
  
  def getTour : HLet = new HLet {
    def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = {
      val reqBody = new Body(tk)
      val pathS = reqBody.asString
      val dpath = Path.parseD(pathS, NamespaceMap.empty)
      val doc = controller.getDocument(dpath)
      
      val myPath = dpath / "guidedTour"

      val myDoc = new Document(myPath)
      val d = new DRef(myPath, LocalName("Section 1"), dpath)
      myDoc.add(d)
      
      //val rh = new presentation.StringBuilder
      //val out = controller.presenter.apply(doc, false)(rh)
      //val resp = rh.get
      val resp = doc.toString
      Server.TextResponse(resp).aact(tk)
    }
  }
  
}