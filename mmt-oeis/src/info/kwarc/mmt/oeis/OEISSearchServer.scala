package info.kwarc.mmt.oeis

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.web._
import parser.{QueryParser}

class OEISSearchServer extends TEMASearchServer("oeis") {
  val qp = new QueryParser
  lazy val imp = controller.extman.get(classOf[OEISImporter]) match {
    case hd :: Nil => hd
    case l => throw new Exception("OEISImporter not found: " + l)
  }
  
  def getSettings(path : List[String], query : String, body : Body) = Nil.toMap
  def process(query : String, settings : Map[String, String]) : objects.Term = {
    println("got here")
    qp.parse(query) flatMap {exp => 
      val elem = exp.toNode("A00") //TODO theory name shouldn't matter
      val dpath = Path.parseD("http://mathhub.info/smglom/mv/defeq.omdoc", NamespaceMap.empty)
      val mpath = dpath ? "defeq"
      val errorCont = new ErrorLogger(controller.report)
      imp.parseNarrativeObject(elem)(dpath, mpath, errorCont)
    } match {
      case _ => throw new Exception("Parsing Failed")
    }
  }
}
