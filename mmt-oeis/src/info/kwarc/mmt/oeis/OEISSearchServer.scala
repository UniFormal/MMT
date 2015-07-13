package info.kwarc.mmt.oeis

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.web._
import info.kwarc.mmt.planetary.{InformalMathMLPresenter}
import parser.{QueryParser}

class OEISSearchServer extends TEMASearchServer("oeis") {
  lazy val presenter = {
    controller.extman.get(classOf[presentation.Presenter]).find(_.key == "planetary").map(_.objectLevel) match {
      case Some(p : presentation.MathMLPresenter) => p
      case _ =>  throw new Exception("Presenter not loaded")
    }
  }
  
  val qp = new QueryParser
  lazy val imp = controller.extman.get(classOf[OEISImporter]) match {
    case hd :: Nil => hd
    case l => throw new Exception("OEISImporter not found: " + l)
  }
  
  def getSettings(path : List[String], query : String, body : Body) = Nil.toMap
  def process(query : String, settings : Map[String, String]) : objects.Term = {
    qp.parse(query) flatMap {exp => 
      println("got here")
      val elem = exp.toNode("A00") //TODO theory name shouldn't matter
      println(elem)
      val narrObj = <div><CMP><OMOBJ>{elem}</OMOBJ></CMP> </div>
      val dpath = Path.parseD("http://mathhub.info/smglom/mv/defeq.omdoc", NamespaceMap.empty)
      val mpath = dpath ? "defeq"
      val errorCont = new ErrorLogger(controller.report)
      imp.parseNarrativeObject(narrObj)(dpath, mpath, errorCont)
    } match {
      case Some(tm) => println(tm.toNode); tm
      case None => throw new Exception("Parsing Failed")
    }
  }
}
