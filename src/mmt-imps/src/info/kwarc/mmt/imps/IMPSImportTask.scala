package info.kwarc.mmt.imps

import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.objects.Term

import utils._

class IMPSImportTask(val controller: Controller, bt: BuildTask, index: Document => Unit) extends Logger with MMTTask
{
	def logPrefix = "imps-omdoc"
	protected def report: Report = controller.report

	def doDocument(es : Exp, uri : URI) : BuildResult =
	{
    var successfulTransfers : Int = 0

    var th_name : String = ""
    for (e <- es.children)
    {
      e match {
        case Heralding(m,_) => th_name = m
      }
    }

    if (th_name != "")
    {
      val theory = new DeclaredTheory(DPath(uri), LocalName(th_name), None)

      controller add theory
      theory.getDeclarations foreach { controller.add(_) }
      successfulTransfers += 1
    }



    println("#### " + successfulTransfers + " successfully transferred to MMT")
		BuildResult.empty
	}

  def doDecl(d : LispExp) : Constant = {

  }

  def doType(d : IMPSMathExp) : Term = {
  ???
  }

  def doExp(d : IMPSMathExp) : Term = d match {
    case IMPSLambda(vs,t) => IMPSTheory.IMPSLambda(vs.map(???),doExp(t))
  }

  def doUnknown(d : LispExp) : Term = {}
}
