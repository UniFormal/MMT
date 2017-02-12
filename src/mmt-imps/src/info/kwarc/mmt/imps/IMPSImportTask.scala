package info.kwarc.mmt.imps

import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.modules.{DeclaredModule, DeclaredTheory}
import info.kwarc.mmt.api.objects.{OMMOD, OMS, Term}
import info.kwarc.mmt.api.parser.SourceRef
import info.kwarc.mmt.api.symbols.Declaration
import info.kwarc.mmt.imps.IMPSTheory.Or
import utils._

class IMPSImportTask(val controller: Controller, bt: BuildTask, index: Document => Unit) extends Logger with MMTTask
{
	def logPrefix = "imps-omdoc"
	protected def report: Report = controller.report

	def doDocument(es : Exp, uri : URI) : BuildResult =
	{
    var successfulTransfers : Int = 0
    println("#### " + successfulTransfers + " successfully transferred to MMT")
		BuildResult.empty
	}

  def doModule(es : Exp) : DeclaredModule =
  {
    //case _ => new DeclaredTheory(???,???,Some(IMPSTheory.thpath))
    ???
  }

  def doDecl(d : LispExp)(implicit parent: MPath) : Constant = {
    val (ret,ref) = d match {
      case Constant(name,df,th,sort,usages,srcref) =>
        (symbols.Constant(OMMOD(parent),LocalName(name),Nil,sort map doType,Some(doMathExp(df)),None),srcref)
    }
    doSourceRef(ret,ref)
  }


  def doType(d : IMPSMathExp) : Term = {
    ???
  }

  /*
  DPath = Namespace
  LocalName(s : String)
  Dpath ? LocalName = MPath (theory/view)
  MPath ? LocalName = GlobalName (declarations)

  namespace http://imps.blubb

  theory Booleans =
   constant true%val <- http://imps.blubb?Booleans?true%val
   */

  def doLispExp(d : LispExp) : Term =
  {
    val ret = d match {
      case AtomicSort(id,srt,argthy,usgs,wtns,src) => ()
    }
    ??? //ret
  }

  def doMathExp(d : IMPSMathExp) : Term =
  {
    val ret = d match
    {
      case IMPSTruth() => IMPSTheory.Truth
      case IMPSFalsehood() => IMPSTheory.Falsehood
      case IMPSSymbolRef(gn) => OMS(gn)
      case IMPSEquals(a,b) => (IMPSTheory.Equals(doMathExp(a),doMathExp(b)))
      case IMPSDisjunction(ls) => (IMPSTheory.Or(ls map doMathExp),None)
      case IMPSConjunction(ls) => (IMPSTheory.And(ls map doMathExp), None)
      case IMPSLambda(vs,t) => (IMPSTheory.IMPSLambda(vs.map(p => (LocalName(p._1.v),p._2 map doType)),doMathExp(t)),None)
    }
    //if (srcrefopt) doSourceRef(ret,srcrefopt.get)
    ??? //ret.term
  }

  def doSourceRef(t : Term, s : SourceRef) = ???
  def doSourceRef(d : Declaration, s : SourceRef) = ???

  def doUnknown(d : LispExp) : Term = {???}
}
