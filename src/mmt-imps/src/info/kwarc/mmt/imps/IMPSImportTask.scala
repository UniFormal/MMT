package info.kwarc.mmt.imps

import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.{LocalName, _}
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


  def doType(d : IMPSMathExp) : Term =
  {
    val ret : Term = d match
    {
      case IMPSSortRef(srt) =>
        IMPSTheory.Sort(OMS(IMPSTheory.thpath ? LocalName(srt)))
    }
    ret
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

  /* Opting for a List because one def-form can generate multiple terms */
  def doDefForm(d : LispExp) : List[Term] =
  {
    val ret : List[Term] = d match
    {
      case AtomicSort(id,srt,argthy,usgs,wtns,src)                  => Nil // TODO
      case Constant(id,mth,thy,srt,usgs,src)                        => Nil // TODO
      case Theorem(id,frml,lm,rev,thy,usgs,trans,mac,hmthy,prf,src) => Nil // TODO
      case ImportedRewriteRules(thynm, srcthy, srcths, src)         => Nil // TODO
      case SchematicMacete(id,frml,thy,nlp,transpp,src)             => Nil // TODO
      case CartesianProduct(id,srts,thy,const,accs,src)             => Nil // TODO
      case QuasiConstructor(id,lmbd,lng,fxt,src)                    => Nil // TODO
      // etc...
      case _                                                        => Nil // this stays
    }
    ret
  }

  /* Translate IMPS Math Expressions to Terms */
  def doMathExp(d : IMPSMathExp) : Term =
  {
    val ret : Term = d match
    {
      case IMPSSymbolRef(gn)    => OMS(gn)
      case IMPSTruth()          => OMS(IMPSTheory.thpath ? "thetrue")
      case IMPSFalsehood()      => OMS(IMPSTheory.thpath ? "thefalse")
      case IMPSNegation(p)      => IMPSTheory.Negation(doMathExp(p))
      case IMPSIf(p,t1,t2)      => IMPSTheory.If(doMathExp(p), doMathExp(t1), doMathExp(t2))
      case IMPSIff(p, q)        => IMPSTheory.Iff(doMathExp(p), doMathExp(q))
      case IMPSIfForm(p,q,r)    => IMPSTheory.If_Form(doMathExp(p), doMathExp(q), doMathExp(r))
      case IMPSEquals(a,b)      => IMPSTheory.Equals(doMathExp(a),doMathExp(b))
      case IMPSDisjunction(ls)  => IMPSTheory.Or(ls map doMathExp)
      case IMPSConjunction(ls)  => IMPSTheory.And(ls map doMathExp)
      case IMPSLambda(vs,t)     => IMPSTheory Lambda(vs map (p => (LocalName(p._1.v), p._2 map doType)), doMathExp(t))
      case IMPSForAll(vs,t)     => IMPSTheory.Forall(vs map (p => (LocalName(p._1.v), p._2 map doType)), doMathExp(t))
      case IMPSForSome(vs, t)   => IMPSTheory.Forsome(vs map (p => (LocalName(p._1.v), p._2 map doType)), doMathExp(t))
      case IMPSImplication(p,q) => IMPSTheory.Equals(doMathExp(p), doMathExp(q))
      case IMPSApply(f,ts)      => IMPSTheory.IMPSApply(doMathExp(f), ts map doMathExp)
      case IMPSIota(v1,s1,p)    => IMPSTheory.Iota(LocalName(v1.v), doType(s1), doMathExp(p))
      case IMPSIotaP(v1,s1,p)   => IMPSTheory.IotaP(LocalName(v1.v), doType(s1), doMathExp(p))
      case IMPSIsDefined(t)     => IMPSTheory.IsDefined(doMathExp(t))
      case IMPSIsDefinedIn(t,s) => IMPSTheory.IsDefinedIn(doMathExp(t), doType(s))
      case IMPSUndefined(s)     => IMPSTheory.Undefined(doMathExp(s))
    }
    ret
  }

  def doSourceRef(t : Term, s : SourceRef) = ???
  def doSourceRef(d : Declaration, s : SourceRef) = ???

  def doUnknown(d : LispExp) : Term = {???}

  def doName(s:String) : LocalName = LocalName(s)
}
