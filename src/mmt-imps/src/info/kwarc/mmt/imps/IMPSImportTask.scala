package info.kwarc.mmt.imps

import info.kwarc.mmt.api
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

class TheoryState(val parent : DPath, val name : LocalName, val meta : MPath)
{
  val path : MPath = parent ? name
  protected var decls : List[Declaration] = Nil
  def add(d : Declaration) = decls ::= d
  def getDeclarations = decls.reverse
  def toTerm = OMMOD(path)
  def declares(n : LocalName) : Boolean = decls.exists(d => d.name == n)

  object includes {
    var stored : List[(MPath,Boolean)] = Nil
    def contains(p : MPath) = stored.exists(a => a._1 == p)
    def ::=(p : MPath, par : Boolean = false) = stored ::= (p,par)
    def find(cond : MPath => Boolean) : Option[MPath] = stored.map(_._1).find(cond)
    def inPars(p : MPath) : Option[Boolean] = stored.find(q => q._1 ==p).map(_._2)
  }
}

class IMPSImportTask(val controller: Controller, bt: BuildTask, index: Document => Unit) extends Logger with MMTTask
{
	def logPrefix = "imps-omdoc"
	protected def report: Report = controller.report

  val rootdpath : DPath = DPath(URI.http colon "imps.mcmaster.ca")

	def doDocument(es : Exp, uri : URI) : BuildResult =
	{
    var transfers : Int                  = 0
    var theories  : List[DeclaredTheory] = Nil

    for (exp <- es.children)
    {
      exp match
      {
        /* Translating Theories to MMT */
        case Theory(id,lang,components,axioms,distinct,src) =>
        {
          var nu_theory = new DeclaredTheory(rootdpath, LocalName(id), Some(IMPSTheory.thpath))
          theories = theories ::: List(nu_theory)

          controller.add(nu_theory, None)

          /* Translate all axioms */
          if (axioms.isDefined)
          {
            var axcount : Int = 0
            for (ax <- axioms.get.axs)
            {
              controller.add(doDecl(ax)(nu_theory))
            }
          }


          /* All constants here per distinction element are
             axiomatically distinct from each other */
          for (dist <- distinct)
          { /*TODO: implement*/ }


          transfers += 1
        }
        case _ => ()
      }
    }

    val doc = new Document(bt.narrationDPath, true)
    controller.add(doc)
    index(doc)

    println("#### " + transfers + " successfully transferred to MMT")
		BuildSuccess(Nil,Nil)
	}

  def doModule(es : Exp) : DeclaredModule =
  {
    ???
  }

  def doDecl(d : LispExp)(implicit parent: DeclaredTheory) : symbols.Constant = {
    //val (ret,ref) = d match {
      //case Constant(name,df,th,sort,usages,srcref) =>
      //  (symbols.Constant(OMMOD(parent),LocalName(name),Nil,sort. map doType,Some(doMathExp(df)),None),srcref)
    //}
    //doSourceRef(ret,ref)

    d match {
      case Constant(name, definition, theory, sort,usages,src) => {
        val srt : Option[Term] = ??? //if (sort.isDefined) { Some(doType(sort.get.sort)) } else { None }

        symbols.Constant(???,???,???, srt, ???,???,???)
      }
      case AxiomSpecification(formula,name,usages, source) => {

        val mth : Term = doMathExp(formula)
        println(mth.toString)
        symbols.Constant(parent.toTerm,LocalName(name.get),Nil,Some(mth),None,Some("Assumption"))
      }
    }
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
      case IMPSImplication(p,q) => IMPSTheory.Implies(doMathExp(p), doMathExp(q))
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
