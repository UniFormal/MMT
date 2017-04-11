package info.kwarc.mmt.imps

import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.{LocalName, _}
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.modules.{DeclaredModule, DeclaredTheory}
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects.{OMMOD, OMS, Term}
import info.kwarc.mmt.api.parser.SourceRef
import info.kwarc.mmt.api.symbols.Declaration
import utils._

/* REMINDER:

DPath = Namespace
LocalName(s : String)
Dpath ? LocalName = MPath (theory/view)
MPath ? LocalName = GlobalName (declarations)

namespace http://imps.blubb

theory Booleans =
 constant true%val <- http://imps.blubb?Booleans?true%val
 */

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
  var theories  : List[DeclaredTheory] = Nil

	def doDocument(es : Exp, uri : URI) : BuildResult =
	{
    var transfers : Int = 0

    val doc = new Document(bt.narrationDPath, true)
    controller.add(doc)

    for (exp <- es.children)
    {
      exp match
      {
        /* Translating Theories to MMT */
        case Theory(id,lang,components,axioms,distinct,src) =>
        {
          val nu_theory = new DeclaredTheory(bt.narrationDPath, LocalName(id), Some(IMPSTheory.thpath))
          theories = theories ::: List(nu_theory)

          controller.add(nu_theory, None)
          controller.add(MRef(bt.narrationDPath,nu_theory.path))

          /* Translate language of the theory */

          /* Translate all axioms */
          if (axioms.isDefined)
          {
            var axcount : Int = -1
            for (ax <- axioms.get.axs)
            {
              ax match {
                case AxiomSpecification(formula, aid, usages, src) =>
                {
                  val mth : Term = IMPSTheory.thm(doMathExp(formula))
                  val name : String = if (aid.isDefined) { aid.get }
                                      else { axcount += 1 ; id + "_unnamed_axiom" + axcount.toString }

                  val assumption = symbols.Constant(nu_theory.toTerm,LocalName(name),Nil,Some(mth),None,Some("Assumption"))

                  controller.add(assumption)

                  //TODO: Handle usages

                  transfers += 1
                }
                case _ => ()
              }
            }
          }

          /* All constants here per distinction element are
             axiomatically distinct from each other */
          for (dist <- distinct)
          { /*TODO: implement*/ }

        }
        // Languages are processed in context of theories using them, not by themselves
        case Language(id,embedlang,embedlangs,bstps,extens,srts,cnstnts,src) => ()
        // If it's none of these, fall back to doDeclaration
        //case Constant(n,d,t,s,u,src) => doDecl(exp)
        case _ => ()
      }
    }

    index(doc)

    println("#### " + transfers + " successfully transferred to MMT")
		BuildSuccess(Nil,Nil)
	}

  def doDecl(d : LispExp) : symbols.Constant =
  {
    d match
    {
      case AtomicSort(name, defstring, theory, usages, witness, src) =>
      {
        // TODO: IMPLEMENT
        ???
      }
      case Constant(name, definition, theory, sort, usages, src) =>
      {
        val ln : LocalName = LocalName(theory.thy)
        assert(theories.exists(t => t.name == ln))
        val parent = theories(theories.indexWhere(dt => dt.name == ln))

        /* look for sort in given theory. */
        var srt : Option[Term] = None
        if (sort.isDefined)
        {
          val ln : LocalName = LocalName(sort.get.sort)

          assert(parent.getDeclarations.exists(b => b.name == ln))

          for (decl <- parent.getDeclarations) {
            if (decl.name == ln) { srt = Some(decl.toTerm) }
          }
        }
        else
        {
          // TODO: infer sort of definition, see IMPS manual pg. 168/169
          srt = Some(???)
        }

        // assert(srt.isDefined) <-- to be added again later

        val nu_constant = symbols.Constant(parent.toTerm, LocalName(name), Nil, srt, None, Some("Constant"))
        controller.add(nu_constant)
        nu_constant
      }
      case Theorem(name, formula, lemma, reverse, theory, usages, trans, macete, homeTheory, proof, src) =>
      {
        // TODO: IMPLEMENT
        ???
      }
      case _ => log("Error: Unknown LispExp encountered, not translated!"); ???
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

  /* Source References. Methods exceedingly small, but look nicer */
  def doSourceRef(t : Term, s : SourceRef) = {
    SourceRef.update(t, s)
  }

  def doSourceRef(d : Declaration, s : SourceRef) = {
    SourceRef.update(d, s)
  }

  /* Other stuff, stubs */
  def doUnknown(d : LispExp) : Term = {???}

  def doName(s:String) : LocalName = LocalName(s)
}
