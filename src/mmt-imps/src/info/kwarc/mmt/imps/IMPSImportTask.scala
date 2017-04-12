package info.kwarc.mmt.imps

import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.{LocalName, _}
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.metadata.MetaDatum
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.parser.SourceRef
import info.kwarc.mmt.api.symbols.Declaration
import info.kwarc.mmt.imps.Usage.Usage
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

          /* Translate all axioms, if there are any */
          if (axioms.isDefined)
          {
            var axcount : Int = -1
            for (ax <- axioms.get.axs)
            {
              ax match {
                case AxiomSpecification(formula, aid, usages, src) =>
                {
                  val mth : Term = IMPSTheory.Thm(doMathExp(formula))
                  val name : String = if (aid.isDefined) { aid.get }
                                      else { axcount += 1 ; id + "_unnamed_axiom" + axcount.toString }

                  val assumption = symbols.Constant(nu_theory.toTerm,LocalName(name),Nil,Some(mth),None,Some("Assumption"))

                  doSourceRef(assumption,src)
                  controller.add(assumption)

                  //TODO: Handle usages
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
        case Constant(n,d,t,s,u,src)   => { controller.add(doDecl(exp)) }
        case AtomicSort(n,d,t,u,w,src) => { controller.add(doDecl(exp)) }
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
      case AtomicSort(name, defstring, theory, usages, _, src) =>
      {
        val ln : LocalName = LocalName(theory.thy)
        assert(theories.exists(t => t.name == ln)) // TODO: Translate to BuildFailure?
        val parent = theories(theories.indexWhere(dt => dt.name == ln))

        var definition : Term = IMPSTheory.Sort(doMathExp(defstring))

        /* TODO: We currently forget about witnesses. Should this change? */

        // instead of type None: Some(doUnknown(???))
        val nu_atomicSort = symbols.Constant(parent.toTerm, doName(name), Nil, None, Some(definition), None)
        if (usages.isDefined) { doUsages(nu_atomicSort, usages.get.usgs) }
        doSourceRef(nu_atomicSort,src)
        nu_atomicSort
      }
      case Constant(name, definition, theory, sort, usages, src) =>
      {
        val ln : LocalName = LocalName(theory.thy)
        assert(theories.exists(t => t.name == ln)) // TODO: Translate to BuildFailure?
        val parent = theories(theories.indexWhere(dt => dt.name == ln))

        /* look for sort in given theory. */
        var srt : Option[Term] = None
        if (sort.isDefined)
        {
          val ln_prime : LocalName = LocalName(sort.get.sort)

          println("~~~> " + ln.toString)
          for(d <- parent.getDeclarations)
            { println("~~~+ " + d.name.toString) }

          assert(parent.getDeclarations.exists(b => b.name == ln_prime))

          for (decl <- parent.getDeclarations) {
            if (decl.name == ln) { srt = Some(decl.toTerm) }
          }
        }
        else
        {
          // TODO: infer sort of definition, see IMPS manual pg. 168/169
          srt = None // Some(???)
        }

        // assert(srt.isDefined) <-- to be added again later

        val nu_constant = symbols.Constant(parent.toTerm, LocalName(name), Nil, srt, None, Some("Constant"))
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
      case IMPSVar(v)           => OMV(v)
      case IMPSSymbolRef(gn)    => OMS(gn)
      case IMPSMathSymbol(s)    => OMS(IMPSTheory.thpath ? s) // TODO: Is this enough or should the theory also be here?
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

  /* Source References. Methods exceedingly small, but look nicer than doing this everywhere directly */
  def doSourceRef(t : Term, s : SourceRef)        = { SourceRef.update(t, s) }
  def doSourceRef(d : Declaration, s : SourceRef) = { SourceRef.update(d, s) }

  /* Other stuff, stubs */

  def doUsages(d : Declaration, usages : List[Usage]) =
  {
    for (usage <- usages)
    {
      // using rootdpath and not IMPSTheory.rootdpath because this is IMPS, not LUTINS
      val metadata_verb   : GlobalName = rootdpath ? d.name ? LocalName("usage")
      val metadata_object : Obj        = OMS(rootdpath ? d.name ? usage.toString)
      d.metadata.add(new MetaDatum(metadata_verb, metadata_object))
    }
  }

  def doUnknown(d : LispExp) : Term = {???}
  def doName(s:String) : LocalName = LocalName(s)
}
