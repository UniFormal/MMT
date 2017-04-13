package info.kwarc.mmt.imps

import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.checking.{Checker, CheckingEnvironment, MMTStructureChecker, RelationHandler}
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.{LocalName, _}
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.metadata.MetaDatum
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.opaque.{OpaqueText, StringFragment}
import info.kwarc.mmt.api.parser.SourceRef
import info.kwarc.mmt.api.symbols.Declaration
import info.kwarc.mmt.imps.Usage.Usage
import info.kwarc.mmt.lf.Typed
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

  val rootdpath : DPath                = DPath(URI.http colon "imps.mcmaster.ca")
  var trans     : TranslationState     = new TranslationState(bt)

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
        case t@(Theory(id,l,c,a,d,src))       => { doTheory(t) }
        // Languages are processed in context of theories using them, not by themselves
        case Language(id,embedlang,embedlangs,bstps,extens,srts,cnstnts,src) => ()
        // If it's none of these, fall back to doDeclaration
        case _ => { doDecl(exp) ; transfers += 1 }
      }
    }

    // Run Checker (to resolve unknowns, etc)
    // Set to true to run
    val typecheck : Boolean = false
    if (typecheck)
    {
      log("Checking:")
      logGroup
      {
        val checker = controller.extman.get(classOf[Checker], "mmt").getOrElse {
          throw GeneralError("no checker found")
        }.asInstanceOf[MMTStructureChecker]
        trans.theories foreach {p =>
          val ce = new CheckingEnvironment(new ErrorLogger(report), RelationHandler.ignore, this)
          (checker.apply(p)(ce))
        }
      }
    }

    index(doc)

    println("#### " + transfers + " successfully transferred to MMT")
		BuildSuccess(Nil,Nil)
	}

  def doTheory (t : Theory) : Unit =
  {
    val nu_theory = new DeclaredTheory(bt.narrationDPath, LocalName(t.name), Some(IMPSTheory.thpath))
    trans.theories = trans.theories ::: List(nu_theory)

    controller.add(nu_theory, None)
    controller.add(MRef(bt.narrationDPath,nu_theory.path))

    /* Translate language of the theory */
    //TODO: Implement

    /* Translate all axioms, if there are any */
    if (t.axioms.isDefined)
    {
      var axcount : Int = -1
      for (ax <- t.axioms.get.axs)
      {
        val mth : Term = IMPSTheory.Thm(doMathExp(ax.formula))

        val name : String = if (ax.name.isDefined) { ax.name.get }
        else { axcount += 1 ; t.name + "_unnamed_axiom" + axcount.toString }

        val assumption = symbols.Constant(nu_theory.toTerm,doName(name),Nil,Some(mth),None,Some("Assumption"))

        if (ax.usgs.isDefined) { doUsages(assumption,ax.usgs.get) }
        doSourceRef(assumption,ax.src)
        controller.add(assumption)
      }
    }

    /* All constants here per distinction element are
       axiomatically distinct from each other */
    for (dist <- t.dstnct)
    { /*TODO: implement*/ }

  }

  def doDecl(d : LispExp) : Unit =
  {
    // set this to true for helpful debug output
    val debug : Boolean = false
    if (debug)
    {
      println("\n>>>>> Call to doDecl for the following expression:\n")
      println(d.toString)
      for (thy <- trans.theories)
      {
        println("\n<<<<< Theory " + thy.name + " contains the following declarations:")
        for(d <- thy.getDeclarations)
        { println("~~~ " + d.name.toString) }
      }
    }

    d match
    {
      case AtomicSort(name, defstring, theory, usages, witness, src) =>
      {
        val ln : LocalName = LocalName(theory.thy)
        assert(trans.theories.exists(t => t.name == ln)) // TODO: Translate to BuildFailure?
        val parent = trans.theories(trans.theories.indexWhere(dt => dt.name == ln))

        val definition : Term = IMPSTheory.Sort(doMathExp(defstring))
        val sorttype   : Term = trans.doUnknown
        trans.bindUnknowns(sorttype)

        val nu_atomicSort = symbols.Constant(parent.toTerm, doName(name), Nil, Some(sorttype), Some(definition), None)

        /* Add available MetaData */
        if (witness.isDefined) { doMetaData(nu_atomicSort, "witness", witness.get.witness.toString) }
        if (usages.isDefined)  { doUsages(nu_atomicSort, usages.get.usgs) }
        doSourceRef(nu_atomicSort,src)

        controller add nu_atomicSort
      }
      case Constant(name, definition, theory, sort, usages, src) =>
      {
        val ln : LocalName = LocalName(theory.thy)
        assert(trans.theories.exists(t => t.name == ln)) // TODO: Translate to BuildFailure?
        val parent = trans.theories(trans.theories.indexWhere(dt => dt.name == ln))

        /* look for sort in given theory. */
        var srt : Option[Term] = None
        if (sort.isDefined)
        {
          val ln_prime : LocalName = LocalName(sort.get.sort)

          assert(parent.getDeclarations.exists(b => b.name == ln_prime))

          for (decl <- parent.getDeclarations) {
            if (decl.name == ln_prime) { srt = Some(decl.toTerm) }
          }
        }
        else {
          srt = Some(trans.doUnknown)
        }

        assert(srt.isDefined)
        if (srt.isDefined) { trans.bindUnknowns(srt.get) }

        /* Add available MetaData */
        val nu_constant = symbols.Constant(parent.toTerm, LocalName(name), Nil, srt, None, Some("Constant"))
        doSourceRef(nu_constant,src)
        if (usages.isDefined) { doUsages(nu_constant,usages.get.usgs) }

        controller add nu_constant
      }
      case Theorem(name, formula, lemma, reverse, theory, usages, transp, macete, homeTheory, proof, src) =>
      {
        val ln : LocalName = LocalName(theory.thy)
        assert(trans.theories.exists(t => t.name == ln)) // TODO: Translate to BuildFailure?
        val parent : DeclaredTheory = trans.theories(trans.theories.indexWhere(dt => dt.name == ln))

        // TODO: Currently still forgets about the proof

        val mth : Term = IMPSTheory.Thm(doMathExp(formula))
        val nu_theorem = symbols.Constant(parent.toTerm, doName(name), Nil, Some(mth), None, Some("Theorem"))
        //                                                                              ^-- proof goes here!

        /* Add available MetaData */
        if (usages.isDefined)     { doUsages(nu_theorem, usages.get.usgs) }
        if (transp.isDefined)     { doMetaData(nu_theorem, "translation", transp.get.trans) }
        if (macete.isDefined)     { doMetaData(nu_theorem, "macete", macete.get.macete) }
        if (homeTheory.isDefined) { doMetaData(nu_theorem, "homeTheory", homeTheory.get.hmthy) }

        if (lemma)   { doMetaData(nu_theorem,"lemma","present") }   else { doMetaData(nu_theorem,"lemma","absent") }
        if (reverse) { doMetaData(nu_theorem,"reverse","present") } else { doMetaData(nu_theorem,"reverse","absent") }

        doSourceRef(nu_theorem, src)

        controller add nu_theorem
      }
      case SchematicMacete(name, formula, thy, nullPresent,transportablePresent,src) =>
      {
        val ln : LocalName = LocalName(thy.thy)
        assert(trans.theories.exists(t => t.name == ln)) // TODO: Translate to BuildFailure?
        val parent : DeclaredTheory = trans.theories(trans.theories.indexWhere(dt => dt.name == ln))

        // Macetes are added as opaque (for now?)
        val opaque = new OpaqueText(parent.path.toDPath, List(StringFragment(d.toString)))

        /* Opaque Text doesn't have metadata, apparently, so we don't add the src */

        controller add opaque
      }
      case _ => log("Error: Unknown LispExp encountered, not translated!"); ???
    }
  }

  def doType(d : IMPSMathExp) : Term =
  {
    // TODO: Check if type is already in theory(?)
    // Reference if yes, introduce if no?

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
    d match
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
  }

  /* Source References. Methods exceedingly small, but look nicer than doing this everywhere directly */
  def doSourceRef(t : Term, s : SourceRef)        = { SourceRef.update(t, s) }
  def doSourceRef(d : Declaration, s : SourceRef) = { SourceRef.update(d, s) }

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

  /* Add more generic metadata, meant to be used for short strings, not proofs
   * Might be rewritten, when we have cleverer solutions for MetaData */
  def doMetaData(d : Declaration, metaVerb : String, metaObject : String) : Unit =
  {
    val mv : GlobalName = rootdpath ? d.name ? LocalName(metaVerb)
    val mo : Obj        = OMS(rootdpath ? d.name ? metaObject)

    d.metadata.add(new MetaDatum(mv,mo))
  }

  def doName(s : String) : LocalName = LocalName(s)
}

// See also: This exact thing, but in PVS
class TranslationState (bt : BuildTask)
{
  var vars     : Context              = Context.empty
  var theories : List[DeclaredTheory] = Nil
  protected var unknowns : Int                  = 0


  protected def doiName(i : Int, isType : Boolean) : LocalName = {
    LocalName("") / { if (isType) LocalName("I") else LocalName("i") } / i.toString
  }

  def doUnknown : Term = OMV(doiName({unknowns+=1;unknowns-1},false))

  def bindUnknowns(t : Term) : Term =
  {
    val symbs = t.freeVars.collect {
      case ln if ln.toString.startsWith("""/i/""") => ln
    }

    val cont = symbs.flatMap(n =>
    {
      val i = (0 until unknowns).find(j => n == doiName(j,false))
      if (i.isDefined) {
        val v1 = VarDecl(doiName(i.get,true), Some(OMS(Typed.ktype)), None, None)
        val v2 = VarDecl(n, Some(OMV(doiName(i.get,true))), None, None)
        List(v1,v2)
      }
      else throw GeneralError("No unknown " + n)
    })

    if (unknowns > 0 && cont.nonEmpty) {
      OMBIND(OMS(Path.parseS("http://cds.omdoc.org/mmt?mmt?unknown", NamespaceMap.empty)), cont, t)
    } else { t }
  }

  def reset = {
    unknowns = 0
    vars = Context.empty
  }
}
