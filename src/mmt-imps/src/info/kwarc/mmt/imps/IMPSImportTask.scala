package info.kwarc.mmt.imps

import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.checking.{Checker, CheckingEnvironment, MMTStructureChecker, RelationHandler}
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.{LocalName, _}
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.metadata.MetaDatum
import info.kwarc.mmt.api.modules.{DeclaredTheory, DeclaredView}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.opaque.{OpaqueText, StringFragment}
import info.kwarc.mmt.api.parser.SourceRef
import info.kwarc.mmt.api.symbols.Declaration
import info.kwarc.mmt.api.symbols.PlainInclude
import info.kwarc.mmt.imps.Usage.Usage
import info.kwarc.mmt.lf.{Apply, ApplySpine}
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

class IMPSImportTask(val controller: Controller, bt: BuildTask, index: Document => Unit, tState : TranslationState)
  extends Logger with MMTTask
{
	          def logPrefix : String = "imps-omdoc"
	protected def report    : Report = controller.report

  val rootdpath : DPath                = DPath(URI.http colon "imps.mcmaster.ca") /* arbitrary, but seemed fitting */

  /* Source References. Methods exceedingly small, but look nicer than doing this everywhere directly */
  def doSourceRef(t : Term, s : SourceRef)        : Unit = { SourceRef.update(t, s) }
  def doSourceRef(d : Declaration, s : SourceRef) : Unit = { SourceRef.update(d, s) }

  /* Add metadata from usages element */
  def doUsages(d : Declaration, usages : List[Usage]) : Unit =
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
    val mv : GlobalName =     rootdpath ? d.name ? LocalName(metaVerb)
    val mo : Obj        = OMS(rootdpath ? d.name ? metaObject)

    d.metadata.add(new MetaDatum(mv,mo))
  }

  def doName(s : String) : LocalName = LocalName(s)

	def doDocument(es : Exp, uri : URI) : BuildResult =
	{
    val doc = new Document(bt.narrationDPath, true)
    controller.add(doc)

    var excps : List[Exception] = Nil

    for (exp <- es.children)
    {
      exp match
      {
        /* Translating Theories to MMT */
        case t@(Theory(_,_,_,_,_,_))       => try
        {
          if (!tState.theories_raw.contains(t)) { doTheory(t, bt.narrationDPath) }
        } catch {
          case e : IMPSDependencyException => { println(" > ... fail. Add to stack: " +  e.getMessage ) ; excps = excps.::(e) }
        }
        // Languages are processed in context of theories using them, not by themselves
        case l@(Language(_,_,_,_,_,_,_,_)) => {
          if (!tState.languages.contains(l)) { println(" > adding language " + l.name) ; tState.languages = tState.languages :+ l }
        }
        // If it's none of these, fall back to doDeclaration
        case _                             => doDeclaration(exp)
      }
    }

    if (!excps.isEmpty) { throw excps.head }

    // Run Checker (to resolve unknowns, etc)
    // Set to true to run
    val typecheck : Boolean = true

    if (typecheck)
    {
      log("Checking:")
      logGroup
      {
        val checker = controller.extman.get(classOf[Checker], "mmt").getOrElse {
          throw GeneralError("no checker found")
        }.asInstanceOf[MMTStructureChecker]
        tState.theories_decl foreach { p =>
          val ce = new CheckingEnvironment(new ErrorLogger(report), RelationHandler.ignore, this)
          checker.apply(p)(ce)
        }
      }
    }

    index(doc)
    BuildSuccess(Nil,Nil)
	}

  def doTheory (t : Theory, dPath: DPath) : Unit =
  {
    val nu_theory = new DeclaredTheory(dPath,
                                       LocalName(t.name),
                                       Some(IMPSTheory.lutinsPath),
                                       modules.Theory.noParams,
                                       modules.Theory.noBase)

    println(" > trying to add theory " + t.name)

    val mref : MRef = MRef(dPath,nu_theory.path)
    controller.add(nu_theory)
    controller.add(mref)

    def thy_reset() : Unit = {
      controller.delete(nu_theory.path)
      controller.delete(mref.path)
    }

    /* Translate language of the theory */
    var l : Option[Language] = None

    // Build correct union of languages
    if (t.lang.isDefined) {
      if (!tState.languages.exists(la => la.name.toLowerCase == t.lang.get.lang.toLowerCase)) { thy_reset() ; throw new IMPSDependencyException("required language " + t.lang.get.lang + " not found") }
      l = tState.languages.find(la => la.name.toLowerCase == t.lang.get.lang.toLowerCase)
    }

    if (t.cmpntthrs.isDefined)
    {
      /* For each component theory, take its language (if there is one) */
      for (comp_theory <- t.cmpntthrs.get.lst)
      {
        if (!tState.theories_raw.exists(t => t.name.toLowerCase == comp_theory.toLowerCase)) { thy_reset() ; throw new IMPSDependencyException("required co-theory " + comp_theory.toLowerCase + " not found") }

        /* Add Include */
        val component = tState.theories_decl.find(p => p.name.toString.toLowerCase == comp_theory.toLowerCase)
        assert(component.isDefined)
        controller add PlainInclude.apply(component.get.path,nu_theory.path)

        /* Union Languages */
        val t_index : Theory = tState.theories_raw.find(t => t.name.toLowerCase == comp_theory.toLowerCase).get

        if (t_index.lang.isDefined)
        {
          assert(tState.languages.exists(la => la.name.toLowerCase == t_index.lang.get.lang.toLowerCase))
          val l_prime: Language = tState.languages.find(la => la.name.toLowerCase == t_index.lang.get.lang.toLowerCase).get

          if (l.isDefined) {
            l = Some(l.get.union(l_prime))
          } else {
            l = Some(l_prime)
          }
        }
      }
    }

    // Actually translate resulting language
    if (l.isDefined) { doLanguage(l.get, nu_theory) }

    /* Translate all axioms, if there are any */
    if (t.axioms.isDefined)
    {
      var axiom_count : Int = -1
      for (ax <- t.axioms.get.axs)
      {
        val mth : Term = tState.bindUnknowns(IMPSTheory.Thm(doMathExp(ax.formula, nu_theory)))
        val name : String = if (ax.name.isDefined) { ax.name.get }
        else { axiom_count += 1 ; t.name + "_unnamed_axiom" + axiom_count.toString }

        val assumption = symbols.Constant(nu_theory.toTerm,doName(name),Nil,Some(mth),None,Some("Assumption"))

        if (ax.usgs.isDefined) { doUsages(assumption,ax.usgs.get) }
        if (ax.src.isDefined) { doSourceRef(assumption,ax.src.get) }
        controller.add(assumption)
      }
    }

    /* All constants here per distinction element are
       axiomatically distinct from each other */
    if (t.dstnct.isDefined)
    {
      var dist_count : Int = 0

      for (dist : List[String] <- t.dstnct.get.lst) /* Reminder: t.dstnct.get.lst : List[List[String]] */
      {
        for (c1 : String <- dist)
        {
          for (c2 : String <- dist.filter(e => e != c1))
          {
            /* Assert the two constants to be distinct exist in the theory */
            assert(nu_theory.getDeclarations.exists(d => d.name == LocalName(c1)))
            assert(nu_theory.getDeclarations.exists(d => d.name == LocalName(c2)))

            /* add axiom that they are indeed distinct */
            val g1 : GlobalName = nu_theory.getDeclarations.find(d => d.name == LocalName(c1)).get.path
            val g2 : GlobalName = nu_theory.getDeclarations.find(d => d.name == LocalName(c2)).get.path

            // val dist_formula : IMPSMathExp = IMPSNegation(IMPSEquals(IMPSSymbolRef(g1), IMPSSymbolRef(g2)))
            val dist_formula : IMPSMathExp = IMPSNegation(IMPSEquals(IMPSMathSymbol(c1), IMPSMathSymbol(c2)))
            val mth          : Term = tState.bindUnknowns(IMPSTheory.Thm(doMathExp(dist_formula, nu_theory)))
            val name         : String = t.name + "_distinction_axiom_" + dist_count.toString

            dist_count += 1

            val dist_assumption = symbols.Constant(nu_theory.toTerm,doName(name),Nil,Some(mth),None,Some("Assumption"))
            if (t.dstnct.get.src.isDefined) { doSourceRef(dist_assumption, t.dstnct.get.src.get) }
            controller.add(dist_assumption)
          }
        }
      }
    }

    println(" > actually adding theory " + t.name)

    tState.theories_decl = tState.theories_decl :+ nu_theory
    tState.theories_raw  = tState.theories_raw  :+ t

  }

  def doLanguage(l : Language, t : DeclaredTheory) : Unit =
  {
    def doLanguageOrTheory(target : String, t : DeclaredTheory) : Unit =
    {
      val exists_theory   : Boolean = tState.theories_raw.exists(p => p.name.toLowerCase == target.toLowerCase)
      val exists_language : Boolean = tState.languages.exists(p => p.name.toLowerCase == target.toLowerCase)

      if (!(exists_language || exists_theory))
      {
        throw new IMPSDependencyException("neither required theory nor language named " + target.toLowerCase + " not found")
      }

      if (exists_language)
      {
        doLanguage(tState.languages.find(p => p.name.toLowerCase == target.toLowerCase).get, t)
      }
      else if (exists_theory)
      {
        val argt : ArgumentLanguage = tState.theories_raw.find(p => p.name.toLowerCase == target.toLowerCase).get.lang.get
        assert(tState.languages.exists(p => p.name.toLowerCase == argt.lang.toLowerCase))
        doLanguage(tState.languages.find(p => p.name.toLowerCase == argt.lang.toLowerCase).get, t)
      }
    }

    if (l.embedlang.isDefined) {
      doLanguageOrTheory(l.embedlang.get.name,t)
    }

    if (l.embedlangs.isDefined)
    {
      for (l_embed <- l.embedlangs.get.names) {
        doLanguageOrTheory(l_embed, t)
      }
    }

    if (l.bstps.isDefined)
    {
      for (baseType : String <- l.bstps.get.tps)
      {
        val tp : Term = IMPSTheory.Sort(OMS(IMPSTheory.lutinsIndType))
        val basetype = symbols.Constant(t.toTerm, doName(baseType), Nil, Some(tp), None, Some("BaseType"))
        if (l.bstps.get.src.isDefined) { doSourceRef(basetype, l.bstps.get.src.get) }
        controller add basetype
      }
    }

    if (l.srts.isDefined)
    {
      /* introduce all sorts with their respective enclosing sorts */
      for (spec : (IMPSSort, IMPSSort) <- l.srts.get.lst)
        { doSubsort(spec._1, spec._2, t, l.srts.get.src) }
    }

    if (l.extens.isDefined)
    {
      for (tal : TypeSortAList <- l.extens.get.lst)
      {
        // TODO: Can this be translated into something non-opaque?
        //       See IMPS manual, pgs. 172, 173
        val opaque = new OpaqueText(t.path.toDPath, OpaqueText.defaultFormat, StringFragment(tal.toString))
        controller.add(opaque)
      }
    }

    if (l.cnstnts.isDefined)
    {
      for (pair : (String, IMPSSort) <- l.cnstnts.get.lst)
      {
        val mth_tp : Term = doSort(pair._2, t)
        val l_const = symbols.Constant(t.toTerm,doName(pair._1),Nil,Some(mth_tp),None,Some("Constant"))
        if (l.cnstnts.get.src.isDefined) { doSourceRef(l_const,l.cnstnts.get.src.get) }
        controller add l_const
      }
    }
  }

  def doDeclaration (d : LispExp) : Unit =
  {
    // set this to true for helpful debug output
    val debug : Boolean = false
    if (debug)
    {
      println("\n>>>>> Call to doDecl for the following expression:\n")
      println(d.toString)
      for (thy <- tState.theories_decl)
      {
        println("\n<<<<< Theory " + thy.name + " contains the following declarations:")
        for(d <- thy.getDeclarations)
        { println("~~~ " + d.name.toString) }
      }
    }

    d match {
      case Heralding(md, src) => {
        println(" > Dropping (herald ...)")
      }
      case AtomicSort(name, defstring, theory, usages, witness, src, sort) =>

        val ln: LocalName = LocalName(theory.thy.toLowerCase())

        if (!tState.theories_decl.exists(t => t.name.toString.toLowerCase == ln.toString)) {
          throw new IMPSDependencyException("required theory " + ln + " for atomic sort not found")
        }
        val parent: DeclaredTheory = tState.theories_decl.find(dt => dt.name.toString.toLowerCase == ln.toString).get

        val definition: Term = tState.bindUnknowns(doMathExp(defstring, parent))
        val enclosing: Term = doSort(sort, parent)
        val nu_atomicSort = symbols.Constant(parent.toTerm, doName(name), Nil, Some(enclosing), Some(definition), Some("AtomicSort"))

        /* Add available MetaData */
        if (witness.isDefined) {
          doMetaData(nu_atomicSort, "witness", witness.get.witness.toString)
        }
        if (usages.isDefined) {
          doUsages(nu_atomicSort, usages.get.usgs)
        }

        if (src.isDefined) { doSourceRef(nu_atomicSort, src.get) }

        println(" > Adding atomic sort: " + name + " (enclosed by " + sort.toString + ")")
        controller add nu_atomicSort

      case Constant(name, definition, theory, sort, usages, src) =>

        val ln: LocalName = LocalName(theory.thy.toLowerCase())
        if (!tState.theories_decl.exists(t => t.name.toString.toLowerCase == ln.toString)) {
          throw new IMPSDependencyException("required theory " + ln + " for constant not found")
        }
        val parent: DeclaredTheory = tState.theories_decl.find(dt => dt.name.toString.toLowerCase == ln.toString).get

        /* look for sort in given theory. */
        var srt: Option[Term] = None
        if (sort.isDefined) {
          println(" > Adding constant with clear sort: " + name)
          /* Theory not in scope, so we find it by hand */
          val theTheory: Option[DeclaredTheory] = tState.theories_decl.find(x => x.name == LocalName(theory.thy))
          assert(theTheory.isDefined)

          srt = Some(doSort(sort.get.sort, theTheory.get))
        }
        else {
          println(" > Adding constant with unclear sort: " + name)
          srt = None
        }

        val mth: Term = tState.bindUnknowns(doMathExp(definition, parent))
        val nu_constant = symbols.Constant(parent.toTerm, LocalName(name), Nil, srt, Some(mth), Some("Constant"))

        /* Add available MetaData */
        if (src.isDefined) { doSourceRef(nu_constant, src.get) }
        if (usages.isDefined) {
          doUsages(nu_constant, usages.get.usgs)
        }
        controller add nu_constant

      case Theorem(name, formula, lemma, reverse, theory, usages, transport, macete, homeTheory, maybeProof, src) => {

        val ln: LocalName = doName(theory.thy.toLowerCase)
        if (!tState.theories_decl.exists(t => t.name.toString.toLowerCase == ln.toString)) {
          throw new IMPSDependencyException("required theory " + ln + " for theorem not found")
        }
        val parent: DeclaredTheory = tState.theories_decl.find(dt => dt.name.toString.toLowerCase == ln.toString).get

        val mth: Term = tState.bindUnknowns(IMPSTheory.Thm(doMathExp(formula, parent)))
        val nu_theorem = symbols.Constant(parent.toTerm, doName(name), Nil, Some(mth), None, Some("Theorem"))
        //                                                                              ^-- proof goes here!

        /* Add available MetaData */
        if (usages.isDefined) {
          doUsages(nu_theorem, usages.get.usgs)
        }
        if (transport.isDefined) {
          doMetaData(nu_theorem, "translation", transport.get.trans)
        }
        if (macete.isDefined) {
          doMetaData(nu_theorem, "macete", macete.get.macete)
        }
        if (homeTheory.isDefined) {
          doMetaData(nu_theorem, "homeTheory", homeTheory.get.hmthy)
        }

        if (lemma) {
          doMetaData(nu_theorem, "lemma", "present")
        } else {
          doMetaData(nu_theorem, "lemma", "absent")
        }
        if (reverse) {
          doMetaData(nu_theorem, "reverse", "present")
        } else {
          doMetaData(nu_theorem, "reverse", "absent")
        }

        if (src.isDefined) { doSourceRef(nu_theorem, src.get) }
        controller add nu_theorem

        if (maybeProof.isDefined) {
          /* opaque proofs are beetter than no proofs */
          val proof_name: StringFragment = StringFragment("Opaque proof of theorem " + name)
          val proof_text: StringFragment = StringFragment(maybeProof.get.prf.toString)

          val opaque = new OpaqueText(parent.path.toDPath, OpaqueText.defaultFormat, StringFragment(proof_name + "\n" + proof_text))
          controller add opaque
        }
      }
      case Translation(name, force, forceQL, dontEnrich, sourcet, targett, assumptions, fixed, sortpairs, constpairs, coretrans, theintcheck, src) => {

        val ln : LocalName = doName(name)

        println(" > translating Translation " + name)

        // Source and Target need to be defined!
        assert(tState.theories_decl.exists(t => t.name.toString.toLowerCase == doName(sourcet.thy).toString.toLowerCase))
        val source_thy : Term = tState.theories_decl.find(t => t.name.toString.toLowerCase == doName(sourcet.thy).toString.toLowerCase).get.toTerm

        assert(tState.theories_decl.exists(t => t.name.toString.toLowerCase == doName(targett.thy).toString.toLowerCase))
        val target_thy : Term = tState.theories_decl.find(t => t.name.toString.toLowerCase == doName(targett.thy).toString.toLowerCase).get.toTerm

        val nu_view = new DeclaredView(bt.narrationDPath, ln, source_thy, target_thy, false)

        if (force) {
          val mv : GlobalName =     rootdpath ? name ? LocalName("force")
          val mo : Obj        = OMS(rootdpath ? name ? "present")
          nu_view.metadata.add(new MetaDatum(mv,mo))
        } else {
          val mv : GlobalName =     rootdpath ? name ? LocalName("force")
          val mo : Obj        = OMS(rootdpath ? name ? "absent")
          nu_view.metadata.add(new MetaDatum(mv,mo))
        }

        if (forceQL) {
          val mv : GlobalName =     rootdpath ? name ? LocalName("force-under-quick-load")
          val mo : Obj        = OMS(rootdpath ? name ? "present")
          nu_view.metadata.add(new MetaDatum(mv,mo))
        } else {
          val mv : GlobalName =     rootdpath ? name ? LocalName("force-under-quick-load")
          val mo : Obj        = OMS(rootdpath ? name ? "absent")
          nu_view.metadata.add(new MetaDatum(mv,mo))
        }

        if (dontEnrich) {
          val mv : GlobalName =     rootdpath ? name ? LocalName("dont-enrich")
          val mo : Obj        = OMS(rootdpath ? name ? "present")
          nu_view.metadata.add(new MetaDatum(mv,mo))
        } else {
          val mv : GlobalName =     rootdpath ? name ? LocalName("dont-enrich")
          val mo : Obj        = OMS(rootdpath ? name ? "absent")
          nu_view.metadata.add(new MetaDatum(mv,mo))
        }

        println(" > Adding translation " + name + " (not complete yet)")

        if (src.isDefined) { doSourceRef(nu_view.toTerm,src.get) }
        controller add nu_view

      }
      case SchematicMacete(_, _, thy, _, _, _) => {

        val ln: LocalName = LocalName(thy.thy.toLowerCase)
        if (!tState.theories_decl.exists(t => t.name.toString.toLowerCase == ln.toString)) {
          throw new IMPSDependencyException("required theory " + ln + " for schematic macete not found")
        }
        val parent: DeclaredTheory = tState.theories_decl.find(dt => dt.name.toString.toLowerCase == ln.toString).get

        // Macetes are added as opaque (for now?)
        val opaque = new OpaqueText(parent.path.toDPath, OpaqueText.defaultFormat, StringFragment(d.toString))

        /* Opaque Text doesn't have metadata, apparently, so we don't add the src */

        controller add opaque
      }
      case _ => {
        println(" > Error: Unknown decl encountered, not translated!")
        println(" > " + d.toString)
      }
    }
  }

  def findKind(s : IMPSSort) : Term =
  {
    s match {
      case IMPSAtomSort("ind")      => OMS(IMPSTheory.lutinsIndType)
      case IMPSAtomSort("prop")     => OMS(IMPSTheory.lutinsPropType)
      case IMPSAtomSort("bool")     => OMS(IMPSTheory.lutinsPropType)
      case IMPSAtomSort("unit%sort")
         | IMPSAtomSort("unitsort") => OMS(IMPSTheory.lutinsIndType)
      case IMPSAtomSort(_)          => OMS(IMPSTheory.lutinsIndType)
      case IMPSBinaryFunSort(s1,s2) => IMPSTheory.FunType(findKind(s1),findKind(s2))
      case _ => ??? // This should never happen, always call curry first!
    }
  }

  def matchSort(e : IMPSSort, t : DeclaredTheory) : Term =
  {
    e match {
      case IMPSAtomSort("ind")  => OMS(IMPSTheory.lutinsPath ? "ind")
      case IMPSAtomSort("prop") => OMS(IMPSTheory.lutinsPath ? "bool")
      case IMPSAtomSort("bool") => OMS(IMPSTheory.lutinsPath ? "bool")
      case IMPSAtomSort("unit%sort")
         | IMPSAtomSort("unitsort") => OMS(IMPSTheory.lutinsPath ? "unitsort")
      case IMPSAtomSort(srt) => OMS(t.path ? srt)
      case IMPSBinaryFunSort(s1, s2) =>
      {
        val tpA: Term = findKind(s1)
        val tpB: Term = findKind(s2)

        val sortA: Term = matchSort(s1, t)
        val sortB: Term = matchSort(s2, t)

        IMPSTheory.FunSort(tpA, tpB, sortA, sortB)
      }
    }
  }

  /* Walks sort structure, currying all NaryFunSorts into BinaryFunSorts */
  def curry(srt : IMPSSort) : IMPSSort =
  {
    srt match
    {
      case IMPSAtomSort(_)                 => srt // don't change atomic sorts
      case IMPSBinaryFunSort(sort1, sort2) => IMPSBinaryFunSort(curry(sort1),curry(sort2))
      case IMPSNaryFunSort(sorts)          => {
        if (sorts.length == 2) {
          IMPSBinaryFunSort(curry(sorts(0)), curry(sorts(1)))
        } else {
          IMPSBinaryFunSort(curry(sorts(0)), curry(IMPSNaryFunSort(sorts.tail)))
        }
      }
    }
  }

  def doSort(d : IMPSSort, t : DeclaredTheory) : Term =
  {
    val d_prime : IMPSSort = curry(d)
    val tp      : Term     = findKind(d_prime)

    IMPSTheory.exp(tp, matchSort(d_prime,t))
  }

  /* Introduces a sort to a theory and also assigns the enclosing sort to it. */
  def doSubsort(subsort : IMPSSort, supersort : IMPSSort, thy : DeclaredTheory, src : Option[SourceRef]) : Unit =
  {
    /* enclosing sort should already be defined */
    println(" > Adding sort: " + subsort.toString + ", enclosed by " + supersort.toString)

    val opt_ind   : Option[Term] = Some(Apply(OMS(IMPSTheory.lutinsPath ? LocalName("sort")), OMS(IMPSTheory.lutinsIndType)))
    val jdgmtname : LocalName    = LocalName(subsort.toString + "_sub_" + supersort.toString)

    val foo       : Term = matchSort(subsort,thy)
    val bar       : Term = matchSort(supersort,thy)
    val baz       : Term = findKind(supersort)

    val subs      : Term = ApplySpine(OMS(IMPSTheory.lutinsPath ? LocalName("subsort")), baz, foo, bar)

    val jdgmttp   : Option[Term] = Some(IMPSTheory.Thm(subs))

    /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

    val typing    : Declaration  = symbols.Constant(thy.toTerm,LocalName(subsort.toString),Nil,opt_ind,None,Some("Subsort_1"))
    val judgement : Declaration  = symbols.Constant(thy.toTerm,jdgmtname,Nil,jdgmttp,None,Some("Subsort_2"))

    if (src.isDefined)
    {
      doSourceRef(typing, src.get)
      doSourceRef(judgement, src.get)
    }

    controller add typing
    controller add judgement
  }

  /* Translate IMPS Math Expressions to Terms */
  def doMathExp(d : IMPSMathExp, thy : DeclaredTheory) : Term =
  {
    d match
    {
      case IMPSVar(v)             => OMV(v)

      case IMPSMathSymbol("an%individual") => { OMS(IMPSTheory.lutinsPath ? "anIndividual") }
      case IMPSMathSymbol(s)      => OMS(thy.path ? LocalName(s))

      case q@IMPSTruth()          => OMS(IMPSTheory.lutinsPath ? "thetrue")
      case q@IMPSFalsehood()      => OMS(IMPSTheory.lutinsPath ? "thefalse")

      case IMPSNegation(p)        => IMPSTheory.Negation(doMathExp(p,thy))

      case IMPSIf(p,t1,t2)        => IMPSTheory.If(tState.addUnknown(), tState.addUnknown(), doMathExp(p,thy), doMathExp(t1,thy), doMathExp(t2,thy))
      case IMPSIff(p, q)          => IMPSTheory.Iff(doMathExp(p,thy), doMathExp(q,thy))
      case IMPSIfForm(p,q,r)      => IMPSTheory.If_Form(doMathExp(p,thy), doMathExp(q,thy), doMathExp(r,thy))
      case IMPSEquals(a,b)        => IMPSTheory.Equals(tState.addUnknown(),tState.addUnknown(),tState.addUnknown(),doMathExp(a,thy),doMathExp(b,thy))
      case IMPSDisjunction(ls)    => IMPSTheory.Or(ls map (x => doMathExp(x,thy)))
      case IMPSConjunction(ls)    => IMPSTheory.And(ls map (x => doMathExp(x,thy)))
      case q@IMPSLambda(_,_)      => doIMPSLambda(q, thy)
      case q@IMPSForAll(_,_)      => doIMPSForall(curryIMPSforall(q), thy)
      case IMPSForSome(vs, r)     => IMPSTheory.Forsome(vs map (p => (LocalName(p._1.v), p._2 map (x => doSort(x,thy)))), doMathExp(r,thy))
      case IMPSImplication(p,q)   => IMPSTheory.Implies(doMathExp(p,thy), doMathExp(q,thy))
      case IMPSApply(f,ts)        =>
      {
        assert(ts.nonEmpty)
        // Wheee, manual currying!
        if (ts.length == 1)
          { IMPSTheory.IMPSApply(tState.addUnknown(),tState.addUnknown(),tState.addUnknown(),tState.addUnknown(),tState.addUnknown(),doMathExp(f,thy),doMathExp(ts.head,thy)) }
        else
        {
          val inner = IMPSApply(f,List(ts.head))
          doMathExp(IMPSApply(inner,ts.tail),thy)
        }
      }
      case IMPSIota(v1,s1,p)      => IMPSTheory.Iota(LocalName(v1.v), doSort(s1,thy), doMathExp(p,thy))
      case IMPSIotaP(v1,s1,p)     => IMPSTheory.IotaP(LocalName(v1.v), doSort(s1,thy), doMathExp(p,thy))
      case IMPSIsDefined(r)       => IMPSTheory.IsDefined(tState.addUnknown(), tState.addUnknown(), doMathExp(r,thy))
      case IMPSIsDefinedIn(r,s)   => IMPSTheory.IsDefinedIn(tState.addUnknown(), tState.addUnknown(), doMathExp(r,thy), doSort(s,thy))
      case IMPSUndefined(s)       => IMPSTheory.Undefined(findKind(s), matchSort(s,thy))

      case IMPSTotal(f,bs)        => IMPSTheory.Total(doMathExp(f,thy),bs.map(b => doSort(b,thy)))
      case IMPSNonVacuous(p)      => IMPSTheory.Nonvacuous(doMathExp(p,thy))
      case IMPSQuasiEquals(p,q)   => IMPSTheory.Quasiequals(doMathExp(p,thy), doMathExp(q,thy))
    }
  }

  /* TRANSLATING SINGLE MATH EXPRESSIONS */

  def replaceVars(vs : List[IMPSVar], t : IMPSMathExp) : IMPSMathExp =
  {
    t match {
      case IMPSMathSymbol(s) =>
      {
        var n : IMPSMathExp = t
        for (v <- vs) { if (v.v == s) {n = v} }
        n
      }
      case IMPSVar(_)
         | IMPSTruth()
         | IMPSFalsehood()
         | IMPSUndefined(_)     => t
      case IMPSNegation(p)      => IMPSNegation(replaceVars(vs,t))
      case IMPSIf(p,t1,t2)      => IMPSIf(replaceVars(vs,p),replaceVars(vs,t1),replaceVars(vs,t2))
      case IMPSIff(p, q)        => IMPSIff(replaceVars(vs,p),replaceVars(vs,q))
      case IMPSIfForm(p,q,r)    => IMPSIfForm(replaceVars(vs,p),replaceVars(vs,q),replaceVars(vs,r))
      case IMPSEquals(a,b)      => IMPSEquals(replaceVars(vs,a),replaceVars(vs,b))
      case IMPSDisjunction(ls)  => IMPSDisjunction(ls map (x => replaceVars(vs,x)))
      case IMPSConjunction(ls)  => IMPSConjunction(ls map (x => replaceVars(vs,x)))
      case IMPSLambda(ws,r)     => IMPSLambda(ws,replaceVars(vs,r))
      case IMPSForAll(ws,r)     => IMPSForAll(ws,replaceVars(vs,r))
      case IMPSForSome(ws, r)   => IMPSForSome(ws,replaceVars(vs,r))
      case IMPSImplication(p,q) => IMPSImplication(replaceVars(vs,p), replaceVars(vs,q))
      case IMPSApply(f,ts)      => IMPSApply(replaceVars(vs,f), ts map (x => replaceVars(vs,x)))
      case IMPSIota(v1,s1,p)    => IMPSIota(v1,s1,replaceVars(vs,p))
      case IMPSIotaP(v1,s1,p)   => IMPSIotaP(v1,s1,replaceVars(vs,p))
      case IMPSIsDefined(r)     => IMPSIsDefined(replaceVars(vs,r))
      case IMPSIsDefinedIn(r,s) => IMPSIsDefinedIn(replaceVars(vs,r),s)
    }
  }

  def doIMPSLambda(lambda : IMPSLambda, thy : DeclaredTheory) : Term =
  {
    assert(lambda.vs.nonEmpty)

    /* Filling implicit sorts in lambda variables, IMPS allows for these */
    /* Example: lambda(x,y:zz,x+y) -> lambda(x:zz,y:zz,x+y) */
    var filled_vs : List[(IMPSVar, IMPSSort)] = List.empty

    /* Last variable sort must be defined */
    assert(lambda.vs.last._2.isDefined)
    var latersort : IMPSSort = curry(lambda.vs.last._2.get)

    for (i <- ((lambda.vs.length-1) to 0 by -1))
    {
      val thisSort : IMPSSort = if (lambda.vs(i)._2.isDefined)
      { lambda.vs(i)._2.get } else { latersort }
      latersort = thisSort

      val k = filled_vs.length
      filled_vs = (lambda.vs(i)._1, thisSort) :: filled_vs
      assert(filled_vs.length > k)
    }
    println(" > doing IMPSLambda " + IMPSForAll(filled_vs.map(k => (k._1,Some(k._2))),lambda.t))

    val final_vs : List[(LocalName, Term)] = filled_vs map (p => (LocalName(p._1.v),  matchSort(curry(p._2),thy)))

    /* Translate body */
    val target   : Term = doMathExp(replaceVars(filled_vs map (x => x._1),lambda.t),thy)

    /* Manual currying because MMT doesn't support flexary anything */
    def curryLambda(vs : List[(LocalName, Term)], body : Term) : Term =
    {
      assert(vs.nonEmpty)
      if (vs.length == 1)
      {
        val lflambda : Term = info.kwarc.mmt.lf.Lambda(vs.head._1,vs.head._2,body)
        IMPSTheory.Lambda(tState.addUnknown(),tState.addUnknown(),vs.head._2,tState.addUnknown(),lflambda)
      }
      else
      {
        val inner   : Term = info.kwarc.mmt.lf.Lambda(vs.last._1,vs.last._2,body)
        val newBody : Term = IMPSTheory.Lambda(tState.addUnknown(),tState.addUnknown(),vs.last._2,tState.addUnknown(),inner)
        curryLambda(vs.init,newBody)
      }
    }
    curryLambda(final_vs,target)
  }

  def curryIMPSforall(f : IMPSForAll) : IMPSForAll =
  {
    assert(f.vs.nonEmpty)
    assert(f.vs.forall(v => v._2.isDefined))
    if (f.vs.length == 1) { f }
    else {
      IMPSForAll(List(f.vs.head),curryIMPSforall(IMPSForAll(f.vs.tail,f.p)))
    }
  }

  def doIMPSForall(forall : IMPSForAll, thy : DeclaredTheory) : Term =
  {
    // Always call curried
    assert(forall.vs.length == 1)

    val thisVar     : LocalName = LocalName(forall.vs.head._1.v)
    val thisSrt     : IMPSSort  = forall.vs.head._2.get
    val expSortTerm : Term      = doSort(thisSrt,thy)     // <-- this is "exp whateversort"
    val target      : Term      = doMathExp(forall.p,thy)
    val body        : Term      = info.kwarc.mmt.lf.Lambda(thisVar, expSortTerm, target)

    val jstSortTerm : Term      = matchSort(thisSrt,thy)
    //                                ^-------v-------------------These are just the sort
    IMPSTheory.Forall(findKind(thisSrt), jstSortTerm, body)
  }
}

class IMPSDependencyException(message: String) extends Exception(message) {

  def this(message: String, cause: Throwable) {
    this(message)
    initCause(cause)
  }

  def this(cause: Throwable) {
    this(Option(cause).map(_.toString).orNull, cause)
  }

  def this() {
    this(null: String)
  }
}

