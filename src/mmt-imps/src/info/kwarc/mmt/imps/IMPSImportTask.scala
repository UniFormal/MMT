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
import info.kwarc.mmt.api.symbols.{Declaration, PlainInclude, TermContainer}
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

  val rootdpath : DPath            = DPath(URI.http colon "imps.mcmaster.ca") /* arbitrary, but seemed fitting */

  /* Source References. */
  def doSourceRefT(t : Term,        s : SourceInfo) : Unit = if (s.isDefined) s.get match {
    case l@scala.util.Left(_)    => ??!(l)
    case r@scala.util.Right(src) => SourceRef.update(t, src)
  }

  def doSourceRefD(d : Declaration, s : SourceInfo) : Unit = if (s.isDefined) s.get match {
    case l@scala.util.Left(_)    => ??!(l)
    case r@scala.util.Right(src) => SourceRef.update(d, src)
  }

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

	def doDocument(es : List[DefForm], uri : URI) : BuildResult =
	{
    val doc = new Document(DPath(uri), true)
    controller.add(doc)

    var excps : List[Exception] = Nil

    for (exp <- es)
    {
      exp match
      {
        /* Translating Theories to MMT */
        case t@(DFTheory(_,_,_,_,_,_,_))       => try
        {
          if (!tState.theories_raw.contains(t)) { doTheory(t, doc.path, bt.narrationDPath) }
        } catch {
          case e : IMPSDependencyException => { println(" > ... fail. Add to stack: " +  e.getMessage ) ; excps = excps.::(e) }
        }
        // Languages are processed in context of theories using them, not by themselves
        case l@(DFLanguage(_,_,_,_,_,_,_,_,_)) => {
          if (!tState.languages.contains(l)) {
            if (tState.verbosity > 0)
            {
              println(" > adding language " + l.name)
            }
            tState.languages = tState.languages :+ l
          }
        }
        // If it's none of these, fall back to doDeclaration
        case _                             => doDeclaration(exp)
      }
    }

    if (excps.nonEmpty) { throw excps.head }

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
          val ce = new CheckingEnvironment(controller.simplifier,new ErrorLogger(report),RelationHandler.ignore,this)
          checker.apply(p)(ce)
        }
      }
    }

    index(doc)
    BuildSuccess(Nil,Nil)
	}

  def doTheory (t : DFTheory, docPath: DPath, ns:DPath) : Unit =
  {
    val nu_theory = new DeclaredTheory(ns,
                                       LocalName(t.name.toString),
                                       Some(IMPSTheory.QCT.quasiLutinsPath),
                                       modules.Theory.noParams,
                                       modules.Theory.noBase)

    if (tState.verbosity > 1)
    {
      println(" > trying to add theory " + t.name.toString)
    } else if (tState.verbosity > 0)
    {
      println(" > adding theory " + t.name.toString)
    }

    val mref : MRef = MRef(docPath,nu_theory.path)
    controller.add(nu_theory)
    controller.add(mref)

    def thy_reset() : Unit = {
      controller.delete(nu_theory.path)
      controller.delete(mref.path)
    }

    /* Translate language of the theory */
    var l : Option[DFLanguage] = None

    // Build correct union of languages
    if (t.lang.isDefined) {
      if (!tState.languages.exists(la => la.name.toString.toLowerCase == t.lang.get.lang.toString.toLowerCase)) { thy_reset() ; throw new IMPSDependencyException("required language " + t.lang.get.lang + " not found") }
      l = tState.languages.find(la => la.name.toString.toLowerCase == t.lang.get.lang.toString.toLowerCase)
    }

    if (t.comp.isDefined)
    {
      /* For each component theory, take its language (if there is one) */
      for (comp_theory <- t.comp.get.cps)
      {
        if (!tState.theories_raw.exists(t => t.name.toString.toLowerCase == comp_theory.toString.toLowerCase)) { thy_reset() ; throw new IMPSDependencyException("required co-theory " + comp_theory.toLowerCase + " not found") }

        /* Add Include */
        val component = tState.theories_decl.find(p => p.name.toString.toLowerCase == comp_theory.toString.toLowerCase)
        assert(component.isDefined)
        if (tState.verbosity > 0)
        {
          println("   > adding include of " + comp_theory.toString.toLowerCase)
        }
        controller add PlainInclude(component.get.path,nu_theory.path)

        /* Union Languages */
        val t_index : DFTheory = tState.theories_raw.find(t => t.name.toString.toLowerCase == comp_theory.toString.toLowerCase).get

        if (t_index.lang.isDefined)
        {
          assert(tState.languages.exists(la => la.name.toString.toLowerCase == t_index.lang.get.lang.toString.toLowerCase))
          val l_prime: DFLanguage = tState.languages.find(la => la.name.toString.toLowerCase == t_index.lang.get.lang.toString.toLowerCase).get

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
      for (ax <- t.axioms.get.cps)
      {
        val mth : Term = tState.bindUnknowns(IMPSTheory.Thm(doMathExp(ax.frm.get, nu_theory, Nil)))
        val name : String = if (ax.name.isDefined) { ax.name.get }
        else { axiom_count += 1 ; t.name + "_unnamed_axiom" + axiom_count.toString }

        val assumption : Declaration = symbols.Constant(nu_theory.toTerm,doName(name),Nil,Some(mth),None,Some("Assumption"))

        if (ax.usgs.isDefined) { doUsages(assumption,ax.usgs.get) }
        if (ax.src.isDefined) { doSourceRefD(assumption,ax.src) }
        controller.add(assumption)
      }
    }

    /* All constants here per distinction element are
       axiomatically distinct from each other */
    if (t.dstnct.isDefined)
    {
      var dist_count : Int = 0

      for (dist : List[Name] <- t.dstnct.get.ds) /* Reminder: t.dstnct.get.lst : List[List[Name]] */
      {
        for (c1 : Name <- dist)
        {
          for (c2 : Name <- dist.filter(e => e != c1))
          {
            /* Assert the two constants to be distinct exist in the theory */
            assert(nu_theory.getDeclarations.exists(d => d.name == LocalName(c1.s)))
            assert(nu_theory.getDeclarations.exists(d => d.name == LocalName(c2.s)))

            /* add axiom that they are indeed distinct */
            val g1 : GlobalName = nu_theory.getDeclarations.find(d => d.name == LocalName(c1.s)).get.path
            val g2 : GlobalName = nu_theory.getDeclarations.find(d => d.name == LocalName(c2.s)).get.path

            val dist_formula : IMPSMathExp = IMPSNegation(IMPSEquals(IMPSMathSymbol(c1.s), IMPSMathSymbol(c2.s)))
            val mth          : Term = tState.bindUnknowns(IMPSTheory.Thm(doMathExp(dist_formula, nu_theory, Nil)))
            val name         : String = t.name + "_distinction_axiom_" + dist_count.toString

            dist_count += 1

            val dist_assumption = symbols.Constant(nu_theory.toTerm,doName(name),Nil,Some(mth),None,Some("Assumption"))
            if (t.dstnct.get.src.isDefined) { doSourceRefD(dist_assumption, t.dstnct.get.src) }
            controller.add(dist_assumption)
          }
        }
      }
    }

    if (t.name.s != "the-kernel-theory")
    {
      if (tState.verbosity > 0)
      {
        println(" > adding include for kernel theory")
      }

      val component = tState.theories_decl.find(p => p.name.toString.toLowerCase == "the-kernel-theory")
      assert(component.isDefined)
      controller add PlainInclude.apply(component.get.path,nu_theory.path)
    }

    if (tState.verbosity > 1)
    {
      println(" > actually adding theory " + t.name)
    }

    tState.theories_decl = tState.theories_decl :+ nu_theory
    tState.theories_raw  = tState.theories_raw  :+ t

  }

  def doLanguage(l : DFLanguage, t : DeclaredTheory) : Unit =
  {
    def doLanguageOrTheory(target : String, t : DeclaredTheory) : Unit =
    {
      val exists_theory   : Boolean = tState.theories_raw.exists(p => p.name.toString.toLowerCase == target.toLowerCase)
      val exists_language : Boolean = tState.languages.exists(p => p.name.toString.toLowerCase == target.toLowerCase)

      if (!(exists_language || exists_theory))
      {
        throw new IMPSDependencyException("neither required theory nor language named " + target.toLowerCase + " not found")
      }

      if (exists_language)
      {
        doLanguage(tState.languages.find(p => p.name.toString.toLowerCase == target.toLowerCase).get, t)
      }
      else if (exists_theory)
      {
        assert(tState.theories_raw.exists(p => p.name.toString.toLowerCase == target.toLowerCase))
        val argt = tState.theories_raw.find(p => p.name.toString.toLowerCase == target.toLowerCase).get
        if (argt.lang.isDefined)
        {
          assert(tState.languages.exists(p => p.name.toString.toLowerCase == argt.lang.get.lang.toString.toLowerCase))
          doLanguage(tState.languages.find(p => p.name.toString.toLowerCase == argt.lang.get.lang.toString.toLowerCase).get, t)
        }
      }
    }

    if (l.el.isDefined) {
      doLanguageOrTheory(l.el.get.nm.s,t)
    }

    if (l.els.isDefined)
    {
      for (l_embed <- l.els.get.nms.map(_.s)) {
        doLanguageOrTheory(l_embed, t)
      }
    }

    if (l.bt.isDefined)
    {
      for (baseType : IMPSSort <- l.bt.get.nms)
      {
        if (tState.verbosity > 0) {
          println(" > adding base type: " + baseType.toString + " to " + t.name)
        }

        val tp : Term = IMPSTheory.Sort(OMS(IMPSTheory.lutinsIndType))
        val basetype = symbols.Constant(t.toTerm, doName(baseType.toString), Nil, Some(tp), None, Some("BaseType"))
        if (l.bt.get.src.isDefined) { doSourceRefD(basetype, l.bt.get.src) }
        controller add basetype
      }
    }

    if (l.srts.isDefined)
    {
      /* introduce all sorts with their respective enclosing sorts */
      for (spec : ArgSortSpec <- l.srts.get.specs)
        { doSubsort(spec.sub, spec.enc, t, spec.src) }
    }

    if (l.ex.isDefined)
    {
      for (tal : ArgTypeSortAList <- l.ex.get.specs)
      {
        var name : String = tal.srt.toString + "_is_"

        val sub : Term = tal.tp match
        {
          case NumericalType.INTEGERTYPE  => {
            name = name + "integer_type"
            OMS(IMPSTheory.lutinsPath ? "integerType")
          }
          case NumericalType.RATIONALTYPE => {
            name = name + "rational_type"
            OMS(IMPSTheory.lutinsPath ? "rationalType")
          }
          case NumericalType.OCTETTYPE    => {
            name = name + "octet_type"
            ???
          }
        }

        val srt = tState.bindUnknowns(matchSort(tal.srt,t))
        val knd = tState.bindUnknowns(findKind(tal.srt))
        val trm = ApplySpine(OMS(IMPSTheory.lutinsPath ? LocalName("subsort")), knd, sub, srt)
        val jdgmttp   : Option[Term] = Some(IMPSTheory.Thm(trm))
        val judgement : Declaration  = symbols.Constant(t.toTerm, LocalName(name),Nil,jdgmttp,None,Some("Numerical Type Subsort"))

        if (tState.verbosity > 0)
        {
          println(" > adding " + name)
        }

        if (tal.src.isDefined) { doSourceRefD(judgement, tal.src) }
        controller add judgement
      }
    }

    if (l.cnsts.isDefined)
    {
      for (pair : ArgConstantSpec <- l.cnsts.get.specs)
      {
        val mth_tp : Term = tState.bindUnknowns(doSort(pair.enc, t))
        val l_const = symbols.Constant(t.toTerm,doName(pair.nm.s),Nil,Some(mth_tp),None,Some("Constant"))
        if (l.cnsts.get.src.isDefined) { doSourceRefD(l_const,l.cnsts.get.src) }
        controller add l_const
      }
    }
  }

  def doDeclaration (d : DefForm) : Unit =
  {
    // set this to true for helpful debug output
    val debug : Boolean = false
    if (debug)
    {
      if (tState.verbosity > 0)
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
    }

    d match {

      case DFAtomicSort(name,dfs,frm,sort,thy,usgs,witness,src,cmt) =>
      {
        val ln: LocalName = LocalName(thy.thy.s.toLowerCase())

        if (!tState.theories_decl.exists(t => t.name.toString.toLowerCase == ln.toString)) {
          throw new IMPSDependencyException("required theory " + ln + " for atomic sort not found")
        }
        val parent: DeclaredTheory = tState.theories_decl.find(dt => dt.name.toString.toLowerCase == ln.toString).get

        val tp : Term = IMPSTheory.Sort(OMS(IMPSTheory.lutinsIndType))
        val nu_atomicSort = symbols.Constant(parent.toTerm, doName(name.s), Nil, Some(tp), None, Some("Atomic Sort"))

        /* Add available MetaData */
        if (witness.isDefined) {
          doMetaData(nu_atomicSort, "witness", witness.get.w.toString)

          if (tState.verbosity > 1)
          {
            println(" > adding atomic sort " + name + "with defstring: " + dfs.toString)
          }

          val exp : IMPSMathExp = IMPSApply(frm,List(IMPSMathSymbol(witness.get.w.s)))

          val wit : Term = tState.bindUnknowns(IMPSTheory.Thm(doMathExp(exp, parent, Nil)))
          val fin = symbols.Constant(parent.toTerm, LocalName(name + "_witness"),Nil,Some(wit),None,Some("Atomic sort witness theorem"))

          controller add fin
        }
        if (usgs.isDefined) {
          doUsages(nu_atomicSort, usgs.get.usgs)
        }

        doSourceRefD(nu_atomicSort, src)

        if (tState.verbosity > 0)
        {
          println(" > Adding atomic sort: " + name + " (enclosed by " + sort.toString + ")")
        }

        controller add nu_atomicSort

        doSubsort(IMPSAtomSort(name.s), sort, parent, src)
      }

      case DFConstant(name,definition,frm,sort,thy,argsort,usages,src,cmt) =>
      {
        val ln: LocalName = LocalName(thy.thy.s.toLowerCase())

        if (!tState.theories_decl.exists(t => t.name.toString.toLowerCase == ln.toString)) {
          throw new IMPSDependencyException("required theory " + ln + " for constant not found")
        }
        val parent: DeclaredTheory = tState.theories_decl.find(dt => dt.name.toString.toLowerCase == ln.toString).get

        val srt : Term  = tState.bindUnknowns(doSort(curry(sort), parent))
        val mth : Term  = tState.bindUnknowns(doMathExp(frm, parent,Nil))
        val nu_constant = symbols.Constant(parent.toTerm, LocalName(name.s.toLowerCase()), Nil, Some(srt), Some(mth), Some("Constant"))

        /* Add available MetaData */
        doSourceRefD(nu_constant, src)
        if (usages.isDefined) { doUsages(nu_constant, usages.get.usgs) }

        if (tState.verbosity > 0)
        {
          println(" > Adding constant: " + name.s.toLowerCase + " : " + sort.toString)
        }

        controller add nu_constant
      }

      case RecursiveConstant(names,maths,sorts,argthy,usgs,definame,src) =>
      {
        val ln: LocalName = LocalName(argthy.thy.toLowerCase())
        if (!tState.theories_decl.exists(t => t.name.toString.toLowerCase == ln.toString)) {
          throw new IMPSDependencyException("required theory " + ln + " for constant not found")
        }
        val parent: DeclaredTheory = tState.theories_decl.find(dt => dt.name.toString.toLowerCase == ln.toString).get

        val theseSorts = sorts.map(curry)

        assert(names.lengthCompare(maths.length) == 0)
        assert(maths.lengthCompare(theseSorts.length) == 0)

        for (i <- names.indices)
        {
          val nm  : LocalName = LocalName(names(i).toLowerCase)

          var srt : Term      = tState.bindUnknowns(doSort(theseSorts(i), parent))
          val mth : Term      = tState.bindUnknowns(doMathExp(maths(i), parent, Nil))

          val nu_constant     = symbols.Constant(parent.toTerm, nm, Nil, Some(srt), Some(mth), Some("Recursive Constant"))

          /* Add available MetaData */
          if (src.isDefined)  { doSourceRefD(nu_constant, src.get) }
          if (usgs.isDefined) { doUsages(nu_constant, usgs.get.usgs) }

          if (tState.verbosity > 0)
          {
            println(" > adding recursive constant " + nm + " : "  + theseSorts(i).toString)
          }

          controller add nu_constant
        }
      }

      case Theorem(name, formula, lemma, reverse, theory, usages, transport, macete, homeTheory, maybeProof, src) =>
      {
        val ln: LocalName = doName(theory.thy.toLowerCase)
        if (!tState.theories_decl.exists(t => t.name.toString.toLowerCase == ln.toString)) {
          throw new IMPSDependencyException("required theory " + ln + " for theorem not found")
        }
        val parent: DeclaredTheory = tState.theories_decl.find(dt => dt.name.toString.toLowerCase == ln.toString).get

        val mth: Term = tState.bindUnknowns(IMPSTheory.Thm(doMathExp(formula, parent,Nil)))
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

        if (maybeProof.isDefined) {
          if (tState.verbosity > 1)
          {
            println(" > Adding proof!")
          }

          /* opaque proofs are beetter than no proofs */
          val proof_name: StringFragment = StringFragment("Opaque proof of theorem " + name)
          val proof_text: StringFragment = StringFragment(maybeProof.get.prf.toString)

          val opaque = new OpaqueText(parent.path.toDPath, OpaqueText.defaultFormat, StringFragment(proof_name + "\n" + proof_text))
          controller add opaque
        }

        if (src.isDefined) { doSourceRefD(nu_theorem, src.get) }
        controller add nu_theorem

        if (tState.verbosity > 0)
        {
          println(" > adding theorem " + name + " to theory " + parent.name)
        }
      }
      case Translation(name, force, forceQL, dontEnrich, sourcet, targett, assumptions, fixed, sortpairs, constpairs, coretrans, theintcheck, src) => {

        val ln : LocalName = doName(name)

        if (tState.verbosity > 0)
        {
          println(" > translating Translation " + name)
        }

        // Source and Target need to be defined!
        assert(tState.theories_decl.exists(t => t.name.toString.toLowerCase == doName(sourcet.thy).toString.toLowerCase))
        val source_thy : Term = tState.theories_decl.find(t => t.name.toString.toLowerCase == doName(sourcet.thy).toString.toLowerCase).get.toTerm

        assert(tState.theories_decl.exists(t => t.name.toString.toLowerCase == doName(targett.thy).toString.toLowerCase))
        val target_thy : Term = tState.theories_decl.find(t => t.name.toString.toLowerCase == doName(targett.thy).toString.toLowerCase).get.toTerm

        val nu_view = new DeclaredView(bt.narrationDPath, ln, TermContainer(source_thy), TermContainer(target_thy), false)

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

        if (tState.verbosity > 0)
        {
          println(" > Adding translation " + name + " (not complete yet)")
        }

        doSourceRefT(nu_view.toTerm,src)
        controller add nu_view

      }
      case DFSchematicMacete(name,dfs,_,_,thy,src,cmt) => {

        val ln: LocalName = LocalName(thy.thy.s.toLowerCase)
        if (!tState.theories_decl.exists(t => t.name.toString.toLowerCase == ln.toString)) {
          throw new IMPSDependencyException("required theory " + ln + " for schematic macete not found")
        }
        val parent: DeclaredTheory = tState.theories_decl.find(dt => dt.name.toString.toLowerCase == ln.toString).get

        // Macetes are added as opaque (for now?)
        val opaque = new OpaqueText(parent.path.toDPath, OpaqueText.defaultFormat, StringFragment(d.toString))

        /* Opaque Text doesn't have metadata, apparently, so we don't add the src */

        controller add opaque
      }
      case DFQuasiConstructor(name,dfs,arglang,fixedthys,src,cmt) =>
      {
        // Quasi-Constructors needed to be built in because they're not parseable

        assert(tState.languages.exists(p => p.name.s.toLowerCase == arglang.lang.s.toLowerCase)
           ||  tState.theories_raw.exists(p => p.name.s.toLowerCase == arglang.lang.s.toLowerCase))

        val parent: DeclaredTheory = if (!tState.languages.exists(p => p.name.s.toLowerCase == arglang.lang.s.toLowerCase))
        {
          tState.theories_decl.find(dt => dt.name.toString.toLowerCase == LocalName(arglang.lang.s).toString.toLowerCase).get
        }
        else
        {
          // Find correct theory for language.
          val ln: LocalName = LocalName(tState.theories_raw.find(t => t.lang.get.lang.s.toLowerCase == arglang.lang.s.toLowerCase).head.name.s)
          if (!tState.theories_decl.exists(t => t.name.toString.toLowerCase == ln.toString.toLowerCase)) {
            throw new IMPSDependencyException("required theory " + ln + " for quasi-constructor not found")
          }

          tState.theories_decl.find(dt => dt.name.toString.toLowerCase == ln.toString.toLowerCase).get
        }

        val opaque = new OpaqueText(parent.path.toDPath, OpaqueText.defaultFormat, StringFragment(d.toString))

        controller add opaque
      }
      case some => {
        if (tState.verbosity > 0)
        {
          println(" > Error: Unknown decl encountered, not translated!")
          println(some)
        }
      }
    }
  }

  def findKind(sort : IMPSSort) : Term =
  {
    if (sort.isInstanceOf[IMPSNaryFunSort]) { return findKind(curry(sort))}

    sort match
    {
      case IMPSUnknownSort(h)       => tState.doUnknown(Some(h))
      case IMPSAtomSort("ind")      => OMS(IMPSTheory.lutinsIndType)
      case IMPSAtomSort("prop")     => OMS(IMPSTheory.lutinsPropType)
      case IMPSAtomSort("bool")     => OMS(IMPSTheory.lutinsPropType)
      case IMPSAtomSort("unit%sort")
         | IMPSAtomSort("unitsort") => OMS(IMPSTheory.lutinsIndType)
      case IMPSAtomSort(_)          => OMS(IMPSTheory.lutinsIndType)
      case IMPSBinaryFunSort(s1,s2) => IMPSTheory.FunType(findKind(s1),findKind(s2))
      case IMPSSetSort(s)           => IMPSTheory.FunType(findKind(s),OMS(IMPSTheory.lutinsIndType))
      case _ => ??? // This should never happen, always call curry first!
    }
  }

  def matchSort(e : IMPSSort, t : DeclaredTheory) : Term =
  {
    if (e.isInstanceOf[IMPSNaryFunSort]) { return matchSort(curry(e),t)}

    e match {
      case IMPSUnknownSort(h)   => tState.doUnknown(Some(h))
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
      case IMPSSetSort(s) => {

        val tp  : Term = findKind(s)
        val srt : Term = matchSort(s,t)

        IMPSTheory.Sets(tp,srt)
      }
    }
  }

  /* Walks sort structure, currying all NaryFunSorts into BinaryFunSorts */
  def curry(srt : IMPSSort) : IMPSSort =
  {
    srt match
    {
      case IMPSUnknownSort(_)
         | IMPSAtomSort(_)                 => srt // don't change atomic sorts
      case IMPSBinaryFunSort(sort1, sort2) => IMPSBinaryFunSort(curry(sort1),curry(sort2))
      case IMPSNaryFunSort(sorts)          => {
        if (sorts.length == 2) {
          IMPSBinaryFunSort(curry(sorts(0)), curry(sorts(1)))
        } else {
          IMPSBinaryFunSort(curry(sorts(0)), curry(IMPSNaryFunSort(sorts.tail)))
        }
      }
      case IMPSSetSort(st) => IMPSSetSort(curry(st))
    }
  }

  def doSort(d : IMPSSort, t : DeclaredTheory) : Term =
  {
    val d_prime : IMPSSort = curry(d)
    val tp      : Term     = findKind(d_prime)

    IMPSTheory.exp(tp, matchSort(d_prime,t))
  }

  /* Introduces a sort to a theory and also assigns the enclosing sort to it. */
  def doSubsort(subsort : IMPSSort, supersort : IMPSSort, thy : DeclaredTheory, src : SourceInfo) : Unit =
  {
    // TODO: Fix different usages

    /* enclosing sort should already be defined */
    if (tState.verbosity > 0)
    {
      println(" > Adding sort: " + subsort.toString + ", enclosed by " + supersort.toString)
    }

    val opt_ind   : Option[Term] = Some(Apply(OMS(IMPSTheory.lutinsPath ? LocalName("sort")), OMS(IMPSTheory.lutinsIndType)))
    val jdgmtname : LocalName    = LocalName(subsort.toString + "_sub_" + supersort.toString)

    val foo       : Term = tState.bindUnknowns(matchSort(subsort,thy))
    val bar       : Term = tState.bindUnknowns(matchSort(supersort,thy))
    val baz       : Term = tState.bindUnknowns(findKind(supersort))

    val subs      : Term = ApplySpine(OMS(IMPSTheory.lutinsPath ? LocalName("subsort")), baz, foo, bar)

    val jdgmttp   : Option[Term] = Some(IMPSTheory.Thm(subs))

    /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

    val typing    : Declaration  = symbols.Constant(thy.toTerm,LocalName(subsort.toString),Nil,opt_ind,None,Some("Subsort_1"))
    val judgement : Declaration  = symbols.Constant(thy.toTerm,jdgmtname,Nil,jdgmttp,None,Some("Subsort_2"))

    if (src.isDefined)
    {
      doSourceRefD(typing, src)
      doSourceRefD(judgement, src)
    }

    controller add typing
    controller add judgement
  }

  /* Translate IMPS Math Expressions to Terms */
  def doMathExp(d : IMPSMathExp, thy : DeclaredTheory, cntxt : List[(IMPSVar,IMPSSort)]) : Term =
  {
    //return OMS(IMPSTheory.lutinsPath ? "thetrue")
    d match
    {
      case IMPSWith(vrs,trgt)     => doMathExp(trgt,thy,cntxt ::: vrs)
      case IMPSVar(v)             => if (cntxt.map(_._1).contains(d)) { OMV(v) } else {
        //println(" | Switching from Var to MathSymbol: " + v + " ∉ {" + cntxt.toString() + "}")
        doMathExp(IMPSMathSymbol(v),thy,cntxt)
      }

      case IMPSMathSymbol("an%individual") => OMS(IMPSTheory.lutinsPath ? "anIndividual")
      case IMPSMathSymbol("truth")         => OMS(IMPSTheory.lutinsPath ? "thetrue")
      case IMPSMathSymbol("falsehood")     => OMS(IMPSTheory.lutinsPath ? "thefalse")
      case IMPSMathSymbol(s)               =>
      {
        if (s.forall(_.isDigit) || (s.startsWith("-") && s.tail.nonEmpty && s.tail.forall(_.isDigit))) {
          IntLiterals.parse(s)
        }
        else
        {
          var srcthy : DeclaredTheory = null

          if (tState.verbosity > 0)
          {
            println(" > Locating IMPSMathSymbol: " + s)
          } else if (tState.verbosity > 1)
          {
            println(" > Looking for IMPSMathSymbol: " + s)
          }


          for (mp <- thy.getIncludes ::: List(thy.path))
          {
            val refthy : DeclaredTheory = controller.getTheory(mp)
            val refcon : List[info.kwarc.mmt.api.symbols.Constant] = refthy.getConstants

            if (refcon.exists(c => c.name.toString.toLowerCase == s.toLowerCase)) {
              srcthy = refthy
              if (tState.verbosity > 1)
              {
                println("    > FOUND in " + refthy.name)
              }
            }
            else {
              if (tState.verbosity > 1)
              {
                println("    > Not found in " + refthy.name)
              }
            }
          }

          assert(srcthy != null)
          OMS(srcthy.path ? LocalName(s))
        }
        //  Rational Literals
        //  case "i/j" => OMLIT((BigInt(i),BigInt(j)),RatLiterals)
      }

      case IMPSIndividual()       => OMS(IMPSTheory.lutinsPath ? "anIndividual")
      case IMPSTruth()            => OMS(IMPSTheory.lutinsPath ? "thetrue")
      case IMPSFalsehood()        => OMS(IMPSTheory.lutinsPath ? "thefalse")

      case IMPSNegation(p)        => IMPSTheory.Negation(doMathExp(p,thy,cntxt))
      case IMPSIf(p,t1,t2)        => IMPSTheory.If(tState.doUnknown(), tState.doUnknown(), doMathExp(p,thy,cntxt), doMathExp(t1,thy,cntxt), doMathExp(t2,thy,cntxt))
      case IMPSIff(p, q)          => IMPSTheory.Iff(doMathExp(p,thy,cntxt), doMathExp(q,thy,cntxt))
      case IMPSIfForm(p,q,r)      => IMPSTheory.If_Form(doMathExp(p,thy,cntxt), doMathExp(q,thy,cntxt), doMathExp(r,thy,cntxt))
      case IMPSEquals(p,q)        =>
      {
        var a     : Term = null
        var alpha : Term = null
        var beta  : Term = null

        p match {
          case IMPSVar(_) => {
            if (cntxt.map(_._1).contains(p)) {
              val theSort = cntxt.find(k => k._1 == p).get._2
              alpha = matchSort(theSort,thy)
              a = findKind(theSort)
            }
          }
          case _          => ()
        }

        q match {
          case IMPSVar(_) => {
            if (cntxt.map(_._1).contains(q)) {
              val theSort = cntxt.find(k => k._1 == q).get._2
              beta = matchSort(theSort,thy)
              if (a == null) { a = findKind(theSort) }
            }
          }
          case _          => ()
        }

        if (alpha == null) { alpha = tState.doUnknown() }
        if (beta  == null) { beta  = tState.doUnknown() }
        if (a     == null) { a     = tState.doUnknown() }

        IMPSTheory.Equals(a,alpha,beta,doMathExp(p,thy,cntxt),doMathExp(q,thy,cntxt))
      }
      case IMPSDisjunction(ls)    => IMPSTheory.Or(ls map (x => doMathExp(x,thy,cntxt)))
      case IMPSConjunction(ls)    => IMPSTheory.And(ls map (x => doMathExp(x,thy,cntxt)))
      case q@IMPSLambda(vs,_)     => doIMPSLambda(curryIMPSlambda(q), thy, cntxt ::: vs)
      case q@IMPSForAll(vs,_)     => doIMPSForall(curryIMPSforall(q), thy, cntxt ::: vs)
      case q@IMPSForSome(vs,_)    => doIMPSForsome(curryIMPSforsome(q),thy, cntxt ::: vs)
      case IMPSImplication(p,q)   => IMPSTheory.Implies(doMathExp(p,thy,cntxt), doMathExp(q,thy,cntxt))
      case IMPSApply(f,ts)        =>
      {
        assert(ts.nonEmpty)
        // Wheee, manual currying!
        if (ts.length == 1)
        {
          var alpha : Term = null
          var beta  : Term = null
          var gamma : Term = null

          var a     : Term = null
          var b     : Term = null

          if (cntxt.map(_._1).contains(f))
          {
            val f_pair = cntxt.find(c => c._1 == f).get
            val cntsrt = curry(f_pair._2)
            assert(cntsrt.isInstanceOf[IMPSBinaryFunSort] || cntsrt.isInstanceOf[IMPSSort])
            cntsrt match
            {
              case IMPSBinaryFunSort(s1,s2) =>
              {
                alpha = matchSort(s1,thy)
                beta  = matchSort(s2,thy)
                a     = findKind(s1)
                b     = findKind(s2)
              }
              case IMPSSetSort(s1) =>
              {
                alpha = matchSort(s1,thy)
                beta  = matchSort(IMPSAtomSort("unitsort"),thy)
                a     = findKind(s1)
                b     = IMPSTheory.lutinsIndType()
              }
            }
          }

          if (cntxt.map(_._1).contains(ts.head))
          {
            val y_pair = cntxt.find(c => c._1 == ts.head).get
            gamma = matchSort(curry(y_pair._2),thy)
            if (a == null) { a = findKind(curry(y_pair._2)) }
          }

          if (alpha == null) { alpha = tState.doUnknown() }
          if (beta  == null) { beta  = tState.doUnknown() }
          if (gamma == null) { gamma = tState.doUnknown() }

          if (a == null) { a = tState.doUnknown() }
          if (b == null) { b = tState.doUnknown() }

          IMPSTheory.IMPSApply(a, b, alpha, gamma, beta, doMathExp(f,thy,cntxt), doMathExp(ts.head,thy,cntxt))
        }
        else
        {
          val inner = IMPSApply(f,List(ts.head))
          doMathExp(IMPSApply(inner,ts.tail),thy,cntxt)
        }
      }
      case IMPSIota(v1,s1,p)      => {
        val s       = curry(s1)
        val the_exp = doMathExp(p,thy,cntxt ::: List((v1,s)))
        val the_srt = matchSort(s,thy)
        val the_knd = findKind(s)

        val target = info.kwarc.mmt.lf.Lambda(LocalName(v1.v), doSort(s,thy), the_exp)

        IMPSTheory.Iota(the_knd,the_srt,target)
      }
      case IMPSIotaP(v1,s1,p)     => {
        val s       = curry(s1)
        val the_exp = doMathExp(p,thy,cntxt ::: List((v1,s)))
        val the_srt = matchSort(s,thy)

        val target = info.kwarc.mmt.lf.Lambda(LocalName(v1.v), doSort(s,thy), the_exp)

        IMPSTheory.IotaP(the_srt,target)
      }
      case IMPSIsDefined(r)       => IMPSTheory.IsDefined(tState.doUnknown(), tState.doUnknown(), doMathExp(r,thy,cntxt))
      case IMPSIsDefinedIn(r,s)   => IMPSTheory.IsDefinedIn(findKind(s), tState.doUnknown(), doMathExp(r,thy,cntxt), matchSort(s,thy))
      case IMPSUndefined(s)       => IMPSTheory.Undefined(findKind(s), matchSort(s,thy))

      case IMPSTotal(f,bs)         =>
      {
        var alpha_s : IMPSSort = null
        var beta_s  : IMPSSort = null

        curry(bs) match
        {
          case IMPSBinaryFunSort(s1,s2) => { alpha_s = s1 ; beta_s = s2}
          case _ => ??? // Impossible
        }

        assert(alpha_s != null && beta_s != null)

        val a     = findKind(alpha_s)
        val b     = findKind(beta_s)
        val alpha = matchSort(alpha_s,thy)
        val beta  = matchSort(beta_s,thy)
        val func  = doMathExp(f,thy,cntxt)

        IMPSTheory.Total(a,b,alpha,beta,func)
      }

      case IMPSNonVacuous(p) => {
        if (cntxt.map(_._1).contains(p)) {
          val foo = cntxt.find(k => k._1 == p).get
          foo._2 match {
            case IMPSBinaryFunSort(s1,s2) => {
              val the_exp = doMathExp(p,thy,cntxt)
              val the_srt = matchSort(s1,thy)
              assert(s2 == IMPSAtomSort("bool") || s2 == IMPSAtomSort("prop"))
              IMPSTheory.Nonvacuous(findKind(s1),the_srt,the_exp)
            }
            case _ => ???
          }
        } else { IMPSTheory.Nonvacuous(tState.doUnknown(),tState.doUnknown(),doMathExp(p,thy,cntxt)) }
      }
      case IMPSQuasiEquals(p,q) => {

        var a     : Term = null
        var alpha : Term = null
        var beta  : Term = null

        val e1 = doMathExp(p,thy,cntxt)
        val e2 = doMathExp(q,thy,cntxt)

        if (cntxt.map(_._1).contains(p)) {
          val foo = cntxt.find(k => k._1 == p).get
          alpha   = matchSort(foo._2,thy)
          a       = findKind(foo._2)
        }

        if (cntxt.map(_._1).contains(q)) {
          val bar = cntxt.find(k => k._1 == q).get
          beta    = matchSort(bar._2,thy)
          if (a == null) { a = findKind(bar._2) } else { assert(a  == findKind(bar._2)) }
        }

        if (a     == null) { a     = tState.doUnknown() }
        if (beta  == null) { beta  = tState.doUnknown() }
        if (alpha == null) { alpha = tState.doUnknown() }

        IMPSTheory.Quasiequals(a, alpha, beta, e1, e2)
      }

      case IMPSQCPred2Indicator(m) => {
        val ca = findSortFromContext(m, cntxt)
        val as: IMPSSort = ca.getOrElse(IMPSUnknownSort(tState.freshHash()))
        val t1 = doMathExp(m, thy, cntxt)

        IMPSTheory.QCT.pred2indicQC(findKind(as), matchSort(as, thy), t1)
      }

      case IMPSQCSort2Indicator(srt) => {
        val ca = findSortFromContext(srt,cntxt)
        val as : IMPSSort = ca.getOrElse(IMPSUnknownSort(tState.freshHash()))

        IMPSTheory.QCT.sort2indicQC(findKind(as),matchSort(as,thy))
      }

      case IMPSQCIn(e1,e2) => {
        val ca = findSortFromContext(e1,cntxt) ; val cb = findSortFromContext(e2,cntxt)
        val as : IMPSSort = ca.getOrElse(cb.getOrElse(IMPSUnknownSort(tState.freshHash())))
        val t1 = doMathExp(e1,thy,cntxt) ; val t2 = doMathExp(e2,thy,cntxt)

        IMPSTheory.QCT.inQC(findKind(as),matchSort(as,thy),t1,t2)
      }

      case IMPSQCSubsetEQ(e1,e2) => {
        val ca = findSortFromContext(e1,cntxt) ; val cb = findSortFromContext(e2,cntxt)
        val as : IMPSSort = ca.getOrElse(cb.getOrElse(IMPSUnknownSort(tState.freshHash())))
        val t1 = doMathExp(e1,thy,cntxt) ; val t2 = doMathExp(e2,thy,cntxt)

        IMPSTheory.QCT.subseteqQC(findKind(as), matchSort(as,thy), t1, t2)
      }

      case IMPSQCSubset(e1,e2) => {
        val ca = findSortFromContext(e1,cntxt) ; val cb = findSortFromContext(e2,cntxt)
        val as : IMPSSort = ca.getOrElse(cb.getOrElse(IMPSUnknownSort(tState.freshHash())))
        val t1 = doMathExp(e1,thy,cntxt) ; val t2 = doMathExp(e2,thy,cntxt)

        IMPSTheory.QCT.subsetQC(findKind(as), matchSort(as,thy), t1, t2)
      }

      case IMPSQCEmptyIndicator(srt) => {
        val ca = findSortFromContext(srt,cntxt)
        val as : IMPSSort = ca.getOrElse(IMPSUnknownSort(tState.freshHash()))

        IMPSTheory.QCT.emptyIndicQC(findKind(as),matchSort(as,thy))
      }

      case IMPSQCEmptyIndicatorQ(srt) => {
        val ca = findSortFromContext(srt,cntxt)
        val as : IMPSSort = ca.getOrElse(IMPSUnknownSort(tState.freshHash()))
        val t1 = doMathExp(srt,thy,cntxt)

        IMPSTheory.QCT.emptyIndicQQC(findKind(as),matchSort(as,thy),t1)
      }

      case IMPSQCNonemptyIndicator(srt) => {
        val ca = findSortFromContext(srt,cntxt)
        val as : IMPSSort = ca.getOrElse(IMPSUnknownSort(tState.freshHash()))
        val t1 = doMathExp(srt,thy,cntxt)

        IMPSTheory.QCT.nonEmptyIndicQQC(findKind(as),matchSort(as,thy),t1)
      }

      case IMPSQCComplement(m) => {
        val ca = findSortFromContext(m,cntxt)
        val as : IMPSSort = ca.getOrElse(IMPSUnknownSort(tState.freshHash()))
        val t1 = doMathExp(m,thy,cntxt)

        IMPSTheory.QCT.complementQC(findKind(as),matchSort(as,thy),t1)
      }

      case IMPSQCUnion(e1,e2) => {
        val ca = findSortFromContext(e1,cntxt) ; val cb = findSortFromContext(e2,cntxt)
        val as : IMPSSort = ca.getOrElse(cb.getOrElse(IMPSUnknownSort(tState.freshHash())))
        val t1 = doMathExp(e1,thy,cntxt) ; val t2 = doMathExp(e2,thy,cntxt)

        IMPSTheory.QCT.unionQC(findKind(as), matchSort(as,thy), t1, t2)
      }

      case IMPSQCIntersection(e1,e2) => {
        val ca = findSortFromContext(e1,cntxt) ; val cb = findSortFromContext(e2,cntxt)
        val as : IMPSSort = ca.getOrElse(cb.getOrElse(IMPSUnknownSort(tState.freshHash())))
        val t1 = doMathExp(e1,thy,cntxt) ; val t2 = doMathExp(e2,thy,cntxt)

        IMPSTheory.QCT.intersectionQC(findKind(as), matchSort(as,thy), t1, t2)
      }

      case IMPSQCDifference(e1,e2) => {
        val ca = findSortFromContext(e1,cntxt) ; val cb = findSortFromContext(e2,cntxt)
        val as : IMPSSort = ca.getOrElse(cb.getOrElse(IMPSUnknownSort(tState.freshHash())))
        val t1 = doMathExp(e1,thy,cntxt) ; val t2 = doMathExp(e2,thy,cntxt)

        IMPSTheory.QCT.differenceQC(findKind(as), matchSort(as,thy), t1, t2)
      }

      case IMPSQCSymDifference(e1,e2) => {
        val ca = findSortFromContext(e1,cntxt) ; val cb = findSortFromContext(e2,cntxt)
        val as : IMPSSort = ca.getOrElse(cb.getOrElse(IMPSUnknownSort(tState.freshHash())))
        val t1 = doMathExp(e1,thy,cntxt) ; val t2 = doMathExp(e2,thy,cntxt)

        IMPSTheory.QCT.symDifferenceQC(findKind(as), matchSort(as,thy), t1, t2)
      }

      case IMPSQCDisjoint(e1,e2) => {
        val ca = findSortFromContext(e1,cntxt) ; val cb = findSortFromContext(e2,cntxt)
        val as : IMPSSort = ca.getOrElse(cb.getOrElse(IMPSUnknownSort(tState.freshHash())))
        val t1 = doMathExp(e1,thy,cntxt) ; val t2 = doMathExp(e2,thy,cntxt)

        IMPSTheory.QCT.disjointQC(findKind(as), matchSort(as,thy), t1, t2)
      }

      case IMPSQCPartitionQ(e1,e2) => {
        val ca = findSortFromContext(e1,cntxt) ; val cb = findSortFromContext(e2,cntxt)
        val as : IMPSSort = ca.getOrElse(cb.getOrElse(IMPSUnknownSort(tState.freshHash())))
        val t1 = doMathExp(e1,thy,cntxt) ; val t2 = doMathExp(e2,thy,cntxt)

        IMPSTheory.QCT.partitionQQC(findKind(as), matchSort(as,thy), t1, t2)
      }

      case IMPSQCSingleton(m) => {
        val ca = findSortFromContext(m, cntxt)
        val as: IMPSSort = ca.getOrElse(IMPSUnknownSort(tState.freshHash()))
        val t1 = doMathExp(m, thy, cntxt)

        IMPSTheory.QCT.singletonQC(findKind(as), matchSort(as, thy), t1)
      }

      case IMPSQCBigUnion(f) =>
      {
        val fp : Term = doMathExp(f,thy,cntxt)
        IMPSTheory.QCT.bigUnionQC(tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),fp)
      }

      case IMPSQCBigIntersection(f) =>
      {
        val fp : Term = doMathExp(f,thy,cntxt)
        IMPSTheory.QCT.bigIntersectionQC(tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),fp)
      }

      case IMPSQCMDomain(f) =>
      {
        val fp : Term = doMathExp(f,thy,cntxt)
        IMPSTheory.QCT.mdomainQC(tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),fp)
      }

      case IMPSQCMRange(f) =>
      {
        val fp : Term = doMathExp(f,thy,cntxt)
        IMPSTheory.QCT.mrangeQC(tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),fp)
      }

      case IMPSQCMComposition(f,g) =>
      {
        val fp : Term = doMathExp(f,thy,cntxt)
        val gp : Term = doMathExp(g,thy,cntxt)
        IMPSTheory.QCT.mcompositionQC(tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),fp,gp)
      }

      case IMPSQCMImage(f,s) =>
      {
        val fp : Term = doMathExp(f,thy,cntxt)
        val sp : Term = doMathExp(s,thy,cntxt)
        IMPSTheory.QCT.mimageQC(tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),fp,sp)
      }

      case IMPSQCMInverseImage(f,s) =>
      {
        val fp : Term = doMathExp(f,thy,cntxt)
        val sp : Term = doMathExp(s,thy,cntxt)
        IMPSTheory.QCT.minverseimageQC(tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),fp,sp)
      }

      case IMPSQCMInverse(f) =>
      {
        val fp : Term = doMathExp(f,thy,cntxt)
        IMPSTheory.QCT.minverseQC(tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),fp)
      }

      case IMPSQCMId(s) =>
      {
        val sp : Term = doMathExp(s,thy,cntxt)
        IMPSTheory.QCT.midQC(tState.doUnknown(), tState.doUnknown(), sp)
      }

      case IMPSQCMRestrict(f,s) =>
      {
        val fp : Term = doMathExp(f,thy,cntxt)
        val sp : Term = doMathExp(s,thy,cntxt)
        IMPSTheory.QCT.mrestrictQC(tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),fp,sp)
      }

      case IMPSQCMRestrict2(f,s,t) =>
      {
        val fp : Term = doMathExp(f,thy,cntxt)
        val sp : Term = doMathExp(s,thy,cntxt)
        val tp : Term = doMathExp(t,thy,cntxt)
        IMPSTheory.QCT.mrestrict2QC(tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),fp,sp,tp)
      }

      case IMPSQCMSurjective(f) =>
      {
        val fp : Term = doMathExp(f,thy,cntxt)
        IMPSTheory.QCT.msurjectiveQQC(tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),fp)
      }

      case IMPSQCMInjective(f) =>
      {
        val fp : Term = doMathExp(f,thy,cntxt)
        IMPSTheory.QCT.minjectiveQQC(tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),fp)
      }

      case IMPSQCMBijective(f) =>
      {
        val fp : Term = doMathExp(f,thy,cntxt)
        IMPSTheory.QCT.mbijectiveQQC(tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),fp)
      }

      case IMPSQCMSurjectiveOn(f,as,bs) =>
      {
        {
          val fp  : Term = doMathExp(f,thy,cntxt)
          val asp : Term = doMathExp(as,thy,cntxt)
          val bsp : Term = doMathExp(bs,thy,cntxt)

          IMPSTheory.QCT.msurjectiveonQQC(tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),fp,asp,bsp)
        }
      }

      case IMPSQCMInjectiveOn(f,as) =>
      {
        {
          val fp  : Term = doMathExp(f,thy,cntxt)
          val asp : Term = doMathExp(as,thy,cntxt)

          IMPSTheory.QCT.minjectiveonQQC(tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),fp,asp)
        }
      }

      case IMPSQCMBijectiveOn(f,as,bs) =>
      {
        {
          val fp  : Term = doMathExp(f,thy,cntxt)
          val asp : Term = doMathExp(as,thy,cntxt)
          val bsp : Term = doMathExp(bs,thy,cntxt)

          IMPSTheory.QCT.mbijectiveonQQC(tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),fp,asp,bsp)
        }
      }

      case _ => { println(d) ; ??? }
    }
  }

  def findSortFromContext(exp : IMPSMathExp, context : List[(IMPSVar,IMPSSort)]) : Option[IMPSSort] =
  {
    exp match {
      case IMPSUndefined(s) => Some(s)
      case IMPSNegation(_)
         | IMPSTruth()
         | IMPSFalsehood()
         | IMPSForAll(_,_)
         | IMPSForSome(_,_)
         | IMPSConjunction(_)
         | IMPSDisjunction(_)
         | IMPSImplication(_,_)
         | IMPSIff(_,_)
         | IMPSIsDefined(_)
         | IMPSIsDefinedIn(_,_)
         | IMPSIfForm(_,_,_) => Some(IMPSAtomSort("prop"))
      case _ => {
        if (context.contains(exp)) { Some(context.find(k => k._1 == exp).get._2) } else { None }
      }
    }
  }

  def curryIMPSlambda(l : IMPSLambda) : IMPSLambda =
  {
    assert(l.vs.nonEmpty)
    if (l.vs.length == 1) { l }
    else {
      IMPSLambda(List(l.vs.head),curryIMPSlambda(IMPSLambda(l.vs.tail,l.t)))
    }
  }

  def doIMPSLambda(lambda : IMPSLambda, thy : DeclaredTheory, cntxt : List[(IMPSVar,IMPSSort)]) : Term =
  {
    // Always call curried
    assert(lambda.vs.length == 1)

    var alpha : Term = null
    var beta  : Term = null

    var a     : Term = null
    var b     : Term = null

    val thisVar     : LocalName = LocalName(lambda.vs.head._1.v)
    val thisSrt     : IMPSSort  = curry(lambda.vs.head._2)
    val expSortTerm : Term      = doSort(thisSrt,thy)     // <-- this is "exp whateversort"
    val target      : Term      = doMathExp(lambda.t,thy,cntxt)
    val body        : Term      = info.kwarc.mmt.lf.Lambda(thisVar, expSortTerm, target)

    a     = findKind(thisSrt)
    alpha = matchSort(thisSrt,thy)

    b     = tState.doUnknown()
    beta  = tState.doUnknown()

    IMPSTheory.Lambda(a, b, alpha, beta, body)
  }

  def curryIMPSforsome(f : IMPSForSome) : IMPSForSome =
  {
    assert(f.vs.nonEmpty)
    if (f.vs.length == 1) { f }
    else {
      IMPSForSome(List(f.vs.head),curryIMPSforsome(IMPSForSome(f.vs.tail,f.p)))
    }
  }

  def doIMPSForsome(forsome : IMPSForSome, thy : DeclaredTheory, cntxt : List[(IMPSVar,IMPSSort)]) : Term =
  {
    // Always call curried
    assert(forsome.vs.length == 1)

    val thisVar     : LocalName = LocalName(forsome.vs.head._1.v)
    val thisSrt     : IMPSSort  = curry(forsome.vs.head._2)
    val expSortTerm : Term      = doSort(thisSrt,thy)     // <-- this is "exp whateversort"
    val target      : Term      = doMathExp(forsome.p,thy,cntxt)
    val body        : Term      = info.kwarc.mmt.lf.Lambda(thisVar, expSortTerm, target)

    val jstSortTerm : Term      = matchSort(thisSrt,thy)
    //                                ^-------v-------------------These are just the sort
    IMPSTheory.Forsome(findKind(thisSrt), jstSortTerm, body)
  }

  def curryIMPSforall(f : IMPSForAll) : IMPSForAll =
  {
    assert(f.vs.nonEmpty)
    if (f.vs.length == 1) { f }
    else {
      IMPSForAll(List(f.vs.head),curryIMPSforall(IMPSForAll(f.vs.tail,f.p)))
    }
  }

  def doIMPSForall(forall : IMPSForAll, thy : DeclaredTheory, cntxt : List[(IMPSVar,IMPSSort)]) : Term =
  {
    // Always call curried
    assert(forall.vs.length == 1)

    val thisVar     : LocalName = LocalName(forall.vs.head._1.v)
    val thisSrt     : IMPSSort  = curry(forall.vs.head._2)
    val expSortTerm : Term      = doSort(thisSrt,thy)     // <-- this is "exp whateversort"
    val target      : Term      = doMathExp(forall.p,thy,cntxt)
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

