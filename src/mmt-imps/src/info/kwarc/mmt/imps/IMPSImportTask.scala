package info.kwarc.mmt.imps

import info.kwarc.mmt.api
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.checking.{History, Solver, SubtypingRule, TypeBasedEqualityRule}
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.{LocalName, _}
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.metadata.MetaDatum
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.opaque.{OpaqueText, StringFragment}
import info.kwarc.mmt.api.parser.{SourcePosition, SourceRef, SourceRegion}
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.imps.Usage.Usage
import info.kwarc.mmt.imps.impsMathParser.SymbolicExpressionParser
import info.kwarc.mmt.lf.ApplySpine
import utils._

object IMPSImportTask{
  val rootdpath : DPath = DPath(URI.http colon "imps.mcmaster.ca") /* arbitrary, but seemed fitting */
  val docpath   : DPath = rootdpath / "impsMath"
}

class IMPSImportTask(val controller  : Controller,
                         bt          : BuildTask,
                         tState      : TranslationState,
                         toplevelDoc : Document,
                         index       : Document => Unit) extends Logger with MMTTask
{
	          def logPrefix : String = "imps-omdoc"
	protected def report    : Report = controller.report


  /* Source References. */
  def doSourceRefT(t : Term,        s : SourceInfo, uri : URI) : Unit = if (s.isDefined) s.get match {
    case l@scala.util.Left(((a,b,c),(x,y,z))) => SourceRef.update(t, SourceRef(uri,SourceRegion(SourcePosition(a,b,c),SourcePosition(x,y,z))))
    case r@scala.util.Right(src)              => SourceRef.update(t, src)
  }

  def doSourceRefD(d : Declaration, s : SourceInfo, uri : URI) : Unit = if (s.isDefined) s.get match {
    case l@scala.util.Left(((a,b,c),(x,y,z))) => SourceRef.update(d, SourceRef(uri,SourceRegion(SourcePosition(a,b,c),SourcePosition(x,y,z))))
    case r@scala.util.Right(src)              => SourceRef.update(d, src)
  }

  /* Add metadata from usages element */
  def doUsages(d : Declaration, usages : List[Usage]) : Unit =
  {
    for (usage <- usages)
    {
      // using rootdpath and not IMPSTheory.rootdpath because this is IMPS, not LUTINS
      val metadata_verb   : GlobalName = IMPSImportTask.rootdpath ? d.name ? LocalName("usage")
      val metadata_object : Obj        = OMS(IMPSImportTask.rootdpath ? d.name ? usage.toString)
      d.metadata.add(new MetaDatum(metadata_verb, metadata_object))
    }
  }

  /* Add more generic metadata, meant to be used for short strings, not proofs
   * Might be rewritten, when we have cleverer solutions for MetaData */
  def doMetaData(d : Declaration, metaVerb : String, metaObject : String) : Unit =
  {
    val mv : GlobalName =     IMPSImportTask.rootdpath ? d.name ? LocalName(metaVerb)
    val mo : Obj        = OMS(IMPSImportTask.rootdpath ? d.name ? metaObject)

    d.metadata.add(new MetaDatum(mv,mo))
  }

  def doName(s : String) : LocalName = LocalName(s)

	def doDocument(es : List[DefForm], uri : URI) : BuildResult =
	{
    val doc = new Document(DPath((IMPSImportTask.rootdpath / "impsMath" / uri.path.last).uri.setExtension("omdoc")), SectionLevel)
    controller add doc
    controller add DRef(toplevelDoc.path,doc.path)

    var excps : List[Exception] = Nil

    for (exp <- es)
    {
      exp match
      {
        /* Translating Theories to MMT */
        case t@DFTheory(_,_,_,_,_,_,_) => try
        {
          if (!tState.theories_raw.contains(t)) { doTheory(t, doc.path, bt.narrationDPath, uri) }
        } catch {
          case e : IMPSDependencyException => println(" > ... fail. Add to stack: " +  e.getMessage ) ; excps = excps.::(e)
        }
        // Languages are processed in context of theories using them, not by themselves
        case l@DFLanguage(_,_,_,_,_,_,_,_,_)              => doLanguage(l, doc.path, bt.narrationDPath, uri)
        case t@DFTranslation(_,_,_,_,_,_,_,_,_,_,_,_,_,_) => doTranslation(t, doc.path, uri)
        case DFTheoryEnsembleMultiple(name,integ,src,cmt) =>

          if (tState.verbosity > 0) {
            println(" > adding theory-multiple " + name)
          }

          val ensemble : TheoryEnsemble          = tState.ensembles.find(e => e.name.toLowerCase == name.s.toLowerCase).get
          val base     : Theory          = ensemble.baseTheory
          val renaming : Int => String => String = ensemble.replicaRenamer

          val n = integ.n
          for (i <- 1 to n)
          {
            if (tState.verbosity > 1) {
              println("   > adding replica number " + i.toString)
            }

            // Create replica if necessary
            if (!ensemble.replicaMap.contains(i)) {
              val replica = makeReplica(base, doc.path, bt.narrationDPath, renaming(i-1))
              ensemble.replicaMap = ensemble.replicaMap + (i -> replica)
            }
          }

          for (j <- 1 to n)
          {
            if (!ensemble.multipleMap.contains(j))
            {
              if (tState.verbosity > 1) {
                println("   > adding multiple number " + j.toString)
              }

              val ln : LocalName = LocalName(ensemble.baseTheory.name.toString + "-" + j.toString + "-TUPLES")

              val nu_multiple = new Theory(bt.narrationDPath,
                ln,
                Some(IMPSTheory.QCT.quasiLutinsPath),
                modules.Theory.noParams,
                modules.Theory.noBase)

              val mref : MRef = MRef(doc.path,nu_multiple.path)
              controller.add(nu_multiple)
              controller.add(mref)

              // Include all includes
              val includes : List[Theory] = recursiveIncludes(List(base)).filter(t => t != base)
              for (incl <- includes) {
                val includee : Theory = incl
                val includer : Theory = nu_multiple
                val theInclude = PlainInclude(includee.path,includer.path)
                controller add theInclude
                controller endAdd theInclude
              }

              // include lower multiple, if applicable
              if (j != 1) {
                val theInclude = PlainInclude(ensemble.multipleMap(j-1).path,nu_multiple.path)
                controller add theInclude
                controller endAdd theInclude
              }

              assert(ensemble.replicaMap.contains(j))
              val theReplica : Theory = ensemble.replicaMap(j)
              val theInclude = PlainInclude(theReplica.path,nu_multiple.path)
              controller add theInclude
              controller endAdd theInclude

              // register new Theory Multiple
              ensemble.multipleMap = ensemble.multipleMap + (j -> nu_multiple)
              tState.theories_decl = nu_multiple :: tState.theories_decl
            }
          }
        case DFTheoryEnsembleInstances(name, _, targetThys, targetMuls, ensembleSorts, ensembleConsts, multiples, _, perms, renamings, src, cmt) =>

          if (tState.verbosity > 0) {
            println(" > adding theory-ensemble-instances: " + name)
          }

          val ensemble : TheoryEnsemble = tState.ensembles.find(te => te.name.toLowerCase == name.s.toLowerCase).get

          val candidates : List[Theory] = if (targetThys.isDefined) {
            targetThys.get.ns.map(nm => getTheory(nm.toString))
          } else if (targetMuls.isDefined) {
            val range = 1 to targetMuls.get.n.n
            range.map(k => ensemble.replicaMap(k)).toList
          } else ??? // should not happen, either argument is always present

          var permutations : List[List[Int]] = List.empty

          if (perms.isDefined) {
            permutations = perms.get.nns.map(ls => ls.map(_.n))
          } else if (multiples.isDefined) {
            for (m <- multiples.get.ns) {
              permutations = candidates.indices.toList.permutations.toList.filter(l => l.length == m.n)
            }
          } else ??? // should not happen, either argument is always present

          def tUnion : (Theory, Theory) => Theory = (t1, t2) =>
          {
            if      (recursiveIncludes(List(t1)).contains(t2)) t1
            else if (recursiveIncludes(List(t2)).contains(t1)) t2
            else {
              ??!("!!! Actual theory union between " + t1.name + " and " + t2.name)
              val union = new Theory(bt.narrationDPath,
                LocalName(t1.name.toString + "_union_" + t2.name.toString),
                Some(IMPSTheory.QCT.quasiLutinsPath),
                modules.Theory.noParams,
                modules.Theory.noBase)

              controller add union
              controller add MRef(doc.path,union.path)
              controller add PlainInclude(t1.path,union.path)
              controller add PlainInclude(t2.path,union.path)
              ??!("these still need endAdds")
              union
            }
          }

          val allJSONTranslations : List[JSONObject] = tState.jsons.flatMap(j => j.getAsList(classOf[JSONObject],"translations"))

          for (permutation <- permutations) {
            val targets: List[Theory] = permutation.map(p => candidates(p))
            val target: Theory = targets.tail.foldRight(targets.head)(tUnion)
            val source: Theory = ensemble.multipleMap(permutation.last + 1)
            val fooname: String = source.name.toString + "-TO-" + target.name.toString
            val vname: LocalName = LocalName(fooname.toUpperCase)

            val relevantTranslations: List[JSONObject] = allJSONTranslations.filter(j => j.getAsString("name").toLowerCase == fooname.toLowerCase)
            //println(" > [TES] " + relevantTranslations.length + " relevant translation(s)!")
            if (relevantTranslations.length == 1)
            {
              if (tState.verbosity > 1) {
                println(" > adding view " + vname + " (source: " + source.name.toString + ", target: " + target.name.toString + ")")
              }

              val theRelevantTranslation : JSONObject = relevantTranslations.head
              val sep : SymbolicExpressionParser = new SymbolicExpressionParser

              val nu_view = View(bt.narrationDPath, vname, source.toTerm, target.toTerm, isImplicit = false)
              val mref : MRef = MRef(doc.path,nu_view.path)
              controller add nu_view
              controller add mref

              tState.translations_decl = nu_view :: tState.translations_decl

              val fixed = theRelevantTranslation.getAsList(classOf[JSONString],"fixed-theories")
              for (ft <- fixed)
              {
                if (tState.verbosity > 1) { println("  > view " + vname + " fixes theory " + ft.value) }

                val fix    : Theory = getTheory(ft.value.toLowerCase)
                val cn     : LocalName = LocalName(ComplexStep(fix.path))
                val id_fix : Structure = Structure(nu_view.toTerm,cn,fix.toTerm, Some(OMIDENT(fix.toTerm)), isImplicit = false) // get it? :D
                controller add id_fix
              }

              if (ensembleSorts.isDefined) {
                assert(targetThys.isDefined)
                assert(targetMuls.isEmpty)

                println(theRelevantTranslation)

                /* Translate all explicity listed sort pairs */

                val sortpairsexps : List[JSONString] = theRelevantTranslation.getAsList(classOf[JSONString],"sort-pairs-sexp")
                assert(sortpairsexps.nonEmpty)

                for (smi <- sortpairsexps.indices)
                {
                  val leftExpStr  : String = sortpairsexps(smi).value.takeWhile(c => !c.isWhitespace).tail.trim.toLowerCase
                  val rightExpStr : String = sortpairsexps(smi).value.dropWhile(c => !c.isWhitespace).trim.init.toLowerCase

                  val sourceName  : String = leftExpStr
                  val sourceSort  : Term   = getConstant(sourceName,ensemble.baseTheory).toTerm

                  val trgt : IMPSSort = IMPSAtomSort(rightExpStr)

                  val target_term : Term = doSort(trgt,target)
                  val quelle      : Option[Theory] = locateMathSymbolHome(sourceName, source)
                  assert(quelle.isDefined)

                  val nu_sort_map = symbols.Constant(nu_view.toTerm,ComplexStep(quelle.get.path) / doName(sourceName),Nil,None,Some(target_term),None)
                  controller add nu_sort_map

                  if (tState.verbosity > 1) {
                    println(" > adding ensemble-instance sort-mapping: " + leftExpStr + " → " + trgt.toString)
                  }
                }

                /*
                for (sortMapping <- ensembleSorts.get.sorts)
                {
                  val sourceName : String = sortMapping.nm.s
                  val sourceSort : Term   = matchSort(IMPSAtomSort(sourceName),locateMathSymbolHome(sourceName,ensemble.baseTheory).get)

                  for (targetSort : ODefString <- sortMapping.sorts)
                  {
                    var trgt : Either[IMPSSort,IMPSMathExp] = targetSort.o match
                    {
                      case scala.util.Right(srt_name) => scala.util.Left(IMPSAtomSort(srt_name.toString))
                      case scala.util.Left((dfs,ime)) => assert(ime.isDefined) ; scala.util.Right(ime.get)
                    }

                    val target_term : Term = trgt match {
                      case scala.util.Left(is)  => val q = locateMathSymbolHome(is.toString,target) ; assert(q.isDefined) ; matchSort(is,q.get)
                      case scala.util.Right(im) => doMathExp(im,target,Nil)
                    }

                    val target_tp : Option[Term] = trgt match {
                      case scala.util.Left(is) => Some(IMPSTheory.Sort(OMS(IMPSTheory.lutinsIndType)))
                      case scala.util.Right(_) => None
                    }

                    val quelle : Option[Theory] = locateMathSymbolHome(sourceName, source)
                    assert(quelle.isDefined)

                    val nu_sort_map = symbols.Constant(nu_view.toTerm,ComplexStep(quelle.get.path) / doName(sourceName),Nil,target_tp,Some(target_term),None)
                    if (tState.verbosity > 1)
                    {
                      val disp : String = targetSort.o match {
                        case scala.util.Left((df,_)) => df.toString
                        case scala.util.Right(xname) => xname.toString
                      }
                      println(" > adding ensemble-instance sort-mapping: " + sourceName + " → " + disp)
                    }

                    controller add nu_sort_map
                  }
                } */
              }

              if (ensembleConsts.isDefined) {
                assert(targetThys.isDefined)
                assert(targetMuls.isEmpty)

                // Define special constant renamer
                def renamer(in : String) : String = {
                  var renamed = in
                  if (renamings.isDefined) {
                    for (re <- renamings.get.ns) {
                      if (in.toLowerCase == re.old.toString.toLowerCase) { renamed = re.nu.toString ; println("actually renamed " + in + " into " + re.nu.toString) }
                    }
                  }
                  renamed
                }

                val constpairsexps : List[JSONString] = theRelevantTranslation.getAsList(classOf[JSONString],"constant-pairs-sexp")
                assert(constpairsexps.nonEmpty)

                for (cmi <- constpairsexps.indices)
                {
                  val leftExpStr  : String = constpairsexps(cmi).value.takeWhile(c => !c.isWhitespace).tail.trim.toLowerCase
                  val rightExpStr : String = constpairsexps(cmi).value.dropWhile(c => !c.isWhitespace).trim.init.toLowerCase

                  val sourceName   : String = leftExpStr
                  val sourceConst  : Term   = getConstant(sourceName,ensemble.baseTheory).toTerm

                  val sexpParser   : SymbolicExpressionParser = new SymbolicExpressionParser
                  val sexp = sexpParser.parseAll(sexpParser.parseSEXP,rightExpStr)
                  assert(sexp.successful)

                  val trgt : IMPSMathExp = impsMathParser.makeSEXPFormula(sexp.get)

                  val target_term : Term = doMathExp(trgt,target,Nil)
                  val target_tp   : Option[Term] = None
                  val quelle      : Option[Theory] = locateMathSymbolHome(sourceName, source)
                  assert(quelle.isDefined)

                  val nu_const_map = symbols.Constant(nu_view.toTerm,ComplexStep(quelle.get.path) / doName(renamer(sourceName)),List(doName(sourceName)),target_tp,Some(target_term),None)
                  controller add nu_const_map

                  if (tState.verbosity > 1) {
                    println(" > adding ensemble-instance const-mapping: " + leftExpStr + " → " + trgt.toString)
                  }
                }
              }

              // Translate natively defined constants here (see manual pg. 109)
              for (nativec <- source.getConstants)
              {
                println(" > translating native constant " + nativec.name + "(from " + source.name + ") along " + nu_view.name)
                println(" > con: " + nativec.name)
                println(" > tp:  " + nativec.tp)
                println(" > df:  " + nativec.df)
                val image : Term = controller.library.ApplyMorphs(nativec.toTerm,nu_view.toTerm)
                val nuname : LocalName = ComplexStep(target.path) / nativec.name
                val nu_trans_native = symbols.Constant(target.toTerm,nuname,Nil,None,Some(image),Some("translated native constant"))
                controller add nu_trans_native
                println(" > " + nu_trans_native)

              }

              if (renamings.isDefined)
              {
                for (re <- renamings.get.ns) {
                  //val q : Theory  = locateMathSymbolHome(re.old.toString,source).get
                  val locname : LocalName = api.ComplexStep(target.path) / LocalName(re.nu.toString)
                  val c       : Constant  = getConstant(re.old.toString,target)

                  val nu_constant = symbols.Constant(target.toTerm,locname,List.empty,c.tp,Some(c.toTerm),Some("Special Renaming"))
                  controller add nu_constant

                  if (tState.verbosity > 1) {
                    println(" > adding special renaming: " + re.nu.toString + " for " + c.path + " in  " + target.name + " // " + nu_constant)
                  }
                }
              }

              controller endAdd nu_view

            } else { println("  > [TES] No unique applicable translation in source for " + fooname + ", skipping!") }

          }

        // If it's none of these, fall back to doDeclaration
        case _                             => doDeclaration(exp,uri)
      }
    }

    if (excps.nonEmpty) { throw excps.head }

    index(doc)
    BuildSuccess(Nil,Nil)
	}

  def doTheory (t : DFTheory, docPath: DPath, ns:DPath, uri : URI) : Unit =
  {
    val nu_theory = new Theory(ns,
                                       LocalName(t.name.toString),
                                       Some(IMPSTheory.QCT.quasiLutinsPath),
                                       modules.Theory.noParams,
                                       modules.Theory.noBase)

    if (tState.verbosity > 0)
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

    if (t.name.s != "the-kernel-theory")
    {
      if (tState.verbosity > 0) {
        println("   > adding include for kernel theory")
      }

      val kernel : Theory = getTheory( name = "the-kernel-theory")
      val theInclude = PlainInclude.apply(kernel.path,nu_theory.path)
      controller add theInclude
      controller endAdd theInclude
    }

    /* Translate language of the theory */
    var l : Option[DFLanguage] = None

    // Build correct union of languages
    if (t.lang.isDefined) {
      val fnd : Option[Theory] = tState.languages_decl.find(th => th.name.toString.toLowerCase == t.lang.get.lang.toString.toLowerCase)
      hAssert(fnd.nonEmpty, tState.languages_decl)

      val includee : Theory = fnd.get
      val includer : Theory = nu_theory
      val theInclude = PlainInclude(includee.path,includer.path)
      controller add theInclude
      controller endAdd theInclude

      if (tState.verbosity > 0) {
        println("   > adding language include: " + nu_theory.name + " includes " + fnd.get.name)
      }
    }

    if (t.comp.isDefined)
    {
      /* Include all component theories */
      for (comp_theory <- t.comp.get.cps)
      {
        hAssert(tState.theories_raw.exists(t => t.name.toString.toLowerCase == comp_theory.toString.toLowerCase),tState.theories_raw)
        val component : Theory = getTheory(comp_theory.toString.toLowerCase)

        /* Add Include */
        if (tState.verbosity > 0) {
          println("   > adding theory include: " + nu_theory.name + " includes " + component.name.toString)
        }
        val theInclude = PlainInclude(component.path,nu_theory.path)
        controller add theInclude
        controller endAdd theInclude
      }
    }

    /* Translate all axioms, if there are any */
    if (t.axioms.isDefined)
    {
      var axiom_count : Int = -1
      for (ax <- t.axioms.get.cps)
      {
        val mth : Term = tState.bindUnknowns(IMPSTheory.Thm(doMathExp(ax.frm.get, nu_theory, Nil)))
        val name : String = if (ax.name.isDefined) { ax.name.get } else { axiom_count += 1 ; t.name + "_unnamed_axiom" + axiom_count.toString }

        val assumption : Declaration = symbols.Constant(nu_theory.toTerm,doName(name),Nil,Some(mth),None,Some("Assumption"))

        if (ax.usgs.isDefined) { doUsages(assumption,ax.usgs.get) }
        if (ax.src.isDefined) { doSourceRefD(assumption,ax.src,uri) }
        controller.add(assumption)

        if (tState.verbosity > 2) {
          println("   > adding axiom " + assumption.name + " (" + ax.defstr.toString + ") to theory " + nu_theory.name)
        }
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
            if (t.dstnct.get.src.isDefined) { doSourceRefD(dist_assumption, t.dstnct.get.src,uri) }
            controller.add(dist_assumption)
          }
        }
      }
    }

    controller endAdd nu_theory

    tState.theories_decl = tState.theories_decl :+ nu_theory
    tState.theories_raw  = tState.theories_raw  :+ t

  }

  def doLanguage(l : DFLanguage, docPath : DPath, ns : DPath, uri : URI) : Unit =
  {
    val nu_lang = new Theory(ns,
      LocalName(l.name.toString),
      Some(IMPSTheory.QCT.quasiLutinsPath),
      modules.Theory.noParams,
      modules.Theory.noBase)

    val mref : MRef = MRef(docPath,nu_lang.path)
    controller.add(nu_lang)
    controller.add(mref)

    if (tState.verbosity > 0)
    {
      println(" > adding language " + l.name.toString)
    }

    // Include embedded languages
    var emblangs : List[Name] = List.empty
    if (l.el.isDefined) {
      emblangs = emblangs :+ l.el.get.nm
    }
    if (l.els.isDefined) {
      emblangs = emblangs ::: l.els.get.nms
    }

    for (embl <- emblangs) {
      val fnd : Option[Theory] = tState.languages_decl.find(la => la.name.toString.toLowerCase == embl.toString.toLowerCase)
      hAssert(fnd.isDefined, tState.languages_decl)

      val includee : Theory = fnd.get
      val includer : Theory = nu_lang
      val theInclude = PlainInclude(includee.path,includer.path)
      controller add theInclude
      controller endAdd theInclude

      if (tState.verbosity > 0) {
        println("   > adding include: " + nu_lang.name + " includes " + fnd.get.name)
      }
    }

    if (l.bt.isDefined)
    {
      for (baseType : IMPSSort <- l.bt.get.nms)
      {
        val tp : Term = IMPSTheory.Sort(OMS(IMPSTheory.lutinsIndType))
        val basetype = symbols.Constant(nu_lang.toTerm, doName(baseType.toString), Nil, Some(tp), None, Some("BaseType"))
        if (l.bt.get.src.isDefined) { doSourceRefD(basetype, l.bt.get.src, uri) }
        controller add basetype

        if (tState.verbosity > 0) { println("   > adding language base type: " + baseType.toString + " to " + nu_lang.name) }
      }
    }

    if (l.srts.isDefined)
    {
      /* introduce all sorts with their respective enclosing sorts */
      for (spec : ArgSortSpec <- l.srts.get.specs) {

        //val opt_ind   : Option[Term] = Some(Apply(OMS(IMPSTheory.lutinsPath ? LocalName("sort")), OMS(IMPSTheory.lutinsIndType)))
        val tp     : Term        = IMPSTheory.Sort(OMS(IMPSTheory.lutinsIndType))
        val typing : Declaration = symbols.Constant(nu_lang.toTerm,LocalName(spec.sub.toString),Nil,Some(tp),None,Some("Sort"))
        controller add typing
        if (tState.verbosity > 0) { println("   > adding language sort: " + spec.sub + " (enclosed by " + spec.enc + ") to " + nu_lang.name) }

        doSubsort(spec.sub, spec.enc, nu_lang, spec.src, uri)
      }
    }

    if (l.ex.isDefined)
    {
      for (tal : ArgTypeSortAList <- l.ex.get.specs)
      {
        val sort         : IMPSSort = tal.srt
        var numeric_name : String   = ""

        val numeric_type   : Term = tal.tp match
        {
          case NumericalType.INTEGERTYPE  =>
            numeric_name = "integer_type"
            OMS(IMPSTheory.lutinsPath ? "integerType")
          case NumericalType.RATIONALTYPE =>
            numeric_name = "rational_type"
            OMS(IMPSTheory.lutinsPath ? "rationalType")
          case NumericalType.OCTETTYPE    =>
            numeric_name = "octet_type"
            OMS(IMPSTheory.lutinsPath ? "octetType")
        }

        val sortTerm : Term = matchSort(tal.srt,nu_lang)
        addSortEqualityRule(sortTerm,numeric_type,nu_lang,tal.srt.toString,numeric_name)

        val supersorts = tState.allSupersorts(sortTerm)
        doTransitiveSubtyping(numeric_type,supersorts,nu_lang)
        doTransitiveSubtyping(sortTerm,supersorts,nu_lang)
      }
    }

    if (l.cnsts.isDefined)
    {
      for (pair : ArgConstantSpec <- l.cnsts.get.specs)
      {
        val mth_tp : Term = tState.bindUnknowns(doSort(pair.enc, nu_lang))
        val l_const = symbols.Constant(nu_lang.toTerm,doName(pair.nm.s),Nil,Some(mth_tp),None,Some("Constant"))
        if (l.cnsts.get.src.isDefined) { doSourceRefD(l_const,l.cnsts.get.src, uri) }
        if (tState.verbosity > 0)
        {
          println(" > adding constant " + pair.nm.s + " : " + pair.enc + " to " + nu_lang.path)
        }
        controller add l_const
      }
    }

    controller endAdd nu_lang

    tState.languages_raw  = tState.languages_raw :+ l
    tState.languages_decl = tState.languages_decl :+ nu_lang
  }

  def doTranslation(d : DFTranslation, docPath: DPath, uri : URI) : Unit = d match
  {
    case t@DFTranslation(name,force,forceQL,dontEnrich,sourcet,targett,_,fixed,sortPairs,constPairs,core,tic,src,cmt) =>

      // TODO: Deal with these delays.
      val delays : List[String] = List("ACT->LEFT%TRANS","ACT->RIGHT%TRANS", "ACT->SET%CONJUGATE", "ACT->LEFT-MUL", "ACT->RIGHT-MUL")
      if (delays.contains(name.s)) { tState.delayed = (t,uri) :: tState.delayed ; return }

      var translated_constant_names : List[LocalName] = Nil

      tState.translations_raw = t :: tState.translations_raw

      val ln : LocalName = doName(name.s)

      if (tState.verbosity > 0) { println(" > translating Translation " + name) }

      // Source and Target need to be defined!
      assert(tState.theories_decl.exists(t => t.name.toString.toLowerCase == doName(sourcet.thy.s).toString.toLowerCase))
      val source_thy   : Theory = getTheory(doName(sourcet.thy.s).toString.toLowerCase)
      val source_thy_t : Term = source_thy.toTerm

      assert(tState.theories_decl.exists(t => t.name.toString.toLowerCase == doName(targett.thy.s).toString.toLowerCase))
      val target_thy : Theory = getTheory(doName(targett.thy.s).toString.toLowerCase)
      val target_thy_t : Term = target_thy.toTerm

      val nu_view = View(bt.narrationDPath, ln, source_thy_t, target_thy_t, isImplicit = false)
      val mref : MRef = MRef(docPath,nu_view.path)
      controller add nu_view
      controller add mref

      tState.translations_decl = nu_view :: tState.translations_decl

      if (fixed.isDefined)
      {
        for (ft <- fixed.get.ts.indices)
        {
          if (tState.verbosity > 1) { println("  > translation " + name.s + " fixes theory " + fixed.get.ts(ft).s) }

          val fix : Theory = getTheory(fixed.get.ts(ft).s.toLowerCase)

          val cn : LocalName = LocalName(ComplexStep(fix.path))

          val id_fix = Structure(nu_view.toTerm,cn,fix.toTerm, Some(OMIDENT(fix.toTerm)), isImplicit = false) // get it? :D
          controller add id_fix
        }
      }

      /* Translate all explicity listed sort pairs */

      if (sortPairs.isDefined) {
        for (sp <- sortPairs.get.defs) {
          var tar : String = ""

          var target : Either[IMPSSort,IMPSMathExp] = sp.srt match
          {
            case scala.util.Left(scala.util.Left(n@srt_name))          => tar = n.toString ; scala.util.Left(IMPSAtomSort(srt_name.s))
            case scala.util.Left(scala.util.Right(n@srt_dfstr))        => tar = n.toString ; assert(sp.mth.isDefined) ; scala.util.Right(sp.mth.get)
            case scala.util.Right(scala.util.Left(n@pred_srt_dfstr))   => tar = n.toString ; assert(sp.mth.isDefined) ; scala.util.Right(sp.mth.get) // TODO: FIX THESE!!!
            case scala.util.Right(scala.util.Right(n@indic_srt_dfstr)) => tar = n.toString ; assert(sp.mth.isDefined) ; scala.util.Right(sp.mth.get)
          }

          val target_term : Term = target match {
            case scala.util.Left(is)  => val q = locateMathSymbolHome(is.toString,target_thy) ; assert(q.isDefined) ; matchSort(is,q.get)
            case scala.util.Right(im) => doMathExp(im,target_thy,Nil)
          }

          val target_tp : Option[Term] = target match {
            case scala.util.Left(is) => Some(IMPSTheory.Sort(OMS(IMPSTheory.lutinsIndType)))
            case scala.util.Right(_) => None
          }

          val quelle : Option[Theory] = locateMathSymbolHome(sp.name.toString, source_thy)
          assert(quelle.isDefined)

          val nu_sort_map = symbols.Constant(nu_view.toTerm,ComplexStep(quelle.get.path) / doName(sp.name.s),Nil,target_tp,Some(target_term),None)
          if (tState.verbosity > 1) { println("  >  adding sort-mapping: " + sp.name.s + " → " + tar + " // " + ComplexStep(quelle.get.path) / doName(sp.name.s)) }

          translated_constant_names = doName(sp.name.toString) :: translated_constant_names
          //println(nu_sort_map)
          controller add nu_sort_map
        }
      }

      /* Translate all explicity listed constant pairs */

      if (constPairs.isDefined) {
        for (cp <- constPairs.get.defs) {
          var tar : String = ""
          val target_const_term : Term = cp.const.o match {
            case scala.util.Left(df) =>
              tar = df._1.s
              assert(df._2.isDefined)
              doMathExp(df._2.get,target_thy,Nil)

            case scala.util.Right(n) => {
              tar = n.s
              val quelle = locateMathSymbolHome(n.s,target_thy)

              if (quelle.isDefined) { doMathExp(IMPSMathSymbol(n.s),quelle.get,Nil) }
              else {
                assert(isRatLiteral(n.s) || isIntLiteral(n.s) || isOctLiteral(n.s))
                doLiteral(n.s)
              }
            }
          }

          val quelle : Theory = locateMathSymbolHome(cp.name.toString, source_thy).get

          val nu_const_map = symbols.Constant(nu_view.toTerm,ComplexStep(quelle.path) / doName(cp.name.s),Nil,None,Some(target_const_term),None)
          if (true) { println("  > adding constant-mapping: " + cp.name.s + " → " + tar + " // " + ComplexStep(quelle.path) / doName(cp.name.s)) }

          translated_constant_names = doName(cp.name.toString) :: translated_constant_names
          controller add nu_const_map
        }
      }

      /* Add translations from all non-mentioned constants to themselves if source = target */
      // ToDo: What about when it isn't? Do we need some manual addings there, too?

      if (source_thy == target_thy)
      {
        if (tState.verbosity > 1) { println("  > adding constant-endo-mappings.") }

        for (c <- source_thy.getConstants) {
          if (!translated_constant_names.contains(c.name))
          {
            if (tState.verbosity > 4) { println("   > adding constant-endo-mapping for " + c.name) }
            val quelle : Theory = locateMathSymbolHome(c.name.toString, source_thy).get
            val orig_c = findConstant(c.name.toString,quelle)
            val nu_id_const = symbols.Constant(nu_view.toTerm,ComplexStep(quelle.path) / c.name,c.alias,c.tp,Some(orig_c.toTerm),c.rl)
            controller add nu_id_const
          }
        }
      } else {
        assert(source_thy != target_thy)

        val bothInclude = (source_thy.getIncludesWithoutMeta ::: target_thy.getIncludesWithoutMeta).distinct.map(m => controller.getTheory(m))
        for (bothinc <- bothInclude)
        {
          if (tState.verbosity > 1) {
            println("  > Both " + source_thy.name + " and " + target_thy.name + " include " + bothinc.name)
            println("  > Adding constant endo-mappings for " + bothinc.name)
          }

          for (c <- bothinc.getConstants) {
            if (!translated_constant_names.contains(c.name))
            {
              if (tState.verbosity > 4) { println("   > adding constant-endo-mapping for " + c.name) }
              val orig_c = findConstant(c.name.toString,bothinc)
              val nu_id_const = symbols.Constant(nu_view.toTerm,ComplexStep(bothinc.path) / c.name,c.alias,c.tp,Some(orig_c.toTerm),c.rl)
              controller add nu_id_const
            }
          }
        }
      }

      /* Include MetaData */

      if (force.isDefined) {
        val mv : GlobalName =     IMPSImportTask.rootdpath ? name.s ? LocalName("force")
        val mo : Obj        = OMS(IMPSImportTask.rootdpath ? name.s ? "present")
        nu_view.metadata.add(new MetaDatum(mv,mo))
      }

      if (forceQL.isDefined) {
        val mv : GlobalName =     IMPSImportTask.rootdpath ? name.s ? LocalName("force-under-quick-load")
        val mo : Obj        = OMS(IMPSImportTask.rootdpath ? name.s ? "present")
        nu_view.metadata.add(new MetaDatum(mv,mo))
      }

      if (dontEnrich.isDefined) {
        val mv : GlobalName =     IMPSImportTask.rootdpath ? name.s ? LocalName("dont-enrich")
        val mo : Obj        = OMS(IMPSImportTask.rootdpath ? name.s ? "present")
        nu_view.metadata.add(new MetaDatum(mv,mo))
      }

      doSourceRefT(nu_view.toTerm,src, uri)
      controller endAdd nu_view

      controller.simplifier.apply(nu_view)

    case _ => println("Error: Unknown translation structure!")
  }

  def doDeclaration (d : DefForm, uri : URI) : Unit =
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
      case LineComment(_,_,_)
           | DFComment(_,_,_) =>
        val opaque = new OpaqueText(IMPSImportTask.rootdpath, OpaqueText.defaultFormat, StringFragment(d.toString))

      case DFAtomicSort(name,dfs,frm,sort,thy,usgs,witness,src,cmt) =>

        val ln: LocalName = LocalName(thy.thy.s.toLowerCase())

        if (!tState.theories_decl.exists(t => t.name.toString.toLowerCase == ln.toString)) {
          throw new IMPSDependencyException("required theory " + ln + " for atomic sort not found")
        }
        val parent: Theory = getTheory(ln.toString)

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

        doSourceRefD(nu_atomicSort, src, uri)

        if (tState.verbosity > 0)
        {
          println(" > Adding atomic sort: " + name + " (enclosed by " + sort.toString + ")")
        }

        controller add nu_atomicSort
        doSubsort(IMPSAtomSort(name.s), sort, parent, src, uri)

      case DFConstant(name,definition,frm,sort,thy,argsort,usages,src,cmt) =>

        val ln: LocalName = LocalName(thy.thy.s.toLowerCase())

        if (!tState.theories_decl.exists(t => t.name.toString.toLowerCase == ln.toString)) {
          throw new IMPSDependencyException("required theory " + ln + " for constant not found")
        }
        val parent: Theory = getTheory(ln.toString)

        val srt : Term  = tState.bindUnknowns(doSort(curry(sort), parent))
        val mth : Term  = tState.bindUnknowns(doMathExp(frm, parent,Nil))
        val nu_constant : FinalConstant = symbols.Constant(parent.toTerm, LocalName(name.s.toLowerCase()), Nil, Some(srt), Some(mth), Some("Constant"))

        /* Add available MetaData */
        doSourceRefD(nu_constant, src, uri)
        if (usages.isDefined) { doUsages(nu_constant, usages.get.usgs) }

        controller add nu_constant

        if (tState.verbosity > 0)
        {
          println(" > Adding constant: " + nu_constant.name + " : " + sort.toString + " to theory " + parent.name)
          //println(controller.get(parent.path))
        }

      case DFRecursiveConstant(names,defs,maths,sorts,argthy,usgs,defname,src,cmt) =>

        val ln: LocalName = LocalName(argthy.thy.s.toLowerCase())
        if (!tState.theories_decl.exists(t => t.name.toString.toLowerCase == ln.toString)) {
          throw new IMPSDependencyException("required theory " + ln + " for constant not found")
        }
        val parent: Theory = getTheory(ln.toString)

        val theseSorts = sorts.map(curry)

        assert(names.nms.lengthCompare(maths.length) == 0)
        assert(maths.lengthCompare(theseSorts.length) == 0)

        for (i <- names.nms.indices)
        {
          val nm  : LocalName = LocalName(names.nms(i).s.toLowerCase)

          var srt : Term      = tState.bindUnknowns(doSort(theseSorts(i), parent))
          val mth : Term      = tState.bindUnknowns(doMathExp(maths(i), parent, Nil))

          val nu_constant     = symbols.Constant(parent.toTerm, nm, Nil, Some(srt), Some(mth), Some("Recursive Constant"))

          /* Add available MetaData */
          doSourceRefD(nu_constant, src, uri)
          if (usgs.isDefined) { doUsages(nu_constant, usgs.get.usgs) }

          if (tState.verbosity > 0) {
            println(" > Adding recursive constant " + nm + " : "  + theseSorts(i).toString + " to theory " + parent.name.toString)
          }

          controller add nu_constant
        }

      case t@DFTheorem(name,defn,frm,modr,modl,theory,usages,trans,macete,homeTheory,proof,src,cmt) =>

        val delays : List[String] = List("LEFT-LEFT%TRANS-INV", "LEFT-RIGHT%TRANS-INV", "RIGHT-LEFT%TRANS-INV", "RIGHT-RIGHT%TRANS-INV",
        "LEFT-TRANSLATION-MACETE", "RIGHT-TRANSLATION-MACETE", "REVERSE-SET%CONJUGATE-ASSOCIATIVITY", "REVERSE-LEFT%TRANS-ASSOCIATIVITY",
          "REVERSE-RIGHT%TRANS-ASSOCIATIVITY", "LEFT-MUL-MACETE", "RIGHT-MUL-MACETE")
        if (delays.contains(name.s)) { tState.delayed = (t,uri) :: tState.delayed ; return }

        val ln: LocalName = doName(theory.thy.s.toLowerCase)
        if (!tState.theories_decl.exists(t => t.name.toString.toLowerCase == ln.toString)) {
          throw new IMPSDependencyException("required theory " + ln + " for theorem not found")
        }
        val parent: Theory = getTheory(ln.toString)

        if (tState.verbosity > 0)
        {
          println(" > adding theorem " + name + " to theory " + parent.name)
        }

        val trm : Term = doMathExp(frm.get, parent,Nil)
        val thm : Term = tState.bindUnknowns(IMPSTheory.Thm(trm))
        //val prf : Term = tState.bindUnknowns(IMPSTheory.Proofs.MagicProof(trm))
        val nu_theorem = symbols.Constant(parent.toTerm, doName(name.s), Nil, Some(thm), None, Some("Theorem"))
        //                                                                                ^-- proof (Some(prf)) goes here! Some(IMPSTheory.Proofs.MagicProof(trm))

        /* Add available MetaData */
        if (usages.isDefined) {
          doUsages(nu_theorem, usages.get.usgs)
        }
        if (trans.isDefined) {
          doMetaData(nu_theorem, "translation", trans.get.t.s)
        }
        if (macete.isDefined) {
          doMetaData(nu_theorem, "macete", macete.get.mn.s)
        }
        if (homeTheory.isDefined) {
          doMetaData(nu_theorem, "homeTheory", homeTheory.get.nm.s)
        }

        if (modl.isDefined) {
          doMetaData(nu_theorem, "lemma", "present")
        }

        if (modr.isDefined) {
          doMetaData(nu_theorem, "reverse", "present")
        }

        if (proof.isDefined)
        {
          /* opaque proofs are beetter than no proofs */
          val proof_name: StringFragment = StringFragment("Opaque proof of theorem " + name)
          val proof_text: StringFragment = StringFragment(proof.get.prf.toString)

          val opaque = new OpaqueText(parent.path.toDPath, OpaqueText.defaultFormat, StringFragment(proof_name + "\n" + proof_text))
          controller add opaque
        }

        doSourceRefD(nu_theorem, src, uri)
        controller add nu_theorem

      case DFSchematicMacete(name,dfs,_,_,thy,src,cmt) =>

        val ln: LocalName = LocalName(thy.thy.s.toLowerCase)
        if (!tState.theories_decl.exists(t => t.name.toString.toLowerCase == ln.toString)) {
          throw new IMPSDependencyException("required theory " + ln + " for schematic macete not found")
        }
        val parent: Theory = getTheory(ln.toString)

        // Macetes are added as opaque (for now?)
        val opaque = new OpaqueText(parent.path.toDPath, OpaqueText.defaultFormat, StringFragment(d.toString))

        /* Opaque Text doesn't have metadata, apparently, so we don't add the src */
        controller add opaque

      case DFCompoundMacete(name,mspec,src,cmt) =>

        // Macetes are added as opaque (for now?)
        val opaque = new OpaqueText(IMPSImportTask.rootdpath, OpaqueText.defaultFormat, StringFragment(d.toString))

        /* Opaque Text doesn't have metadata, apparently, so we don't add the src */
        controller add opaque

      case DFQuasiConstructor(name,dfs,arglang,fixedthys,src,cmt) =>

        // Quasi-Constructors needed to be built in because they're not parseable

        assert(tState.languages_raw.exists(p => p.name.s.toLowerCase == arglang.lang.s.toLowerCase)
           ||  tState.theories_raw.exists(p => p.name.s.toLowerCase == arglang.lang.s.toLowerCase))

        val parent: Theory = if (!tState.languages_raw.exists(p => p.name.s.toLowerCase == arglang.lang.s.toLowerCase))
        {
          getTheory(LocalName(arglang.lang.s).toString.toLowerCase)
        }
        else
        {
          // Find correct theory for language.
          val ln: LocalName = LocalName(tState.theories_raw.find(t => t.lang.get.lang.s.toLowerCase == arglang.lang.s.toLowerCase).head.name.s)
          if (!tState.theories_decl.exists(t => t.name.toString.toLowerCase == ln.toString.toLowerCase)) {
            throw new IMPSDependencyException("required theory " + ln + " for quasi-constructor not found")
          }
          getTheory(ln.toString.toLowerCase)
        }

        val opaque = new OpaqueText(parent.path.toDPath, OpaqueText.defaultFormat, StringFragment(d.toString))
        controller add opaque

      case r@DFRenamer(_,_,_,_) => tState.renamers = r :: tState.renamers
      case DFTransportedSymbols(names,translation,renamer,src,cmt) =>

        if (tState.verbosity > 1) { println( " > Adding transported symbols along " + translation.t.s + ": " + names.nms.mkString(" ")) }

        val rename : String => String = {
          if (renamer.isDefined) {
            val rnmr = tState.renamers.find(r => r.nm.s.toLowerCase == renamer.get.rn.s.toLowerCase)
            if (rnmr.isEmpty) { println("  > could not find renamer " + renamer.get.rn.s.toLowerCase) ; println(tState.renamers) }
            assert(rnmr.isDefined)
            rnmr.get.toFunction
          } else { identity }
        }

        val trans_decl = tState.translations_decl.find(t => t.name.toString.toLowerCase == translation.t.s.toLowerCase)
        assert(trans_decl.isDefined)

        val sourceName : String = tState.translations_raw.find(t => t.n.s.toLowerCase == translation.t.s.toLowerCase).get.sour.thy.s.toLowerCase
        val source : Theory = getTheory(sourceName)

        val targetName : String = tState.translations_raw.find(t => t.n.s.toLowerCase == translation.t.s.toLowerCase).get.tar.thy.s.toLowerCase
        val target : Theory = getTheory(targetName)

        for (n <- names.nms)
        {
          println("   > Transported symbol: " + n + " => " + rename(n.s) + " (" + source.name +  " ~> " + target.name + ")")

          val p = doName(n.s)
          val q = doName(rename(n.s))

          val nnsource = locateMathSymbolHome(n.s,source)
          assert(nnsource.isDefined)
          val nsource = nnsource.get
          val urimage = nsource.getConstants.find(c => c.name.toString.toLowerCase == n.s.toLowerCase).get

          //println(nsource)
          println("    ~~ urimage: " + urimage + " from " + nsource.name)
          // ToDo: reintegrate types.
          val tp    : Option[Term] = None // if (urimage.tp.isDefined) { Some(controller.library.ApplyMorphs(urimage.tp.get,trans_decl.get.toTerm)) } else { None }
          val image : Term         = controller.library.ApplyMorphs(urimage.toTerm,trans_decl.get.toTerm)
          val nu_trans_symbol = symbols.Constant(target.toTerm,q,Nil,tp,Some(image),Some("transported symbol"))
          controller add nu_trans_symbol
        }

      case DFTheoryEnsemble(name,baseTheory,fixed,reprenamer,src,cmt) =>

        val thyName : String         = if (baseTheory.isDefined) {baseTheory.get.nm.s} else {name.s}
        val base    : Theory = getTheory(thyName.toLowerCase)

        if (tState.verbosity > 0) {
          println(" > adding theory-ensemble " + name.s)
        }

        val renamer : Int => String => String = if (reprenamer.isDefined)
        {
          val rn = tState.renamers.find(r => r.nm.toString.toLowerCase == reprenamer.get.nm.toString.toLowerCase)
          assert(rn.isDefined)
          ??? // I don't think this actually happens
        } else {
          n : Int => (s : String) => s + "_" + n.toString
        }

        val fxd : List[Theory] = if (fixed.isDefined) {
          fixed.get.ts.map(t => t.s).map(nm => tState.theories_decl.find(thy => thy.name.toString.toLowerCase == nm.toLowerCase).get)
        } else {
          List.empty
        }

        val nu_ensemble = new TheoryEnsemble(name.s,base,fxd,renamer)
        tState.ensembles = nu_ensemble :: tState.ensembles

      case DFTheoryEnsembleOverloadings(basename, numbers, src, cmt) =>

        if (tState.verbosity > 1) { println(" > adding overloadings to multiples (" + numbers.ns.mkString(" ") + ") of theory " + basename.s) }

        assert(tState.ensembles.exists(e => e.name.toString.toLowerCase == basename.s.toLowerCase))
        val ensemble : TheoryEnsemble = tState.ensembles.find(e => e.name.toString.toLowerCase == basename.s.toLowerCase).get

        val base     : Theory = getTheory(basename.s)
        val nums     : List[Int]      = numbers.ns.map(_.n)
        assert(nums.nonEmpty)

        val multiples : List[Theory] = nums.map(j => ensemble.multipleMap(j))
        for (m <- multiples) {
          for (mc <- m.getConstants) {
            for (dest <- tState.translations_decl) {
              println(" [>] [overloading for " + mc.name + " along " + dest.name + " goes here]")
            }
          }
        }

      case DFOverloading(operator, pairs, _, _) =>

        val name : LocalName = LocalName(operator.s)

        for (pair <- pairs)
        {
          val thy : Theory = getTheory(pair.tname.s)
          val c   : Constant       = getConstant(pair.sname.s,thy)

          val nu_constant = symbols.Constant(thy.toTerm,name,List.empty,c.tp,Some(c.toTerm),Some("Overloading"))
          controller add nu_constant
        }

      case DFInductor(name,princ,thy,trans,bh,ish,du,src,cmt) =>

        val ln: LocalName = LocalName(thy.thy.s.toLowerCase)
        if (!tState.theories_decl.exists(t => t.name.toString.toLowerCase == ln.toString)) {
          throw new IMPSDependencyException("required theory " + ln + " for inductor " + name.s + " not found")
        }
        val parent: Theory = getTheory(ln.toString)
        val opaque = new OpaqueText(parent.path.toDPath, OpaqueText.defaultFormat, StringFragment(d.toString))
        if (tState.verbosity > 1) { println("   > adding inductor " + name.toString + " to theory " + thy.thy.s) }
        controller add opaque

      case DFAlgebraicProcessor(nm,_,lang,_,_,_,_,_) =>

        // Processors are theory-independent
        val opaque = new OpaqueText(IMPSImportTask.rootdpath, OpaqueText.defaultFormat, StringFragment(d.toString))
        if (tState.verbosity > 1) {
          println(" > adding algebraic-processor " + nm.s)
        }
        controller add opaque

      case DFTheoryProcessors(nm,_,_,_,_,_) =>

        // Processors are theory-independent
        val opaque = new OpaqueText(IMPSImportTask.rootdpath, OpaqueText.defaultFormat, StringFragment(d.toString))
        if (tState.verbosity > 1) {
          println(" > adding theory-processor " + nm.s)
        }
        controller add opaque

      case DFOrderProcessor(nm,_,_,_,_,_) =>

        // Processors are theory-independent
        val opaque = new OpaqueText(IMPSImportTask.rootdpath, OpaqueText.defaultFormat, StringFragment(d.toString))
        if (tState.verbosity > 1) {
            println(" > adding order-processor " + nm.s)
        }
        controller add opaque

      case s@DFScript(nm,_,_,_,_,_,_) =>

        val opaque = new OpaqueText(IMPSImportTask.rootdpath, OpaqueText.defaultFormat, StringFragment(d.toString))
        if (tState.verbosity > 1) {
          println(" > adding script: " + nm.s)
        }
        controller add opaque

      case DFPrintSyntax(_,_,_,_,_,_,_,_)
         | DFParseSyntax(_,_,_,_,_,_,_,_) => // Not used, because they are hardcoded.
      case Heralding(_,_,_)
           | DFIncludeFiles(_,_,_,_,_)
           | DFSection(_,_,_,_,_)
           | Define(_,_,_)
           | ArgSet(_,_,_)
           | DFLoadSection(_,_,_) => // Not used, because information is present elsewhere.
      case some =>
        if (tState.verbosity > 0) {
          println(" > Error: Unknown decl encountered, not translated!")
          println(" > " + some.getClass + "\n")
        }
    }
  }

  def makeReplica(base : Theory, docPath : DPath, ns : DPath, renamer : String => String) : Theory =
  {
    // Create a new replica
    val nu_replica = new Theory(ns,
                                        LocalName(renamer(base.name.toString)),
                                        Some(IMPSTheory.QCT.quasiLutinsPath),
                                        modules.Theory.noParams,
                                        modules.Theory.noBase)

    if (tState.verbosity > 0) {
      println(" > creating replica " + nu_replica.name.toString)
    }

    val mref : MRef = MRef(docPath,nu_replica.path)
    controller.add(nu_replica)
    controller.add(mref)

    val nu_name : String = renamer(base.name.toString)

    // Fix all includes
    val includes : List[Theory] = base.getIncludesWithoutMeta.map(controller.getTheory)

    for (fix <- includes) {
      val theInclude = PlainInclude(fix.path,nu_replica.path)
      controller add theInclude
      controller endAdd theInclude
    }
    // Each replica only carries one structure (a  kind of theory morphism) from the base theory into it.
    val rep_struc = Structure(nu_replica.toTerm,LocalName(nu_name),base.toTerm,isImplicit = false)
    controller add rep_struc

    for (fix <- includes) {
      val fixname = LocalName(ComplexStep(fix.path))
      val nu_fix = Structure(rep_struc.toTerm,fixname,fix.toTerm, Some(OMIDENT(fix.toTerm)), isImplicit = false)
      controller add nu_fix
    }

    val replic_trans : View = View(docPath,LocalName(renamer("replicating_translation")),base.toTerm,nu_replica.toTerm,isImplicit = false)
    controller add replic_trans

    /**
    for (c <- base.getConstants)
    {
      val chome : Term            = OMMOD(rep_struc.path.toMPath)
      val alias : List[LocalName] = LocalName(renamer(c.name.toString)) :: c.alias
      val nu_constant = symbols.Constant(chome,ComplexStep(c.parent)/c.name,alias,None,None,None)
      controller add nu_constant

      if (tState.verbosity > 2) {
        println("     > adding constant " + nu_constant.name + " to replica theory " + rep_struc.name)
      }
    }**/

    controller endAdd replic_trans

    // Elaborate Structures
    controller.simplifier(nu_replica)

    nu_replica
  }

  def findKind(sort : IMPSSort) : Term = sort match {
    case IMPSUnknownSort(_)         => tState.doUnknown()
    case IMPSAtomSort("ind")        => OMS(IMPSTheory.lutinsIndType)
    case IMPSAtomSort("prop")       => OMS(IMPSTheory.lutinsPropType)
    case IMPSAtomSort("bool")       => OMS(IMPSTheory.lutinsPropType)
    case IMPSAtomSort("unit%sort")
         | IMPSAtomSort("unitsort") => OMS(IMPSTheory.lutinsIndType)
    case IMPSAtomSort(_)            => OMS(IMPSTheory.lutinsIndType)
    case IMPSBinaryFunSort(s1,s2)   => IMPSTheory.FunType(findKind(s1),findKind(s2))
    case IMPSSetSort(s)             => IMPSTheory.FunType(findKind(s),OMS(IMPSTheory.lutinsIndType))
    case IMPSNaryFunSort(sorts)     => findKind(curry(sort))
  }

  def matchSort(e : IMPSSort, t : Theory) : Term =
  {
    if (e.isInstanceOf[IMPSNaryFunSort]) { return matchSort(curry(e),t)}

    e match {
      case IMPSUnknownSort(h)   => tState.doUnknown(Some(h))
      case IMPSAtomSort("ind")  => OMS(IMPSTheory.lutinsPath ? "ind")
      case IMPSAtomSort("prop") => OMS(IMPSTheory.lutinsPath ? "bool")
      case IMPSAtomSort("bool") => OMS(IMPSTheory.lutinsPath ? "bool")
      case IMPSAtomSort("unit%sort")
         | IMPSAtomSort("unitsort") => OMS(IMPSTheory.lutinsPath ? "unitsort")
      case IMPSBinaryFunSort(s1, s2) =>

        val tpA: Term = findKind(s1)
        val tpB: Term = findKind(s2)

        val sortA: Term = matchSort(s1, t)
        val sortB: Term = matchSort(s2, t)

        IMPSTheory.FunSort(tpA, tpB, sortA, sortB)

      case IMPSSetSort(s) =>

        val tp  : Term = findKind(s)
        val srt : Term = matchSort(s,t)

        IMPSTheory.Sets(tp,srt)

      case IMPSAtomSort(srt) =>
        val thy : Option[Theory] = locateMathSymbolHome(srt,t)
        assert(thy.isDefined)
        OMS(thy.get.path ? srt)
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
      case IMPSNaryFunSort(sorts)          =>
        if (sorts.length == 2) {
          IMPSBinaryFunSort(curry(sorts.head), curry(sorts(1)))
        } else {
          IMPSBinaryFunSort(curry(sorts.head), curry(IMPSNaryFunSort(sorts.tail)))
        }
      case IMPSSetSort(st) => IMPSSetSort(curry(st))
    }
  }

  def doSort(d : IMPSSort, t : Theory) : Term =
  {
    val d_prime : IMPSSort = curry(d)
    val tp      : Term     = findKind(d_prime)

    IMPSTheory.exp(tp, matchSort(d_prime,t))
  }

  class SubtypeJudgRule(val tm1 : Term, val tm2 : Term, val by : GlobalName) extends SubtypingRule {
    val head : GlobalName = by
    def applicable(tp1: Term, tp2: Term): Boolean = tp1.hasheq(tm1) && tp2.hasheq(tm2)
    def apply(solver: Solver)(tp1: Term, tp2: Term)(implicit stack: Stack, history: History): Option[Boolean] = {
      Some(true)
    }
  }

  class IMPSTypeEqualityRule(unter : List[GlobalName], kopf : GlobalName) extends TypeBasedEqualityRule(unter, kopf) {
    def apply(solver: Solver)(tm1: Term, tm2: Term, tp: Term)(implicit stack: Stack, history: History): Option[Boolean] = {
      Some(true)
    }
    def applicableToTerm(tm: Term): Boolean = true
  }

  /* Introduces a sort to a theory and also assigns the enclosing sort to it. */
  def doSubsort(subsort : IMPSSort, supersort : IMPSSort, thy : Theory, src : SourceInfo, uri : URI) : Unit =
  {
    /* enclosing sort should already be defined */
    if (tState.verbosity > 0) {
      println(" > Adding sort: " + subsort.toString + ", enclosed by " + supersort.toString + " in " + thy.name.toString)
    }

    val jdgmtname : LocalName    = LocalName(subsort.toString + "_sub_" + supersort.toString)

    val the_subsort   : Term = tState.bindUnknowns(matchSort(subsort,thy))
    val the_supersort : Term = tState.bindUnknowns(matchSort(supersort,thy))
    val the_kind      : Term = tState.bindUnknowns(findKind(supersort))

    val subs      : Term = ApplySpine(OMS(IMPSTheory.lutinsPath ? LocalName("subsort")), the_kind, the_subsort, the_supersort)

    val jdgmttp   : Option[Term] = Some(IMPSTheory.Thm(subs))

    val judgement : Declaration  = symbols.Constant(thy.toTerm,jdgmtname,Nil,jdgmttp,None,Some("Subsort_2"))
    doSourceRefD(judgement, src, uri)
    controller add judgement

    addSubtypingRule(the_subsort,the_supersort,thy,judgement.path,subsort.toString,supersort.toString)
    tState.supersorts = tState.supersorts + (the_subsort -> (tState.supersorts.getOrElse(the_subsort,Nil) ::: List(the_supersort)))
    doTransitiveSubtyping(the_subsort,List(the_supersort).distinct,thy)
  }

  // makes a constant more readable by trimming everything but the localname
  def makeReadable(str : String) : String = str.reverse.takeWhile(c => c != '?').reverse

  def doTransitiveSubtyping(the_subsort : Term, known_supersorts : List[Term], theory : Theory) : Unit =
  {
    val allSuperSorts : List[Term] = (tState.allSupersorts(the_subsort) ::: known_supersorts).distinct
    println("all supersorts of " + makeReadable(the_subsort.toString) + ": " + allSuperSorts.map(e => makeReadable(e.toString)))
    for (the_supersort <- allSuperSorts)
    {
      val jdgmtname : LocalName = LocalName(the_subsort.toString + "_is_subsort_of_" + the_supersort.toString)
      val the_kind  : Term      = OMS(IMPSTheory.lutinsIndType)

      val subs      : Term = ApplySpine(OMS(IMPSTheory.lutinsPath ? LocalName("subsort")), the_kind, the_subsort, the_supersort)
      val jdgmttp   : Option[Term] = Some(IMPSTheory.Thm(subs))

      val judgement : Declaration  = symbols.Constant(theory.toTerm,jdgmtname,Nil,jdgmttp,None,Some("Subsort"))
      controller add judgement

      val subName   : String = makeReadable(the_subsort.toString)
      val supName   : String = makeReadable(the_supersort.toString)

      addSubtypingRule(the_subsort, the_supersort, theory, judgement.path, subName, supName)
    }
  }

  def addSortEqualityRule(sort1 : Term, sort2 : Term, theory : Theory, praesSort1 : String = "", praesSort2 : String = "") : Unit =
  {
    val present_sort1 : String = if (praesSort1 == "") {sort1.toString} else praesSort1
    val present_sort2 : String = if (praesSort2 == "") {sort2.toString} else praesSort2
    val rulename  : LocalName = LocalName(present_sort1 + "_is_exactly_" + praesSort1)

    val rule : IMPSTypeEqualityRule = new IMPSTypeEqualityRule(Nil,IMPSTheory.lutinsIndType)
    val ruleConst : RuleConstant = RuleConstant(theory.toTerm,rulename,None,Some(rule))
    ruleConst.setOrigin(GeneratedBy(this))

    if (tState.verbosity > 2) {
      println(" > Adding type-equality rule: " + present_sort1 + " = " + present_sort2)
    }
    controller add ruleConst
  }

  /* Adding subtyping rules the checker can use. Note: subsort and supersort need exps around them. */
  def addSubtypingRule(subsort : Term, supersort : Term, theory : Theory, origin : GlobalName, subname : String = "", supname : String = "") : Unit =
  {
    val present_subsort   : String = if (subname == "") {subsort.toString} else subname
    val present_supersort : String = if (supname == "") {subsort.toString} else supname
    val rulename  : LocalName = LocalName(present_subsort + "_<:_" + present_supersort)

    val expSubsort   : Term = IMPSTheory.exp(OMS(IMPSTheory.lutinsIndType),subsort)
    val expSupersort : Term = IMPSTheory.exp(OMS(IMPSTheory.lutinsIndType),supersort)

    val rule      : SubtypeJudgRule = new SubtypeJudgRule(expSubsort, expSupersort, origin)
    val ruleConst : RuleConstant    = RuleConstant(theory.toTerm,rulename,None,Some(rule))
    ruleConst.setOrigin(GeneratedBy(this))

    if (tState.knownsubtyperules.contains(rulename)) {
      if (tState.verbosity > 2) {
        println(" > Skipping subtyping rule: exp " + present_subsort + " <: exp " + present_supersort + " (already known)")
      }
    }
    else {
      if (tState.verbosity > 2) {
        println(" > Adding subtyping rule: exp " + present_subsort + " <: exp " + present_supersort)
      }

      tState.knownsubtyperules = tState.knownsubtyperules + rulename
      controller add ruleConst
    }
  }

  def isIntLiteral(s : String) : Boolean = { s.forall(_.isDigit) || (s.startsWith("-") && s.tail.nonEmpty && s.tail.forall(_.isDigit)) }
  def isRatLiteral(s : String) : Boolean = false
  def isOctLiteral(s : String) : Boolean = { s.endsWith("#8") && s.init.init.nonEmpty && s.init.init.forall(_.isDigit) }

  def doLiteral(s : String) : Term =
  {
    if (isIntLiteral(s)) { IntLiterals.parse(s) }
    else if (isOctLiteral(s)) { OctLiterals.parse(s.init.init) }
    else { ??? } // ToDo: Ratliterals
  }

  /* Translate IMPS Math Expressions to Terms */
  def doMathExp(d : IMPSMathExp, thy : Theory, cntxt : List[(IMPSVar,IMPSSort)]) : Term =
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

        if (s.startsWith("\"") && s.endsWith("\"")) { doMathExp(IMPSMathSymbol(s.tail.init),thy,cntxt) }
        else if (isIntLiteral(s) || isRatLiteral(s) || isOctLiteral(s)) { doLiteral(s) }
        else
        {
          val srcthy : Option[Theory] = locateMathSymbolHome(s,thy)
          if (srcthy.isEmpty) { println("> ERROR: Could not find home for math symbol: " + s) ; println("\n\n" + thy)}
          assert(srcthy.isDefined)
          OMS(srcthy.get.path ? LocalName(s))
        }
        //  Rational Literals
        //  case "i/j" => OMLIT((BigInt(i),BigInt(j)),RatLiterals)

      case IMPSIndividual()       => OMS(IMPSTheory.lutinsPath ? "anIndividual")
      case IMPSTruth()            => OMS(IMPSTheory.lutinsPath ? "thetrue")
      case IMPSFalsehood()        => OMS(IMPSTheory.lutinsPath ? "thefalse")

      case IMPSNegation(p)        => IMPSTheory.Negation(doMathExp(p,thy,cntxt))
      case IMPSIf(p,t1,t2)        => IMPSTheory.If(tState.doUnknown(), tState.doUnknown(), doMathExp(p,thy,cntxt), doMathExp(t1,thy,cntxt), doMathExp(t2,thy,cntxt))
      case IMPSIff(p, q)          => IMPSTheory.Iff(doMathExp(p,thy,cntxt), doMathExp(q,thy,cntxt))
      case IMPSIfForm(p,q,r)      => IMPSTheory.If_Form(doMathExp(p,thy,cntxt), doMathExp(q,thy,cntxt), doMathExp(r,thy,cntxt))
      case IMPSEquals(p,q)        =>

        var a     : Term = null
        var alpha : Term = null
        var beta  : Term = null

        p match {
          case IMPSVar(_) =>
            if (cntxt.map(_._1).contains(p)) {
              val theSort = cntxt.find(k => k._1 == p).get._2
              alpha = matchSort(theSort,thy)
              a = findKind(theSort)
            }
          case _          => ()
        }

        q match {
          case IMPSVar(_) =>
            if (cntxt.map(_._1).contains(q)) {
              val theSort = cntxt.find(k => k._1 == q).get._2
              beta = matchSort(theSort,thy)
              if (a == null) { a = findKind(theSort) }
            }
          case _          => ()
        }

        if (alpha == null) { alpha = tState.doUnknown() }
        if (beta  == null) { beta  = tState.doUnknown() }
        if (a     == null) { a     = tState.doUnknown() }

        IMPSTheory.Equals(a,alpha,beta,doMathExp(p,thy,cntxt),doMathExp(q,thy,cntxt))

      case IMPSDisjunction(ls)    => IMPSTheory.Or(ls map (x => doMathExp(x,thy,cntxt)))
      case IMPSConjunction(ls)    => IMPSTheory.And(ls map (x => doMathExp(x,thy,cntxt)))
      case q@IMPSLambda(vs,_)     => doIMPSLambda(curryIMPSlambda(q), thy, cntxt ::: vs)
      case q@IMPSForAll(vs,_)     => doIMPSForall(curryIMPSforall(q), thy, cntxt ::: vs)
      case q@IMPSForSome(vs,_)    => doIMPSForsome(curryIMPSforsome(q),thy, cntxt ::: vs)
      case IMPSImplication(p,q)   => IMPSTheory.Implies(doMathExp(p,thy,cntxt), doMathExp(q,thy,cntxt))
      case IMPSApply(f,ts)        =>

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
                alpha = matchSort(s1,thy)
                beta  = matchSort(s2,thy)
                a     = findKind(s1)
                b     = findKind(s2)
              case IMPSSetSort(s1) =>
                alpha = matchSort(s1,thy)
                beta  = matchSort(IMPSAtomSort("unitsort"),thy)
                a     = findKind(s1)
                b     = IMPSTheory.lutinsIndType()
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

      case IMPSIota(v1,s1,p)      =>
        val s       = curry(s1)
        val the_exp = doMathExp(p,thy,cntxt ::: List((v1,s)))
        val the_srt = matchSort(s,thy)
        val the_knd = findKind(s)

        val target = info.kwarc.mmt.lf.Lambda(LocalName(v1.v), doSort(s,thy), the_exp)

        IMPSTheory.Iota(the_knd,the_srt,target)

      case IMPSIotaP(v1,s1,p)     =>
        val s       = curry(s1)
        val the_exp = doMathExp(p,thy,cntxt ::: List((v1,s)))
        val the_srt = matchSort(s,thy)

        val target = info.kwarc.mmt.lf.Lambda(LocalName(v1.v), doSort(s,thy), the_exp)

        IMPSTheory.IotaP(the_srt,target)

      case IMPSIsDefined(r)       => IMPSTheory.IsDefined(tState.doUnknown(), tState.doUnknown(), doMathExp(r,thy,cntxt))
      case IMPSIsDefinedIn(r,s)   => IMPSTheory.IsDefinedIn(findKind(s), tState.doUnknown(), doMathExp(r,thy,cntxt), matchSort(s,thy))
      case IMPSUndefined(s)       => IMPSTheory.Undefined(findKind(s), matchSort(s,thy))

      case IMPSTotal(f,bs)         =>

        var alpha_s : IMPSSort = null
        var beta_s  : IMPSSort = null

        curry(bs) match
        {
          case IMPSBinaryFunSort(s1,s2) => alpha_s = s1 ; beta_s = s2
          case _ => assert(false) // Impossible
        }

        assert(alpha_s != null && beta_s != null)

        val a     = findKind(alpha_s)
        val b     = findKind(beta_s)
        val alpha = matchSort(alpha_s,thy)
        val beta  = matchSort(beta_s,thy)
        val func  = doMathExp(f,thy,cntxt)

        IMPSTheory.Total(a,b,alpha,beta,func)

      case IMPSNonVacuous(p) =>
        if (cntxt.map(_._1).contains(p)) {
          val foo = cntxt.find(k => k._1 == p).get
          foo._2 match {
            case IMPSBinaryFunSort(s1,s2) =>
              val the_exp = doMathExp(p,thy,cntxt)
              val the_srt = matchSort(s1,thy)
              assert(s2 == IMPSAtomSort("bool") || s2 == IMPSAtomSort("prop"))
              IMPSTheory.Nonvacuous(findKind(s1),the_srt,the_exp)

            case _ => ???
          }
        } else { IMPSTheory.Nonvacuous(tState.doUnknown(),tState.doUnknown(),doMathExp(p,thy,cntxt)) }

      case IMPSQuasiEquals(p,q) =>

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

      case qc : IMPSUserDefinedQuasiConstructor => doQuasiConstructor(qc,thy,cntxt)

      case _ => ??!("Error: Unknown IMPSMathExp\n" + d + " (" + d.getClass + ")")
    }
  }

  def doQuasiConstructor(d : IMPSMathExp, thy : Theory, cntxt : List[(IMPSVar,IMPSSort)]) : Term = d match
  {
    /* QCs from indicators.t */

    case IMPSQCPred2Indicator(m) =>
      val ca = findSortFromContext(m, cntxt)
      var as: IMPSSort = ca.getOrElse(IMPSUnknownSort(tState.freshHash()))

      assert(as.isInstanceOf[IMPSUnknownSort] | as.isInstanceOf[IMPSBinaryFunSort] | as.isInstanceOf[IMPSNaryFunSort])

      val srt : IMPSSort = as match {
        case IMPSBinaryFunSort(s1,s2) => hAssert(s2 == IMPSAtomSort("bool") || s2 == IMPSAtomSort("prop"), s2) ; s1
        case IMPSNaryFunSort(srts)    => assert(srts.last == IMPSAtomSort("bool") || srts.last == IMPSAtomSort("prop"))
                                         assert(srts.length == 2)
                                         srts.head
        case IMPSUnknownSort(_)       => IMPSUnknownSort(tState.freshHash())
      }
      val t1 = doMathExp(m, thy, cntxt)

      IMPSTheory.QCT.pred2indicQC(findKind(srt), matchSort(srt, thy), t1)

    case IMPSQCSort2Indicator(und) =>
      assert(und.isInstanceOf[IMPSUndefined])
      val srt : IMPSSort = und match {
        case IMPSUndefined(s) => s
        case _ => ???
      }
      val wit : Term = doMathExp(und,thy,cntxt)
      IMPSTheory.QCT.sort2indicQC(findKind(srt),matchSort(srt,thy),wit)

    case IMPSQCIn(e1,e2) =>
      val ca = findSortFromContext(e1,cntxt) ; val cb = findSortFromContext(e2,cntxt)
      val as : IMPSSort = ca.getOrElse(IMPSUnknownSort(tState.freshHash()))
      val t1 = doMathExp(e1,thy,cntxt) ; val t2 = doMathExp(e2,thy,cntxt)

      val k = findKind(as)
      val s = matchSort(as,thy)

      IMPSTheory.QCT.inQC(k,s,t1,t2)

    case IMPSQCSubsetEQ(e1,e2) =>
      val ca = findSortFromContext(e1,cntxt)
      val cb = findSortFromContext(e2,cntxt)

      var uu : IMPSSort = ca.getOrElse(cb.getOrElse(IMPSUnknownSort(tState.freshHash())))
      if (!uu.isInstanceOf[IMPSUnknownSort]) {
        assert(uu.isInstanceOf[IMPSBinaryFunSort])
        uu match {
          case IMPSBinaryFunSort(s1,s2) =>
            assert(s2 == IMPSAtomSort("unit%sort"))
            uu = s1
          case _ => ??!("This never happens")
        }
      }

      val t1 = doMathExp(e1,thy,cntxt)
      val t2 = doMathExp(e2,thy,cntxt)

      IMPSTheory.QCT.subseteqQC(findKind(uu), matchSort(uu,thy), t1, t2)

    case IMPSQCSubset(e1,e2) =>
      val ca = findSortFromContext(e1,cntxt) ; val cb = findSortFromContext(e2,cntxt)
      val as : IMPSSort = ca.getOrElse(cb.getOrElse(IMPSUnknownSort(tState.freshHash())))
      val t1 = doMathExp(e1,thy,cntxt) ; val t2 = doMathExp(e2,thy,cntxt)

      IMPSTheory.QCT.subsetQC(findKind(as), matchSort(as,thy), t1, t2)

    case IMPSQCEmptyIndicator(srt) =>
      val ca = findSortFromContext(srt,cntxt)
      val as : IMPSSort = ca.getOrElse(IMPSUnknownSort(tState.freshHash()))
      IMPSTheory.QCT.emptyIndicQC(findKind(as),matchSort(as,thy))

    case IMPSQCEmptyIndicatorQ(ind) =>
      val ca = findSortFromContext(ind,cntxt)
      val as : IMPSSort = ca.getOrElse(IMPSUnknownSort(tState.freshHash()))

      assert(as.isInstanceOf[IMPSUnknownSort]   || as.isInstanceOf[IMPSSetSort]
          || as.isInstanceOf[IMPSBinaryFunSort] || as.isInstanceOf[IMPSNaryFunSort])

      val srt : IMPSSort = as match {
        case IMPSSetSort(s)           => s
        case IMPSBinaryFunSort(s1,s2) => hAssert(s2 == IMPSAtomSort("unit%sort") || s2 == IMPSAtomSort("unitsort"), s2) ; s1
        case IMPSNaryFunSort(srts)    => assert(srts.last == IMPSAtomSort("unit%sort") || srts.last == IMPSAtomSort("unitsort"))
                                         assert(srts.length == 2)
                                         srts.head
        case IMPSUnknownSort(_)       => IMPSUnknownSort(tState.freshHash())
      }

      val t1 = doMathExp(ind,thy,cntxt)

      IMPSTheory.QCT.emptyIndicQQC(findKind(srt),matchSort(srt,thy),t1)

    case IMPSQCNonemptyIndicator(a) =>
      val ca = findSortFromContext(a,cntxt)
      val as : IMPSSort = ca.getOrElse(IMPSUnknownSort(tState.freshHash()))

      assert(as.isInstanceOf[IMPSUnknownSort]   || as.isInstanceOf[IMPSSetSort]
        || as.isInstanceOf[IMPSBinaryFunSort] || as.isInstanceOf[IMPSNaryFunSort])

      val srt : IMPSSort = as match {
        case IMPSSetSort(s)           => s
        case IMPSBinaryFunSort(s1,s2) => hAssert(s2 == IMPSAtomSort("unit%sort") || s2 == IMPSAtomSort("unitsort"), s2) ; s1
        case IMPSNaryFunSort(srts)    => assert(srts.last == IMPSAtomSort("unit%sort") || srts.last == IMPSAtomSort("unitsort"))
          assert(srts.length == 2)
          srts.head
        case IMPSUnknownSort(_)       => IMPSUnknownSort(tState.freshHash())
      }

      val t1 = doMathExp(a,thy,cntxt)

      IMPSTheory.QCT.nonEmptyIndicQQC(findKind(srt),matchSort(srt,thy),t1)

    case IMPSQCComplement(m) =>
      val ca = findSortFromContext(m,cntxt)
      val as : IMPSSort = ca.getOrElse(IMPSUnknownSort(tState.freshHash()))
      val t1 = doMathExp(m,thy,cntxt)

      IMPSTheory.QCT.complementQC(findKind(as),matchSort(as,thy),t1)

    case IMPSQCUnion(e1,e2) =>
      val t1 = doMathExp(e1,thy,cntxt) ; val t2 = doMathExp(e2,thy,cntxt)
      IMPSTheory.QCT.unionQC(tState.doUnknown(), tState.doUnknown(), t1, t2)

    case IMPSQCIntersection(e1,e2) =>
      val t1 = doMathExp(e1,thy,cntxt) ; val t2 = doMathExp(e2,thy,cntxt)
      IMPSTheory.QCT.intersectionQC(tState.doUnknown(), tState.doUnknown(), t1, t2)

    case IMPSQCDifference(e1,e2) =>
      val t1 = doMathExp(e1,thy,cntxt) ; val t2 = doMathExp(e2,thy,cntxt)
      IMPSTheory.QCT.differenceQC(tState.doUnknown(), tState.doUnknown(), t1, t2)

    case IMPSQCSymDifference(e1,e2) =>
      val t1 = doMathExp(e1,thy,cntxt) ; val t2 = doMathExp(e2,thy,cntxt)
      IMPSTheory.QCT.symDifferenceQC(tState.doUnknown(), tState.doUnknown(), t1, t2)

    case IMPSQCDisjoint(e1,e2) =>
      val t1 = doMathExp(e1,thy,cntxt) ; val t2 = doMathExp(e2,thy,cntxt)
      IMPSTheory.QCT.disjointQC(tState.doUnknown(), tState.doUnknown(), t1, t2)

    case IMPSQCPartitionQ(e1,e2) =>
      val t1 = doMathExp(e1,thy,cntxt) ; val t2 = doMathExp(e2,thy,cntxt)
      IMPSTheory.QCT.partitionQQC(tState.doUnknown(), tState.doUnknown(), t1, t2)

    case IMPSQCSingleton(m) =>
      val ca = findSortFromContext(m, cntxt)
      val as: IMPSSort = ca.getOrElse(IMPSUnknownSort(tState.freshHash()))
      val t1 = doMathExp(m, thy, cntxt)

      IMPSTheory.QCT.singletonQC(findKind(as), matchSort(as, thy), t1)

    case IMPSQCBigUnion(f) =>
      val fp : Term = doMathExp(f,thy,cntxt)
      IMPSTheory.QCT.bigUnionQC(tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),fp)

    case IMPSQCBigIntersection(f) =>
      val fp : Term = doMathExp(f,thy,cntxt)
      IMPSTheory.QCT.bigIntersectionQC(tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),fp)

    /* QCs from mappings.t */

    case IMPSQCMDomain(f) =>
      val fp : Term = doMathExp(f,thy,cntxt)
      IMPSTheory.QCT.mdomainQC(tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),fp)

    case IMPSQCMRange(f) =>
      val fp : Term = doMathExp(f,thy,cntxt)
      IMPSTheory.QCT.mrangeQC(tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),fp)

    case IMPSQCMComposition(f,g) =>
      val fp : Term = doMathExp(f,thy,cntxt)
      val gp : Term = doMathExp(g,thy,cntxt)
      IMPSTheory.QCT.mcompositionQC(tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),fp,gp)

    case IMPSQCMImage(f,s) =>
      val fp : Term = doMathExp(f,thy,cntxt)
      val sp : Term = doMathExp(s,thy,cntxt)
      IMPSTheory.QCT.mimageQC(tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),fp,sp)

    case IMPSQCMInverseImage(f,s) =>
      val fp : Term = doMathExp(f,thy,cntxt)
      val sp : Term = doMathExp(s,thy,cntxt)
      IMPSTheory.QCT.minverseimageQC(tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),fp,sp)

    case IMPSQCMInverse(f) =>
      val fp : Term = doMathExp(f,thy,cntxt)
      IMPSTheory.QCT.minverseQC(tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),fp)

    case IMPSQCMId(s) =>
      val sp : Term = doMathExp(s,thy,cntxt)
      IMPSTheory.QCT.midQC(tState.doUnknown(), tState.doUnknown(), sp)

    case IMPSQCMRestrict(f,s) =>
      val fp : Term = doMathExp(f,thy,cntxt)
      val sp : Term = doMathExp(s,thy,cntxt)
      IMPSTheory.QCT.mrestrictQC(tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),fp,sp)

    case IMPSQCMRestrict2(f,s,t) =>
      val fp : Term = doMathExp(f,thy,cntxt)
      val sp : Term = doMathExp(s,thy,cntxt)
      val tp : Term = doMathExp(t,thy,cntxt)
      IMPSTheory.QCT.mrestrict2QC(tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),fp,sp,tp)

    case IMPSQCMSurjective(f) =>
      val fp : Term = doMathExp(f,thy,cntxt)
      IMPSTheory.QCT.msurjectiveQQC(tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),fp)

    case IMPSQCMInjective(f) =>
      val fp : Term = doMathExp(f,thy,cntxt)
      IMPSTheory.QCT.minjectiveQQC(tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),fp)

    case IMPSQCMBijective(f) =>
      val fp : Term = doMathExp(f,thy,cntxt)
      IMPSTheory.QCT.mbijectiveQQC(tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),fp)

    case IMPSQCMSurjectiveOn(f,as,bs) =>
      val fp  : Term = doMathExp(f,thy,cntxt)
      val asp : Term = doMathExp(as,thy,cntxt)
      val bsp : Term = doMathExp(bs,thy,cntxt)

      IMPSTheory.QCT.msurjectiveonQQC(tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),fp,asp,bsp)

    case IMPSQCMInjectiveOn(f,as) =>
      val fp  : Term = doMathExp(f,thy,cntxt)
      val asp : Term = doMathExp(as,thy,cntxt)

      IMPSTheory.QCT.minjectiveonQQC(tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),fp,asp)

    case IMPSQCMBijectiveOn(f,as,bs) =>
      val fp  : Term = doMathExp(f,thy,cntxt)
      val asp : Term = doMathExp(as,thy,cntxt)
      val bsp : Term = doMathExp(bs,thy,cntxt)

      IMPSTheory.QCT.mbijectiveonQQC(tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),fp,asp,bsp)

    /* QCs from groups.t */

    case IMPSQCGroups(m,mul,e,inv) =>
      val g_t : Term = doMathExp(m,thy,cntxt)
      val m_t : Term = doMathExp(mul,thy,cntxt)
      val e_t : Term = doMathExp(e,thy,cntxt)
      val i_t : Term = doMathExp(inv,thy,cntxt)

      IMPSTheory.QCT.groupsQC(tState.doUnknown(),tState.doUnknown(),g_t,m_t,e_t,i_t)

    /* QCs from cardinality.t */

    case IMPSQCEquinumerous(p,q) =>
      val p_t : Term = doMathExp(p,thy,cntxt)
      val q_t : Term = doMathExp(q,thy,cntxt)

      IMPSTheory.QCT.equinumerousQC(tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),p_t,q_t)

    case IMPSQCEmbeds(p,q) =>
      val p_t : Term = doMathExp(p,thy,cntxt)
      val q_t : Term = doMathExp(q,thy,cntxt)

      IMPSTheory.QCT.embedsQC(tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),p_t,q_t)

    /* QCs from covers.t */

    case IMPSQCCountableCover(f,a) =>
      val f_t : Term = doMathExp(f,thy,cntxt)
      val a_t : Term = doMathExp(a,thy,cntxt)

      val r = getTheory("h-o-real-arithmetic")
      val zz : Term = matchSort(IMPSAtomSort("zz"), r)

      IMPSTheory.QCT.countableCoverQC(OMS(IMPSTheory.lutinsIndType), tState.doUnknown(), zz, tState.doUnknown(), f_t, a_t)

    case IMPSQCFiniteCover(f,a) =>
      val f_t : Term = doMathExp(f,thy,cntxt)
      val a_t : Term = doMathExp(a,thy,cntxt)

      val r = getTheory("h-o-real-arithmetic")
      val zz : Term = matchSort(IMPSAtomSort("zz"), r)

      val minus : Constant = getConstant("-",r)
      val leq   : Constant = getConstant("<=", r)

      IMPSTheory.QCT.finiteCoverQC(OMS(IMPSTheory.lutinsIndType), tState.doUnknown(), zz, tState.doUnknown(), minus.toTerm, leq.toTerm, f_t, a_t)

    /* QCs from finite-cardinality.t */

    case IMPSQCFinCard(a) =>
      val a_t : Term = doMathExp(a,thy,cntxt)

      val r = getTheory("h-o-real-arithmetic")
      val nn : Term = matchSort(IMPSAtomSort("nn"), r)
      val  w : Term = getConstant("omega",r).toTerm

      IMPSTheory.QCT.finiteCardinalityQC(tState.doUnknown(),tState.doUnknown(),nn,w,a_t)

    case IMPSQCFinIndic(i) =>
      val i_t : Term = doMathExp(i,thy,cntxt)

      val r = getTheory("h-o-real-arithmetic")
      val nn : Term = matchSort(IMPSAtomSort("nn"), r)
      val  w : Term = getConstant("omega",r).toTerm

      IMPSTheory.QCT.finiteIndicatorQC(tState.doUnknown(),tState.doUnknown(),nn,w,i_t)

    case IMPSQCFinSort(s) =>
      val s_t : Term = matchSort(s,thy)

      val r = getTheory("h-o-real-arithmetic")
      val nn : Term = matchSort(IMPSAtomSort("nn"), r)
      val  w : Term = getConstant("omega",r).toTerm

      IMPSTheory.QCT.finiteSortQC(tState.doUnknown(),tState.doUnknown(),nn,w,s_t)

    /* QCs from iterate-supplements.t */

    case IMPSQCInvariant(a,f) =>
      val a_t : Term = doMathExp(a,thy,cntxt)
      val f_t : Term = doMathExp(f,thy,cntxt)

      IMPSTheory.QCT.invariantQC(tState.doUnknown(),tState.doUnknown(),a_t,f_t)

    /* QCs from pairs.t */

    case IMPSQCPair(p,q) =>
      val p_t : Term = doMathExp(p,thy,cntxt)
      val q_t : Term = doMathExp(q,thy,cntxt)

      IMPSTheory.QCT.pairQC(tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),p_t,q_t)

    case IMPSQCPairQ(p) =>
      val p_t : Term = doMathExp(p,thy,cntxt)
      IMPSTheory.QCT.pairQQC(tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),p_t)

    case IMPSQCFirst(p) =>
      val p_t : Term = doMathExp(p,thy,cntxt)
      IMPSTheory.QCT.firstQC(tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),p_t)

    case IMPSQCSecond(p) =>
      val p_t : Term = doMathExp(p,thy,cntxt)
      IMPSTheory.QCT.secondQC(tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),p_t)

    // Crossproduct QC defunct

    case IMPSQCLength(l) =>
      val l_t : Term = doMathExp(l,thy,cntxt)
      val r = getTheory("h-o-real-arithmetic")
      val nn : Term = matchSort(IMPSAtomSort("nn"), r)
      val leq : Term = getConstant("<=",r).toTerm
      IMPSTheory.QCT.lengthQC(tState.doUnknown(),tState.doUnknown(),nn,leq,l_t)

    case IMPSQCFseqQ(l) =>
      val l_t : Term = doMathExp(l,thy,cntxt)
      val r = getTheory("h-o-real-arithmetic")
      val nn : Term = matchSort(IMPSAtomSort("nn"), r)
      val leq : Term = getConstant("<=",r).toTerm
      IMPSTheory.QCT.fSeqQQC(tState.doUnknown(),tState.doUnknown(),nn,leq,l_t)

    case IMPSQCNil(l) =>
      val l_t : Term = doMathExp(l,thy,cntxt)
      val r = getTheory("h-o-real-arithmetic")
      val nn : Term = matchSort(IMPSAtomSort("nn"), r)
      IMPSTheory.QCT.nilQC(tState.doUnknown(),tState.doUnknown(),nn,l_t)

    case IMPSQCCons(e,l) =>
      val l_t : Term = doMathExp(l,thy,cntxt)
      val e_t : Term = doMathExp(e,thy,cntxt)
      val r = getTheory("h-o-real-arithmetic")
      val nn : Term = matchSort(IMPSAtomSort("nn"), r)
      val leq : Term = getConstant("<=",r).toTerm
      val minus : Term = getConstant("-",r).toTerm
      val one : Term = doLiteral("1")
      IMPSTheory.QCT.consQC(tState.doUnknown(),tState.doUnknown(),nn,leq,minus,one,e_t,l_t)

    case IMPSQCDrop(e,l) =>
      val l_t : Term = doMathExp(l,thy,cntxt)
      val e_t : Term = doMathExp(e,thy,cntxt)
      val r = getTheory("h-o-real-arithmetic")
      val nn : Term = matchSort(IMPSAtomSort("nn"), r)
      val plus : Term = getConstant("+",r).toTerm
      IMPSTheory.QCT.dropQC(tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),nn,plus,e_t,l_t)

    case IMPSQCAppend(l1,l2) =>
      val l1_t : Term = doMathExp(l1,thy,cntxt)
      val l2_t : Term = doMathExp(l2,thy,cntxt)
      val r = getTheory("h-o-real-arithmetic")
      val nn : Term = matchSort(IMPSAtomSort("nn"), r)
      val minus : Term = getConstant("-",r).toTerm
      val leq : Term = getConstant("<=",r).toTerm
      IMPSTheory.QCT.appendQC(tState.doUnknown(),tState.doUnknown(),nn,leq,minus,l1_t,l2_t)

    case IMPSQCTakeFirst(l,n) =>
      val l_t : Term = doMathExp(l,thy,cntxt)
      val n_t : Term = doMathExp(n,thy,cntxt)
      val r = getTheory("h-o-real-arithmetic")
      val nn : Term = matchSort(IMPSAtomSort("nn"), r)
      val leq : Term = getConstant("<=",r).toTerm
      IMPSTheory.QCT.takeFirstQC(tState.doUnknown(),tState.doUnknown(),tState.doUnknown(),nn,leq,l_t,n_t)

    case IMPSQCInSeq(l1,l2) =>
      val l1_t : Term = doMathExp(l1,thy,cntxt)
      val l2_t : Term = doMathExp(l2,thy,cntxt)
      val r = getTheory("h-o-real-arithmetic")
      val nn : Term = matchSort(IMPSAtomSort("nn"), r)
      IMPSTheory.QCT.inSeqQC(tState.doUnknown(),tState.doUnknown(),nn,l1_t,l2_t)

    case _ => println(" > Error: Unknown Quasi-Constructor!") ; ??!(d)
  }

  def getTheory(name : String) : Theory =
  {
    var thy = tState.theories_decl.find(t => t.name.toString.toLowerCase == name.toLowerCase)
    if (thy.isEmpty) { tState.theories_decl.find(t => t.name.toString.toLowerCase == name.toLowerCase + "_ensemble") }
    if (thy.isEmpty) { ??!(name) }
    assert(thy.isDefined)
    thy.get
  }

  def getConstant(name : String, thy : Theory) : Constant =
  {
    val con1 = thy.getConstants.find(c => c.name.toString.toLowerCase == name.toLowerCase)
    val con2 = thy.getConstants.find(c => c.alias.map(_.toString.toLowerCase).contains(name))
    if (con1.isDefined) { con1.get }
    else if (con2.isDefined) { con2.get }
    else {
      val foundTheory = locateMathSymbolHome(name, thy)
      getConstant(name, foundTheory.get)
    }
  }

  def recursiveIncludes(ts : List[Theory]) : List[Theory] =
  {
    var is : List[Theory] = ts

    for (t <- ts)
    {
      val recs = recursiveIncludes(t.getIncludesWithoutMeta.map(m => controller.getTheory(m)))
      is = is ::: recs
    }

    is.distinct
  }

  def locateMathSymbolHome(s : String, thy : Theory) : Option[Theory] =
  {
    def cmatch(c : Constant, s : String) : Boolean = {
      c.alias.map(_.toString.toLowerCase).contains(s.toLowerCase) || (c.name.toString.toLowerCase == s.toLowerCase)
    }

    var multipleCandidates : List[Theory] = List.empty
    if (tState.ensembles.exists(te => te.name.toLowerCase == thy.name.toString.toLowerCase))
    {
      val ensemble = tState.ensembles.find(te => te.name.toLowerCase == thy.name.toString.toLowerCase).get
      multipleCandidates = recursiveIncludes(ensemble.multipleMap.values.toList).reverse
    }

    val candidates : List[Theory]   = (recursiveIncludes(List(thy)).reverse ::: multipleCandidates).distinct
    val srcthy     : Option[Theory] = candidates.find(t => t.getConstants.exists(c => cmatch(c,s)))
    if (tState.verbosity > 3)
    {
      println(" > Locating IMPSMathSymbol " + s + " for use in theory " + thy.name.toString)
    }

    if (srcthy.isEmpty) {
      println(" >>> location for " + s + " could not be found, starting from " + thy.name)
    }

    srcthy
  }

  def findConstant(name : String, thy : Theory) : Constant =
  {
    val needle : String = name.toLowerCase
    val const = thy.getConstants.find(c => c.name.toString.toLowerCase == needle || c.alias.map(_.toString.toLowerCase).contains(needle))
    assert(const.isDefined)
    const.get
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
      case _ => if (context.map(_._1).contains(exp)) { Some(context.find(k => k._1 == exp).get._2) } else { None }
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

  def doIMPSLambda(lambda : IMPSLambda, thy : Theory, cntxt : List[(IMPSVar,IMPSSort)]) : Term =
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

  def doIMPSForsome(forsome : IMPSForSome, thy : Theory, cntxt : List[(IMPSVar,IMPSSort)]) : Term =
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

  def doIMPSForall(forall : IMPSForAll, thy : Theory, cntxt : List[(IMPSVar,IMPSSort)]) : Term =
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

