package info.kwarc.mmt.imps

import info.kwarc.mmt.api._

import scala.io.Source
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.checking.{Checker, CheckingEnvironment, MMTStructureChecker, RelationHandler}
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.frontend.{Logger, Report}
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.lf.Typed
import info.kwarc.mmt.api.utils.JSON

class IMPSImporter extends Importer
{
  val key: String = "imps-omdoc"

  def inExts = List("index")

  def importDocument(bf: BuildTask, index: Document => Unit): BuildResult =
  {
    val tState : TranslationState = new TranslationState()
    val targetSection : Section = impsLibrarySections.impsMathLibrary

    log("Reading index file: " + bf.inFile.getName, log_structure)
    log("== BUILDING DEPENDENCY TREE ==",           log_structure)
    log("Target section: " + targetSection.name,    log_structure)

    var readingT : List[String] = Nil
    var readingJ : List[String] = Nil

    def importSection(t : Section, n : Int) : Unit =
    {
      val indent : String = "  " * n  // This syntax makes me shiver!

      log(indent + "> " + t.name, log_details)
      for (f <- t.files) { log(indent + "  | " + f, log_details) }

      readingT = t.files ::: readingT
      readingJ = t.jsons ::: readingJ

      for (s <- t.dependencies) { importSection(s,n+1) }
    }

    importSection(targetSection,0)

    readingJ = readingJ.distinct
    readingT = readingT.distinct

    log("== DEPENDECIES CLEAR ; BEGINNING JSON PARSING ==", log_structure)

    val jsonfiles = bf.inFile.up.canonical.listFiles.filter(_.getName.endsWith(".json"))
    var parsed_json : List[JSONObject] = Nil
    val translatejsonFiles = readingJ.map(fn => {
      val foo = jsonfiles.find(p => p.getName == fn)
      if (foo.isEmpty) { logError(" ERROR: JSON NOT FOUND: " + fn) }
      foo.get
    })

    assert(translatejsonFiles.length == readingJ.length)

    var json_report : String = ""
    for (rj <- readingJ)
    {
      json_report += (if (translatejsonFiles.exists(f => f.getName == rj)) { "✓ " } else { "  " })
      json_report += rj + "\n"
    }
    json_report += "\n"
    log(json_report, log_details)

    log("# Reading json files. This may take a while...", log_structure)
    for (file <- translatejsonFiles)
    {
      log("# Reading json file: " + file,log_details)

      // Prevent leakage of underlying file handle.
      val json_source = Source.fromFile(file)

      try {
        val fileLines = json_source.getLines()
        var contents: String = ""
        for (line <- fileLines) {
          contents = contents + line + "\n"
        }

        val j : JSONObject = JSON.parse(contents).asInstanceOf[JSONObject]
        parsed_json = parsed_json.::(j)
      } finally {
        json_source.close()
      }

    }
    tState.jsons = parsed_json

    log("== JSON PARSING COMPLETE ; BEGINNING T PARSING ==", log_structure)

    val tfiles    = bf.inFile.up.canonical.listFiles.filter(_.getName.endsWith(".t")).toList
    val translateFiles = readingT.map(fn => {
      val foo = tfiles.find(p => p.getName == fn)
      if (foo.isEmpty) { logError("ERROR: T NOT FOUND: " + fn) }
      foo.get
    })


    var imps_report : String = ""
    for (rt <- readingT)
    {
      imps_report += (if (translateFiles.exists(f => f.getName == rt)) { "✓ " } else { "  " })
      imps_report += rt + "\n"
    }
    imps_report += "\n"
    log(imps_report,log_details)

    assert(translateFiles.length == readingT.length)

    var parsed_t : List[(List[DefForm], URI)] = Nil

    log("Reading imps files!", log_structure)
    for (file <- translateFiles)
    {
      log("Reading imps file: " + file, log_details)

      val e : List[DefForm] = try
      {
        val imps_source = Source.fromFile(file)
        try {
          val contents = imps_source.mkString
          val nlp : NEWIMPSParser = new NEWIMPSParser()
          val res = nlp.parse(contents, FileURI(file), parsed_json)
          Source.fromFile(file).close()
          res
        } finally {
          imps_source.close()
        }
      } catch {
        case e : IMPSDependencyException =>
          logError("Failure: " + e.getMessage)
          sys.exit()

        case e: ExtractError =>
          log(e.getMessage)
          sys.exit()
      }

      val weight = Math.round(e.toString().length / 100.0) / 10.0
      log("Done! Succesfully parsed " + e.length.toString + " def-forms with a weight of " + weight + "K in " + file.getName, log_specifics)
      parsed_t = parsed_t ::: List((e,FileURI(file)))
    }

    log("== PARSING COMPLETE ; BEGINNING T TRANSLATION ==", log_structure)

    val doc = new Document(IMPSImportTask.docpath, FileLevel)
    controller.add(doc)

    val importTask = new IMPSImportTask(controller, bf, tState,doc, index)

    // We fake a few things for the kernel theory since it's not translated from a file.
    val fakeURI : URI = URI(bf.inFile.getParentFile.getParentFile.getAbsolutePath + "/the-kernel-theory.t")
    val fakeexp : List[DefForm] = List(theKernelLang,theKernelTheory,unitSortTheorem)

    val len = parsed_t.length + 1
    var run = 1

    log("########## (1/" + len + ") Translating: " + fakeURI, log_structure)
    importTask.doDocument(fakeexp,fakeURI)
    log("Success", log_structure)

    for (e <- parsed_t)
    {
      run += 1
      log("########## (" + run + "/" + len + ") Translating: " + e._2, log_structure)

      try {
        importTask.doDocument(e._1, e._2)
        log("Success", log_structure)
      }
      catch {
        case e : IMPSDependencyException => logError("Failure! " + e.getMessage); sys.exit()
      }
    }

    if (tState.delayed.nonEmpty || tState.delayedTrans.nonEmpty) {
      log("Principle translation finished, starting translation of delayed def-forms.", log_structure)

      for (d <- tState.delayed) {
        importTask.doDeclaration(d._1,d._2)
      }

      log("Starting translation of delayed translations.", log_structure)
      for (d <- tState.delayedTrans) {
        importTask.doTranslation(d._1,d._2,d._3)
      }
    }

    // Run Checker (to resolve unknowns, etc)
    // Set to true to run
    val typecheck : Boolean = false

    if (typecheck) {
      log("Translation complete! Starting type-checking.", log_structure)
    } else {
      log("Translation complete! Type-checking disabled, skipping.", log_structure)
    }

    if (typecheck)
    {
      log("Checking:")
      logGroup
      {
        val checker = controller.extman.get(classOf[Checker], format = "mmt").getOrElse {
          throw GeneralError("no checker found")
        }.asInstanceOf[MMTStructureChecker]
        tState.theories_decl foreach { p =>
          val ce = new CheckingEnvironment(controller.simplifier,new ErrorLogger(report),RelationHandler.ignore,importTask)
          checker.apply(p)(ce)
        }
      }
    }
    index(doc)
    log("Translation process imps-omdoc complete!",log_structure)
    BuildSuccess(Nil, Nil)
  }

  val theKernelLang : DFLanguage = DFLanguage(
    Name("THE-KERNEL-LANGUAGE",None,None),
    None,
    None,
    Some(ArgBaseTypes(List(IMPSAtomSort("ind"),IMPSAtomSort("prop"),IMPSAtomSort("unit%sort")),None,None)),
    Some(ArgSorts(List(ArgSortSpec(IMPSAtomSort("ind"),IMPSAtomSort("ind"),None,None),
      ArgSortSpec(IMPSAtomSort("prop"),IMPSAtomSort("prop"),None,None),
      ArgSortSpec(IMPSAtomSort("unit%sort"),IMPSAtomSort("unit%sort"),None,None)),None,None)),
    None,
    Some(ArgConstants(List(ArgConstantSpec(Name("an%individual",None,None),IMPSAtomSort("unit%sort"),None,None)),None,None)),
    None,
    None
  )

  val theKernelTheory : DFTheory = DFTheory(
    Name("the-kernel-theory",None,None),
    Some(ArgLanguage(Name("THE-KERNEL-LANGUAGE",None,None),None,None)),
    None,
    Some(ArgAxioms(List(AxiomSpec(
      Some("unit-sort-defining-axiom"),
      DefString("forall(x,y:unit%sort,x=y iff truth",None,None),
      Some(IMPSForAll(List((IMPSVar("z"),IMPSAtomSort("unit%sort"))),IMPSEquals(IMPSVar("z"),IMPSMathSymbol("an%individual")))),
      None,
      None,None)),None,None)),
    None,
    None,
    None
  )

  val unitSortTheorem : DFTheorem = {

    val frm = Some(IMPSForAll(List((IMPSVar("x"),IMPSAtomSort("unit%sort")),(IMPSVar("y"),IMPSAtomSort("unit%sort"))),
      IMPSIff(IMPSEquals(IMPSVar("x"),IMPSVar("y")),IMPSTruth())))

    DFTheorem(
      Name("()",None,None),
      ODefString(scala.util.Left((DefString("forall(x:unit%sort,x=an%individual)",None,None),frm)),None,None),
      frm,
      None,
      None,
      ArgTheory(Name("the-kernel-theory",None,None),None,None),
      Some(ArgUsages(List(Usage.ELEMENTARYMACETE),None,None)),
      None,
      None,
      None,
      None,
      None,
      None
    )
  }
}

class NEWIMPSParser extends Logger
{
  override def report    : Report = new Report()
  override def logPrefix : String = "imps-parsing"

  def parse(s: String, uri : URI, js : List[JSONObject]) : List[DefForm]
    = parse(new Unparsed(s, msg => throw GeneralError(msg)), uri, js)

  def parse(u : Unparsed, uri : URI, js : List[JSONObject]) : List[DefForm] =
  {
    val dfp = new DefFormParsers(js)
    val foo  = ParserWithSourcePosition.parseAll(dfp.parseImpsSource,u)
    if (!foo.successful) { logError("### Parsing Error near: Line " + u.pos.line + " Column" + u.pos.column) }
    assert(foo.successful)

    val dfs : List[DefForm] = foo.get
    dfs.foreach(_.updateSource(uri))
    dfs
  }
}

class TheoryEnsemble(nm : String, base : Theory, fixed : List[Theory], renamer: Int => String => String)
{
  val name           : String = nm
  val baseTheory     : Theory = base
  val fixedTheories  : List[Theory] = fixed
  val replicaRenamer : Int => String => String = renamer

  var replicaMap     : Map[Int,Theory] = Map.empty
  var multipleMap    : Map[Int,Theory] = Map.empty

  var interpretationsMap : Map[(Int,Int),View] = Map.empty
}

/* Some things are convenient to carry around in state.
   See also: This exact thing, but in PVS */
class TranslationState ()
{
  var theories_raw       : List[DFTheory]       = Nil
  var theories_decl      : List[Theory]         = Nil
  var ensembles          : List[TheoryEnsemble] = Nil

  var languages_raw      : List[DFLanguage]     = Nil
  var languages_decl     : List[Theory]         = Nil

  var translations_raw   : List[DFTranslation]  = Nil
  var translations_decl  : List[View]           = Nil

  /* Has the trivial renamer preinstalled */
  var renamers           : List[DFRenamer]      = List(DFRenamer(Name("identity",None,None),None,None,None))

  var delayed            : List[(DefForm,URI)]              = Nil
  var delayedTrans       : List[(DFTranslation,DPath,URI)]  = Nil


  var jsons              : List[JSONObject]     = Nil

  var memoised_homes     : Map[(String,Theory),Theory] = Map.empty
  var supersorts         : Map[Term,List[Term]]        = Map.empty
  var knownsubtyperules  : Set[LocalName]              = Set.empty

  var nativeConstants    : Map[LocalName, Set[String]] = Map.empty

  var vars               : Context              = Context.empty
  var knownUnknowns      : List[(Int,Term)]     = Nil
  var hashCount          : Int                  = 0

  protected var unknowns : Int                  = 0

  protected def doiName(i : Int, isType : Boolean) : LocalName = {
    LocalName("") / { if (isType) LocalName("I") else LocalName("i") } / i.toString
  }

  def hashes() : List[Int] = knownUnknowns.map(_._1)

  def doUnknown(h : Option[Int] = None) : Term = {
    if (h.isDefined)
    {
      if (hashes().contains(h.get)) {
        knownUnknowns.find(p => p._1 == h.get).get._2
      } else {
        val trm : Term = OMV(doiName({unknowns+=1;unknowns-1},false))
        knownUnknowns = knownUnknowns ::: List((h.get,trm))
        trm
      }
    }
    else
    {
      OMV(doiName({unknowns+=1;unknowns-1},false))
    }
  }

  def bindUnknowns(t : Term) : Term = {
    val symbs = t.freeVars.collect{
      case ln if ln.toString.startsWith("""/i/""") => ln
    }
    val cont = symbs.flatMap(n => {
      val i = (0 until unknowns).find(j => n == doiName(j,isType = false))/*.getOrElse(
          throw new Exception("Wrong free Variable: " + n + " in " + t)
        )*/
      if (i.isDefined)
        List(VarDecl(doiName(i.get,isType = true), OMS(Typed.ktype)), VarDecl(n, OMV(doiName(i.get,isType = true))))
      else throw GeneralError("No unknown " + n)
    })
    if (unknowns > 0 && cont.nonEmpty) OMBIND(OMS(Path.parseS("http://cds.omdoc.org/mmt?mmt?unknown", NamespaceMap.empty)),
      cont,
      t)
    else t
  }

  def resetUnknowns() : Unit =
  {
    unknowns = 0
    knownUnknowns = Nil
    vars = Context.empty
  }

  def freshHash() : Int =
  {
    hashCount += 1
    hashCount.toString.hashCode()
  }

  def allSupersorts(sub : Term) : List[Term] = {
    var list  = List(sub)
    var delta = true
    while (delta) {
      val tmplst : List[Term] = list ::: list.flatMap(e => supersorts.getOrElse(e,Nil))
      delta = list.distinct != tmplst.distinct
      list  = tmplst
    }
    list.distinct.filter(e => e != sub)
  }
}
