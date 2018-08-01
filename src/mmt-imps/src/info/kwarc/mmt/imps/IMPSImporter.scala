package info.kwarc.mmt.imps

import info.kwarc.mmt.api._

import scala.io.Source
import scala.util.Either
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.lf.Typed
import info.kwarc.mmt.api.utils.JSON

import info.kwarc.mmt.imps.impsLibrarySections.allSections

class IMPSImporter extends Importer
{
  val key: String = "imps-omdoc"

  def inExts = List("index")

  def importDocument(bf: BuildTask, index: Document => Unit): BuildResult =
  {
    val tState : TranslationState = new TranslationState()
    tState.verbosity = 1
    val targetSection : Section = impsLibrarySections.foundation

    if (tState.verbosity > 0)
    {
      println("\nReading index file: " + bf.inFile.getName)
      println("\n== BUILDING DEPENDENCY TREE ==\n")
      println("Target section: " + targetSection.name + "\n")
    }

    var readingT : List[String] = Nil
    var readingJ : List[String] = Nil

    def importSection(t : Section, n : Int) : Unit =
    {
      val indent : String = "  " * n  // This syntax makes me shiver!

      if (tState.verbosity > 1)
      {
        println(indent + "> " + t.name)
        for (f <- t.files) { println(indent + "  | " + f) }
      }

      readingT = t.files ::: readingT
      readingJ = t.jsons ::: readingJ

      for (s <- t.dependencies) { importSection(s,(n+1)) }
    }

    importSection(targetSection,0)

    readingJ = readingJ.distinct
    readingT = readingT.distinct

    if (tState.verbosity > 0)
    {
      println("\n== DEPENDECIES CLEAR ; BEGINNING JSON PARSING ==\n")
    }

    val jsonfiles = bf.inFile.up.canonical.listFiles.filter(_.getName.endsWith(".json"))
    var parsed_json : List[JSONObject] = Nil
    val translatejsonFiles = readingJ.map(fn => {
      val foo = jsonfiles.find(p => p.getName == fn)
      if (foo.isEmpty) { println(" ERROR: JSON NOT FOUND: " + fn) }
      foo.get
    })

    assert(translatejsonFiles.length == readingJ.length)

    if (tState.verbosity > 0)
    {
      for (rj <- readingJ)
      {
        if (translatejsonFiles.find(f => f.getName == rj).isDefined) { print("✓ ") } else { print("  ") }
        println(rj)
      }
      println("")
    }

    for (file <- translatejsonFiles)
    {
      if (tState.verbosity > 0)
      {
        println("# Reading json file: " + file)
      }

      val fileLines = Source.fromFile(file).getLines
      var contents: String = ""
      for (line <- fileLines) {
        contents = contents + line + "\n"
      }

      val j : JSONObject = JSON.parse(contents).asInstanceOf[JSONObject]
      parsed_json = parsed_json.::(j)

    }
    tState.jsons = parsed_json

    if (tState.verbosity > 0)
    {
      println("\n== JSON PARSING COMPLETE ; BEGINNING T PARSING ==\n")
    }

    val tfiles    = bf.inFile.up.canonical.listFiles.filter(_.getName.endsWith(".t")).toList
    val translateFiles = readingT.map(fn => {
      val foo = tfiles.find(p => p.getName == fn)
      if (foo.isEmpty) { println(" ERROR: T NOT FOUND: " + fn) }
      foo.get
    })

    if (tState.verbosity > 0)
    {
      for (rt <- readingT)
      {
        if (translateFiles.find(f => f.getName == rt).isDefined) { print("✓ ") } else { print("  ") }
        println(rt)
      }
    }

    assert(translateFiles.length == readingT.length)

    var parsed_t : List[(List[DefForm], URI)] = Nil

    for (file <- translateFiles)
    {
      if (tState.verbosity > 0)
      {
        println("\n###########\nReading imps file: " + file)
      }

      val e : List[DefForm] = try
      {
        val contents = Source.fromFile(file).mkString

        val nlp : NEWIMPSParser = new NEWIMPSParser()
        val res = nlp.parse(contents, FileURI(file), parsed_json)
        res
      } catch {
        case e : IMPSDependencyException => {
          println(" > Failure: " + e.getMessage)
          sys.exit
        }
        case e: ExtractError => {
          log(e.getMessage)
          sys.exit
        }
      }
      parsed_t = parsed_t ::: List((e,FileURI(file)))
    }

    if (tState.verbosity > 0)
    {
      println("\n== PARSING COMPLETE ; BEGINNING T TRANSLATION ==\n")
    }

    val importTask = new IMPSImportTask(controller, bf, index, tState)

    val fakeURI : URI = URI(bf.inFile.getParentFile.getParentFile.getAbsolutePath + "/the-kernel-theory.t")
    val fakeexp : List[DefForm] = List(theKernelLang,theKernelTheory,unitSortTheorem)

    if (tState.verbosity > 0)
    {
      println("#> Translating: " + fakeURI)
    }
    importTask.doDocument(fakeexp,fakeURI)
    if (tState.verbosity > 0)
    {
      println(" > Success!")
    }

    for (e <- parsed_t)
    {
      if (tState.verbosity > 0) {
        println("\n#> Translating: " + e._2)
      }

      try {
        importTask.doDocument(e._1, e._2)
        if (tState.verbosity > 0) {
          println(" > Success!")
        }
      }
      catch {
        case e : IMPSDependencyException => { println(" > Failure! " + e.getMessage) ; sys.exit }
      }
    }
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

  val unitSortTheorem : DFTheorem = DFTheorem(
    Name("()",None,None),
    ODefString(scala.util.Left(DefString("forall(x:unit%sort,x=an%individual)",None,None)),None,None),
    Some(IMPSForAll(List((IMPSVar("x"),IMPSAtomSort("unit%sort")),(IMPSVar("y"),IMPSAtomSort("unit%sort"))),
      IMPSIff(IMPSEquals(IMPSVar("x"),IMPSVar("y")),IMPSTruth()))),
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

class NEWIMPSParser
{
  def parse(s: String, uri : URI, js : List[JSONObject]) : List[DefForm]
  = parse(new Unparsed(s, msg => throw GeneralError(msg)), uri, js)

  def parse(u : Unparsed, uri : URI, js : List[JSONObject]) : List[DefForm] =
  {
    val dfp = new DefFormParsers(js)
    val foo  = ParserWithSourcePosition.parseAll(dfp.parseImpsSource,u)
    if (!foo.successful) { println("### Parsing Error near: Line " + u.pos.line + " Column" + u.pos.column) }
    assert(foo.successful)

    val dfs : List[DefForm] = foo.get
    dfs.foreach(_.updateSource(uri))
    dfs
  }
}

/* Some things are convenient to carry around in state.
   See also: This exact thing, but in PVS */
class TranslationState ()
{
  var vars               : Context              = Context.empty
  var theories_decl      : List[DeclaredTheory] = Nil
  var theories_raw       : List[DFTheory]       = Nil
  var languages          : List[DFLanguage]     = Nil

  var jsons              : List[JSONObject]     = Nil

  var knownUnknowns      : List[(Int,Term)]     = Nil

  var hashCount          : Int = 0

  var verbosity          : Int = 0

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
      val i = (0 until unknowns).find(j => n == doiName(j,false))/*.getOrElse(
          throw new Exception("Wrong free Variable: " + n + " in " + t)
        )*/
      if (i.isDefined)
        List(VarDecl(doiName(i.get,true), OMS(Typed.ktype)), VarDecl(n, OMV(doiName(i.get,true))))
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
}
