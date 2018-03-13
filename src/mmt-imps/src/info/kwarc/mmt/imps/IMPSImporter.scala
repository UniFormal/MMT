package info.kwarc.mmt.imps

import info.kwarc.mmt.api._

import scala.io.Source
import util.control.Breaks._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.modules.DeclaredTheory
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

    println("\nReading index file: " + bf.inFile.getName + "\n")

    val tfiles    = bf.inFile.up.canonical.listFiles.filter(_.getName.endsWith(".t")).toList
    val jsonfiles = bf.inFile.up.canonical.listFiles.filter(_.getName.endsWith(".json"))

    var parsed_json : List[JSONObject] = Nil

    for (file <- jsonfiles)
    {
      println("Reading json file: " + file)

      val fileLines = Source.fromFile(file).getLines
      var contents: String = ""
      for (line <- fileLines) {
        contents = contents + line + "\n"
      }

      val j : JSONObject = JSON.parse(contents).asInstanceOf[JSONObject]
      parsed_json = parsed_json.::(j)

    }
    tState.jsons = parsed_json

    println("\n== JSON PARSING COMPLETE ; BUILDING DEPENDENCY LIST ==\n")

    /* hand coded because everything else is too complicated */
    val order : Array[String] = Array(
      "pure-generic-theories.t",
      "indicators.t",
      "pure-generic-theories-with-subsorts.t",
      "pre-reals.t",
      "reals.t",
      "quotient-structures.t",
      "normed-spaces.t",
      "mappings-from-an-interval.t",
      //...
    )

    val theseFiles = order.take(1)
    order.map(i => println((if (theseFiles.contains(i)) {"✓ "} else {"  "}) + i))
    println("  ...")
    val translateFiles = tfiles.filter(f => theseFiles.contains(f.getName))

    println("\n== BUILDING DEPENDENCY TREE COMPLETE ; BEGINNING T TRANSLATION ==\n")

    val importTask = new IMPSImportTask(controller, bf, index, tState)

    println(" > tState: " + tState.theories_raw.length + " raw theories, " + tState.theories_decl.length + " declared theories and " + tState.languages.length + " languages.\n")

    val fakeURI : URI = URI(bf.inFile.getParentFile.getParentFile.getAbsolutePath + "/the-kernel-theory.t")
    val fakeexp : Exp = Exp(List(theKernelLang(),theKernelTheory(),unitSortTheorem()),None)

    importTask.doDocument(fakeexp,fakeURI)

    println("\n > tState: " + tState.theories_raw.length + " raw theories, " + tState.theories_decl.length + " declared theories and " + tState.languages.length + " languages.\n")

    for (file <- translateFiles)
    {
      println("\n###########\nReading imps file: " + file)

      val e = try
      {
        val contents = Source.fromFile(file).mkString
        val lp: IMPSParser = new IMPSParser()

        lp.parse(contents, FileURI(file), parsed_json)

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

      val uri : URI = FileURI(file)
      println("#> Translating: " + uri)

      try
      {
        e match
        {
          case e@Exp(_,_) => {
            importTask.doDocument(e, uri)
            println(" > Success!")
          }
          case _ => println("> Parsing mess-up!") ; sys.exit()
        }
      }
      catch {
        case e : IMPSDependencyException => { println(" > Failure! " + e.getMessage) ; sys.exit }
      }
      println(" > tState: " + tState.theories_raw.length + " raw theories, " + tState.theories_decl.length + " declared theories and " + tState.languages.length + " languages.\n")
    }

    BuildSuccess(Nil, Nil)
  }

  def theKernelLang() : Language = Language(
    "THE-KERNEL-LANGUAGE",
    None,
    None,
    Some(LangBaseTypes(List("ind","prop","unitsort"),None)),
    None,
    Some(SortSpecifications(List((IMPSAtomSort("ind"),IMPSAtomSort("ind")),
      (IMPSAtomSort("prop"),IMPSAtomSort("prop")),
      (IMPSAtomSort("unit%sort"),IMPSAtomSort("unit%sort"))),None)),
    Some(ConstantSpecifications(List(("an%individual",IMPSAtomSort("unit%sort"))),None)),
    None
  )

  def theKernelTheory() : Theory = Theory(
    "the-kernel-theory",
    Some(ArgumentLanguage("THE-KERNEL-LANGUAGE",None)),
    None,
    Some(TheoryAxioms(List(AxiomSpecification(
      IMPSForAll(List((IMPSVar("x"),Some(IMPSAtomSort("unit%sort")))),IMPSEquals(IMPSVar("x"),IMPSMathSymbol("an%individual"))),
      Some("unit-sort-defining-axiom"),
      None,
      None)),None)),
    None,
    None
  )

  def unitSortTheorem() : Theorem = Theorem(
    "()",
    IMPSForAll(List((IMPSVar("x"),Some(IMPSAtomSort("unit%sort"))),(IMPSVar("y"),Some(IMPSAtomSort("unit%sort")))),
      IMPSIff(IMPSEquals(IMPSVar("x"),IMPSVar("y")),IMPSTruth())),
    false,
    false,
    ArgumentTheory("the-kernel-theory",None),
    Some(ArgumentUsages(List(Usage.ELEMENTARYMACETE),None)),
    None,
    None,
    None,
    None,
    None
  )
}

/* Some things are convenient to carry around in state.
   See also: This exact thing, but in PVS */
class TranslationState ()
{
  var vars               : Context              = Context.empty
  var theories_decl      : List[DeclaredTheory] = Nil
  var theories_raw       : List[Theory]         = Nil
  var languages          : List[Language]       = Nil

  var jsons              : List[JSONObject]     = Nil

  protected var unknowns : Int                  = 0

  protected def doiName(i : Int, isType : Boolean) : LocalName = {
    LocalName("") / { if (isType) LocalName("I") else LocalName("i") } / i.toString
  }

  def addUnknown() : Term = OMV(doiName({unknowns+=1;unknowns-1},false))

  def bindUnknowns(t : Term) = {
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
    vars = Context.empty
  }
}
