package info.kwarc.mmt.imps

import info.kwarc.mmt.api.{GeneralError, LocalName, NamespaceMap, Path}

import scala.io.Source
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.lf.Typed
import info.kwarc.mmt.api.utils.JSON
import info.kwarc.mmt.api.utils.JSON.JSONError

class IMPSImporter extends Importer
{
  val key: String = "imps-omdoc"

  def inExts = List("index")

  def importDocument(bf: BuildTask, index: Document => Unit): BuildResult =
  {
    val tState : TranslationState = new TranslationState()

    println("\nReading index file: " + bf.inFile.getName + "\n")

    val tfiles    = bf.inFile.up.canonical.listFiles.filter(_.getName.endsWith(".t"))
    val jsonfiles = bf.inFile.up.canonical.listFiles.filter(_.getName.endsWith(".json"))

    var parsed_json : List[JSONObject] = Nil
    var parsed_t    : List[(Exp, URI)] = Nil

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
    println(" ")

    for (file <- tfiles)
    {
      println("Reading imps file: " + file)

      val e = try
      {
        val fileLines = Source.fromFile(file).getLines
        var contents: String = ""

        for (line <- fileLines)
        {
          /* Drop code comments. There's like four of them
           * in the entire codebase and they mess up parsing. */
          contents = contents + line.takeWhile(_ != ';') + "\n"
        }

        val lp: IMPSParser = new IMPSParser()
        lp.parse(contents, FileURI(file), parsed_json)

      } catch {
        case e: ExtractError =>
          log(e.getMessage)
          sys.exit
      }

      assert(e.isInstanceOf[Exp])
      e match {
        case (d: Exp) => parsed_t = parsed_t.::((d,FileURI(file)))
        case _        => BuildResult.empty
      }

    }

    println("\n== READING / PARSING COMPLETE ; BEGINNING TRANSLATION==\n")

    val importTask = new IMPSImportTask(controller, bf, index, tState)

    var all_translations : List[((Exp, URI), Boolean)] = parsed_t.map(e => (e,false))
    var i : Int = 0

    while (all_translations.map(_._2).contains(false))
    {
      if (!all_translations(i)._2) // If not translated yet, translate
      {
        val d : (Exp, URI) = parsed_t(i)
        println("#> Translating: " + d._2)

        try
        {
          importTask.doDocument(d._1, d._2)
          println(" > Success!\n")

          all_translations = all_translations.updated(i,(d,true))
        }
        catch {
          case e : IMPSDependencyException => {
            println(" > Failure, will retry next pass: " + e.getMessage + "\n")
          }
        }
      }

      if (i == (all_translations.length - 1)) { i = 0 } else { i += 1 }

    }

    BuildSuccess(Nil, Nil)
  }
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
