package info.kwarc.mmt.imps

import info.kwarc.mmt.api.{GeneralError, LocalName, NamespaceMap, Path}

import scala.io.Source
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.lf.Typed

class IMPSImporter extends Importer
{
  val key: String = "imps-omdoc"

  def inExts = List("t")

  def importDocument(bf: BuildTask, index: Document => Unit): BuildResult =
  {
    val tState : TranslationState = new TranslationState()

    if (bf.inPath == FilePath("boole_prime.t"))
    {
      val files = bf.inFile.up.canonical.listFiles.filter(_.getName.endsWith(".t"))

      for (file <- files)
      {
        log("Reading " + file)
        val e = try {
          // TODO: Process comments at end of line
          val fileLines = Source.fromFile(file).getLines
          var contents: String = ""
          for (line <- fileLines) {
            contents = contents + line + "\n"
          }
          val lp: IMPSParser = new IMPSParser()
          lp.parse(contents, FileURI(file))
        } catch {
          case e: ExtractError =>
            log(e.getMessage)
            sys.exit
        }

        val importTask = new IMPSImportTask(controller, bf, index, tState)

        assert(e.isInstanceOf[Exp])
        e match {
          case (d : Exp) => importTask.doDocument(d, FileURI(bf.inFile))
          case _         => BuildResult.empty
        }
      }
    }
    else { BuildResult.empty }

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
