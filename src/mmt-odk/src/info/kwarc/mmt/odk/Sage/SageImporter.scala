package info.kwarc.mmt.odk.Sage

import info.kwarc.mmt.api.archives.{BuildResult, BuildTask, Importer, RedirectableDimension}
import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.{LocalName, ParseError, utils}
import info.kwarc.mmt.api.utils._


sealed abstract class SageObject
case class ParsedCategory(name : String, implied: List[String], axioms: List[String], structure : List[String],
                          doc : String, methods: JSONObject) extends SageObject {
  override def toString = "Category " + name + "\n  Inherits: " + implied.mkString(",") + "\n  Axioms: " +
    axioms.mkString(",") + "\n  Structure: " + structure.mkString(",") + "\n  Doc: " + doc

  private val steps = name.replaceAll("""sage.categories.""","").split("\\.")
  private val dpath = steps.init.foldLeft(Sage.catdoc)((base,step) => base / step)
  private val tname = LocalName(steps.last)
  val path = dpath ? tname
  val includes = (implied ::: structure).distinct
}

class SageImporter extends Importer {
  def toplog(s : => String) = log(s)
  def toplogGroup[A](a : => A) = logGroup(a)
  val reporter = this.report
  val key = "sage-omdoc"
  def inExts = List("json")
  override def logPrefix = "sage"
  override def inDim = RedirectableDimension("sage")
  // reader.export = 100
  // reader.file = Some(File("/home/raupi/lmh/MathHub/ODK/GAP/gap/bitesize.json"))

  var categories : List[ParsedCategory] = Nil

  def readJSON(input : JSON) = input match {
    case JSONArray(all@_*) => all foreach {
      case obj : JSONObject =>
        val tp = obj.getAsString("type") match {
          case "Sage_Category" => ()
          case s : String => throw new ParseError("SAGE type not implemented: " + s)
        }
        val name = obj.getAsString("name")
        val doc = obj.getAsString("__doc__")
        val implied = obj.getAsList(classOf[String],"implied")
        val axioms = obj.getAsList(classOf[String],"axioms")
        val structure = obj.getAsList(classOf[String],"structure")
        val methods = obj.getAs(classOf[JSONObject],"required_methods")
        val ncat = ParsedCategory(name,implied,axioms,structure,doc,methods)
        categories ::= ncat
      case j : JSON => throw new ParseError("Not a JSONObject: " + j)
    }
    case _ => throw new ParseError("Input not a JSONArray!")
  }

  def importDocument(bf: BuildTask, index: Document => Unit): BuildResult = {
    //     if (bf.inFile.filepath.toString < startAt) return
    val d = bf.inFile.name
    val e = try {
      log("reading...")
      val read = File.read(bf.inFile) // .replace("\\\n","")
      log("parsing as JSON")
      val parsed = JSON.parse(read)
      log("To Scala...")
      readJSON(parsed)
    } catch {
      case utils.ExtractError(msg) =>
        println("utils.ExtractError")
        println(msg)
        sys.exit
    }
    // categories foreach(c => log(c.toString))
    log(categories.length + " Objects parsed")
    val trans = new SageTranslator(controller,bf,index)
    trans(categories)
    BuildResult.empty
  }
}

