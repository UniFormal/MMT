package info.kwarc.mmt.odk.Singular

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.checking.{Checker, CheckingEnvironment, MMTStructureChecker, RelationHandler}
import info.kwarc.mmt.api.documents.{Document, FileLevel, MRef}
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.objects.{OMMOD, OMS, Term}
import info.kwarc.mmt.api.ontology.{RelationalElement, ULOStatement}
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.lf.Arrow

class SingularImporter extends Importer {
  // def toplog(s : => String) = log(s)
  // def toplogGroup[A](a : => A) = logGroup(a)
  val reporter = this.report
  val key = "singular-omdoc"
  def inExts = List("js")
  override def logPrefix = "singular"
  override def inDim = RedirectableDimension("Singular")

  // private var all : List[ParsedObject] = Nil
  // private var allmethods : List[ParsedMethod] = Nil

  def importDocument(bf: BuildTask, index: Document => Unit,rel:ULOStatement => Unit): BuildResult = {
    val d = bf.inFile.name
    val topdoc = new Document(bf.narrationDPath,FileLevel)
    var ths : List[MPath] = Nil
    controller add topdoc
    val e = try {
      log("reading...")
      val read = File.read(bf.inFile) // .replace("\\\n","")
      log("JSON Parsing...")
      val parsed = JSON.parse(read) match {
        case JSONArray(ls@_*) => ls.collect{
          case o : JSONObject =>
            SingularFile(o.getAsString("library_filename"),o.getAsList(classOf[JSONObject],"funcs") flatMap doFunction)
            // println(o.getAsString("library_filename"))
        }
        case _ => throw new ParseError("File not a JSON Array")
      }
      val checker = controller.extman.get(classOf[Checker], "mmt").getOrElse {
        throw GeneralError("no MMT checker found")
      }.asInstanceOf[MMTStructureChecker]
      val cont : (StructuralElement => Unit) = se => {
        controller add se
      }
      parsed.map(_.asTheory(cont)).foreach(t => {
        controller add MRef(topdoc.path,t.path)
        ths ::= t.path
      })
      ths foreach {p =>
        val ce = new CheckingEnvironment(controller.simplifier, new ErrorLogger(report), RelationHandler.ignore, bf)
        checker(p)(ce)
      }
    } catch {
      case e: utils.ExtractError =>
        println("utils.ExtractError")
        println(e.msg)
        sys.exit()
    }
    log("Finished")
    index(topdoc)
    BuildSuccess(List(LogicalDependency(Singular.meta)),ths map LogicalDependency)
  }

  private def doFunction(jo : JSONObject) : Option[SingularFunction] = {
    //log(jo.map.map(_._1.toString).mkString(", "))
    // println(jo.map.map(_._1))
    val ret = SingularFunction(
      jo.getAsString("name"),
      jo.getAs(classOf[JSONArray],"arguments") collect {
        case JSONString(s) => s
        case JSONBoolean(false) => return None
      },
      jo.getAs(classOf[Boolean],"is_global"),
      jo.getAsString("doc_string_plain"),
      jo.apply("guessed_output_type").map(_.asInstanceOf[JSONString].value)
    )
    // log(ret.toString)
    Some(ret)
  }
}

case class SingularFile(name : String, content : List[SingularFunction]) {
  private val ls = name.split('/')
  private val (sub,thname) = (ls.drop(ls.indexOf("LIB")).init,LocalName(ls.last))
  private val thpath = sub.foldLeft(Singular.dpath)((d,s) => d / s)
  def asTheory(cont : StructuralElement => Unit) = {
    val th = Theory.empty(thpath,thname,Some(Singular.meta))
    cont(th)
    content foreach (c => cont(c.asConstant(thpath ? thname)))
    th
  }
}
case class SingularFunction(name : String, arguments : List[String], is_global : Boolean, docstring : String, got : Option[String]) {
  import Singular._
  override def toString: String = name + ": " + arguments.mkString(" -> ") + "\n - " + docstring.replace("\n","\n - ")
  private def doType = arguments.foldLeft[Term](tp(got.getOrElse("returntype")))((tm,s) => Arrow(tp(s),tm))
  def asConstant(th : MPath) : Constant = Constant(OMMOD(th),LocalName(name),Nil,Some(doType),None,None)
}
