package info.kwarc.mmt.odk.Sage

import info.kwarc.mmt.api.archives.{BuildResult, BuildTask, Importer, RedirectableDimension}
import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.{LocalName, ParseError, utils}
import info.kwarc.mmt.api.utils._


sealed abstract class SageObject
case class SageMethod(name : String, doc : String, arity : Int, tp : String)
case class ParsedCategory(name : String, implied: List[String], axioms: List[String], structure : List[String],
                          doc : String,
                          mmt : String,
                          gap : String,
                          elem_methods: (String,List[SageMethod]),
                          morph_methods: (String,List[SageMethod]),
                          parent_methods: (String,List[SageMethod]),
                          subcategory_methods : (String,List[SageMethod])) extends SageObject {
  override def toString = "Category " + name + "\n  Inherits: " + implied.mkString(",") + "\n  Axioms: " +
    axioms.mkString(",") + "\n  Structure: " + structure.mkString(",") + "\n  Doc: " + doc

  private val steps = name.replaceAll("""sage.categories.""","").split("\\.")
  private val dpath = steps.init.foldLeft(Sage.catdoc)((base,step) => base / step)
  private val tname = LocalName(steps.last)
  val path = dpath ? tname
  val includes = implied.distinct

  val isStructure = structure contains name
}
case class ParsedClass(name : String, doc : String, implied: List[String],methods : List[SageMethod]) extends SageObject {
  private val steps = name.replaceAll("""sage.categories.""","").split("\\.")
  private val dpath = steps.init.foldLeft(Sage.clssdoc)((base,step) => base / step)
  private val tname = LocalName(steps.last)
  val path = dpath ? tname
  val includes = implied.distinct
}

class SageImporter extends Importer {
  def toplog(s : => String) = log(s)
  def toplogGroup[A](a : => A) = logGroup(a)
  val reporter = this.report
  val key = "sage-omdoc"
  def inExts = List("json")
  override def logPrefix = "sage-omdoc"
  override def inDim = RedirectableDimension("sage")
  // reader.export = 100
  // reader.file = Some(File("/home/raupi/lmh/MathHub/ODK/GAP/gap/bitesize.json"))

  var categories : List[ParsedCategory] = Nil
  var classes : List[ParsedClass] = Nil

  def doMethod(input : JSONObject, name : String, tp : String) : SageMethod = {
    val doc = input.getAsString("__doc__")
    val arity = try { input.getAsList(classOf[String],"args").length } catch {
      case e : Exception => 0
    }
    SageMethod(name,doc,arity,tp)
  }

  def doClass(obj : JSONObject) = {
    val name = obj.getAsString("name")
    val doc = obj.getAsString("__doc__")
    val implied = obj.getAsList(classOf[String], "implied")
    val methobj = obj.getAs(classOf[JSONObject], "methods")
    val methods = methobj.map.collect {
      case (s, j: JSONObject) => doMethod(j, s.value, "classmethod")
    }
    // todo argspec
    classes ::= ParsedClass(name,doc,implied,methods)
  }

  def doCategory(obj : JSONObject) = {
    val eaten = "type" :: {
      obj.getAsString("type") match {
        case "Sage_Category" =>
          val name = obj.getAsString("name")
          val doc = obj.getAsString("__doc__")
          val implied = obj.getAsList(classOf[String], "implied")
          val axioms = obj.getAsList(classOf[String], "axioms")
          val structure = obj.getAsList(classOf[String], "structure")
          /*
          val methods = obj.getAs(classOf[JSONObject], "methods")
          val elems = obj.getAs(classOf[JSONObject],"element_class").getAsList(classOf[JSONObject],"optional").map(
            doMethod(_,true)) ::: methods.getAs(classOf[JSONObject],"element").getAsList(classOf[JSONObject],"required").map(doMethod(_,false))
          val morphs = methods.getAs(classOf[JSONObject],"morphism").getAsList(classOf[JSONObject],"optional").map(
            doMethod(_,true)) ::: methods.getAs(classOf[JSONObject],"morphism").getAsList(classOf[JSONObject],"required").map(doMethod(_,false))
          val parents = methods.getAs(classOf[JSONObject],"parent").getAsList(classOf[JSONObject],"optional").map(
            doMethod(_,true)) ::: methods.getAs(classOf[JSONObject],"parent").getAsList(classOf[JSONObject],"required").map(doMethod(_,false))
          */
          var mclass = obj.getAs(classOf[JSONObject],"element_class")
          val elems = (mclass.getAsString("__doc__"),mclass.getAs(classOf[JSONObject],"methods").map.collect{
            case (s,j : JSONObject) => doMethod(j,s.value,"element")
          })
          mclass = obj.getAs(classOf[JSONObject],"morphism_class")
          val morphs = (mclass.getAsString("__doc__"),mclass.getAs(classOf[JSONObject],"methods").map.collect{
            case (s,j : JSONObject) => doMethod(j,s.value,"morphism")
          })
          mclass = obj.getAs(classOf[JSONObject],"parent_class")
          val parents = (mclass.getAsString("__doc__"),mclass.getAs(classOf[JSONObject],"methods").map.collect{
            case (s,j : JSONObject) => doMethod(j,s.value,"parent")
          })
          mclass = obj.getAs(classOf[JSONObject],"subcategory_class")
          val subcats = (mclass.getAsString("__doc__"),mclass.getAs(classOf[JSONObject],"methods").map.collect{
            case (s,j : JSONObject) => doMethod(j,s.value,"subcategory")
          })
          val mmt = obj.getAsString("mmt")
          val gap = obj.getAsString("gap")
          val ncat = ParsedCategory(name, implied, axioms, structure, doc, mmt, gap, elems, morphs, parents,subcats)
          categories ::= ncat
          List("name", "__doc__", "implied", "axioms", "structure", "element_class", "morphism_class",
            "parent_class","gap","mmt","subcategory_class")
        case s: String => throw new ParseError("SAGE type not implemented: " + s)
      }
    }
    val missings = obj.map.filter(p => !eaten.contains(p._1.value))
    if (missings.nonEmpty) throw new ParseError("Object fields not implemented: " + missings)
  }

  def readJSON(input : JSON) = input match {
    case JSONArray(all@_*) => all foreach {
      case obj : JSONObject =>
        doCategory(obj)
      case j : JSON => throw new ParseError("Not a JSONObject: " + j)
    }
    case JSONObject(List((JSONString("categories"),cats : JSONObject),(JSONString("classes"),clss : JSONObject))) =>
      cats.map.map(_._2).foreach {
        case o : JSONObject => doCategory(o)
      }
      clss.map.map(_._2).foreach {
        case o : JSONObject => doClass(o)
      }
    case _ =>
      throw new ParseError("Input not a JSONArray!")
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
      case e: utils.ExtractError =>
        println("utils.ExtractError")
        println(e.msg)
        sys.exit
      case e : Exception =>
        println(e.getMessage)
        sys.exit
    }
    // categories foreach(c => log(c.toString))
    categories = categories.distinct
    classes = classes.distinct
    val allaxioms = categories.flatMap(_.axioms).distinct
    val allmethods = (categories.flatMap(c => c.elem_methods._2 ::: c.subcategory_methods._2 :::
      c.morph_methods._2 ::: c.parent_methods._2) ::: classes.flatMap(_.methods)).distinct

    val trans = new SageTranslator(controller,bf,index,toplog)
    trans(categories ::: classes)
    log(categories.length + " Categories")
    log(allaxioms.length + " Axioms")
    log(allmethods.length + " Methods")
    log(classes.length + " Classes")
    BuildResult.empty
  }
}

