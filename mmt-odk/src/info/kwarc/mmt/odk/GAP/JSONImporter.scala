package info.kwarc.mmt.odk.GAP

import info.kwarc.mmt.api.archives.{BuildResult, BuildTask, Importer, RedirectableDimension}
import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.frontend.Logger
import info.kwarc.mmt.api.objects.{OMS, Term}
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.lf.Apply

import scala.collection.mutable
import scala.util.Try

sealed abstract class ParsedObject {
  val name : String
  val dependencies: List[String]
}
case class ParsedMethod(op : String, filters : List[List[String]], comment : String, rank : Int) {
  val dependencies = op :: filters.flatten.distinct
}
case class ParsedProperty(name : String, implied : List[String], isTrue :Boolean = false) extends ParsedObject {
  val dependencies = implied
}
case class ParsedOperation(name : String, filters : List[List[List[String]]], methods : List[ParsedMethod], locations : (String,Int))
  extends ParsedObject {
  val dependencies = (filters.flatten.flatten).distinct
}
case class ParsedCategory(name : String, implied : List[String]) extends ParsedObject {
  val dependencies = implied
}
case class ParsedAttribute(name : String, filters : List[String]) extends ParsedObject {
  val dependencies = filters
}
case class ParsedRepresentation(name : String, implied : List[String]) extends ParsedObject {
  val dependencies = implied
}
case class ParsedFilter(name : String, implied : List[String]) extends ParsedObject {
  val dependencies = implied
}

class GAPJSONImporter extends Importer {
  def toplog(s : => String) = log(s)
  def toplogGroup[A](a : => A) = logGroup(a)
  val reporter = this.report
  val key = "gap-omdoc"
  def inExts = List("json")
  override def logPrefix = "gap"
  override def inDim = RedirectableDimension("gap")

  private var all : List[ParsedObject] = Nil
  private var allmethods : List[ParsedMethod] = Nil

  def importDocument(bf: BuildTask, index: Document => Unit): BuildResult = {
    val d = bf.inFile.name
    val e = try {
      log("reading...")
      val read = File.read(bf.inFile) // .replace("\\\n","")
      log("JSON Parsing...")
      val parsed = JSON.parse(read) match {
        case JSONArray(ls@_*) => ls
        case _ => throw new ParseError("File not a JSON Array")
      }
      log("To Scala...")
      logGroup {
        parsed.foreach({
          case obj: JSONObject => parse(obj)
          case j: JSON => throw new ParseError("Not a JSONObject: " + j)
        })
      }
    } catch {
      case utils.ExtractError(msg) =>
        println("utils.ExtractError")
        println(msg)
        sys.exit
    }
    log(all.length + " Objects parsed")
    all foreach (po => {
      val done = dones.get(po)
      if (done.isEmpty) getObject(po)
    })

    val conv = new Translator(controller, bf, index,this)
    conv(dones.values.toList)
    BuildResult.empty
  }

  private def parse(obj : JSONObject) {
    val name = obj.getAsString("name")
    val tp = obj.getAsString("type")
    reg.regs foreach {r =>
      name match {
        case "IsBool" => return ()
        case r(s) => return ()
        case _ =>
      }
    }
    val (ret,missing : List[String]) = tp match {
      case "GAP_Property" | "GAP_TrueProperty" =>
        val impls = Try(obj.getAsList(classOf[String],"implied")).getOrElse(
          obj.getAsList(classOf[String],"filters")).filter(redfilter(name))
        val missings = obj.map.filter(p => p._1.value!="name" && p._1.value!="implied" && p._1.value!="type"
          && p._1.value!="filters")
        if (missings.nonEmpty) throw new ParseError("GAP_Property has additional fields " + missings)
        (ParsedProperty(name,impls,if (tp == "GAP_TrueProperty") true else false),List("implied","filters"))

      case "GAP_Operation" =>
        val filters = obj.getAsList(classOf[JSONArray],"filters") map (_.values.toList map {
          case JSONArray(ls@_*) => (ls.toList map {
            case JSONString(s) => s
            case _ => throw new ParseError("GAP_Operation: filters is not an Array of Arrays of Strings!")
          }).filter(redfilter(name))
          case _ => throw new ParseError("GAP_Operation: filters is not an Array of JSONArrays!")
        })
        val methods = doMethods(obj.getAs(classOf[JSONObject],"methods"),name)
        allmethods :::= methods
        val locobj = obj.getAsList(classOf[JSONObject],"locations").head
        val locations = (locobj.getAsString("file"),locobj.getAsInt("line"))
        // TODO: don't just take the head!
        (ParsedOperation(name,filters,methods,locations),List("filters","methods","locations"))

      case "GAP_Attribute" =>
        val filters = obj.getAsList(classOf[String],"filters").filter(redfilter(name))
        (ParsedAttribute(name,filters),List("filters"))

      case "GAP_Representation" =>
        val implied = obj.getAsList(classOf[String],"implied").filter(redfilter(name))
        (ParsedRepresentation(name,implied),List("implied"))

      case "GAP_Category" =>
        val implied = obj.getAsList(classOf[String],"implied").filter(redfilter(name))
        (ParsedCategory(name,implied),List("implied"))

      case "GAP_Filter" =>
        val implied = obj.getAsList(classOf[String],"implied").filter(redfilter(name))
        (ParsedFilter(name,implied),List("implied"))

      case _ => throw new ParseError("Type not yet implemented: " + tp + " in " + obj)
    }
    val missings = obj.map.filter(p => !("name" :: "type" :: missing).contains(p._1.value))
    if (missings.nonEmpty) throw new ParseError("Type " + tp + " has additional fields " + missings)
    log("Added: " + ret)
    all ::= ret
  }

  private def doMethods(j : JSONObject, op : String) : List[ParsedMethod] = {
    val lists : List[JSON] = (0 to 6).map(_.toString + "args").map(j.apply).map(_.getOrElse(throw new ParseError("<i>args not found in method: " + j))).toList
    lists flatMap (js => js match {
      case ar : JSONArray =>
        ar.toList.map(jo => jo match {
          case o : JSONObject => doMethod(o,op)
          case _ => throw new ParseError("not a json object: " + jo)
        })
      case _ => throw new ParseError("<i>args in method not a JSONArray: " + js)
    })
  }
  private def redfilter(name : String)(s : String) : Boolean = {
    val reals = reg.inner(s)
    if (reals == "<<unknown>>" || reals == name) false else true
  }


  private def doMethod(obj : JSONObject, op : String) : ParsedMethod = {
    val comment = obj.getAsString("comment")
    val filters = obj.getAsList(classOf[JSONArray],"filters") map (_.values.toList.map{
      case JSONString(s) => s
      case _ => throw new ParseError("GAP_Method: filters is not an Array of Arrays of Strings!")
    }.filter(redfilter(op)))
    val rank = obj.getAsInt("rank")
    ParsedMethod(op,filters,comment,rank)
  }

  private val dones : mutable.HashMap[ParsedObject,GAPObject] = mutable.HashMap.empty
  private val donemethods : mutable.HashMap[ParsedMethod,GAPMethod] = mutable.HashMap.empty

  private def getObject(po : ParsedObject) : GAPObject = {
    val done = dones.get(po)
    if (done.isDefined) return done.get
    val deps = logGroup {
      (po.dependencies map { str => (str, reg.parse(str)) }).toMap
    }
    val ret : GAPObject = po match {
      case ParsedFilter(name,implied) =>
        new DeclaredFilter(LocalName(name),implied map deps)
      case ParsedRepresentation(name,implied) =>
        new GAPRepresentation(LocalName(name),implied map deps)
      case ParsedProperty(name,implied,istrue) =>
        new DeclaredProperty(LocalName(name),implied map deps,istrue)
      case ParsedAttribute(name,filters) =>
        new DeclaredAttribute(LocalName(name), filters map deps)
      case ParsedCategory(name,implied) =>
        new DeclaredCategory(LocalName(name), implied map deps)
      case ParsedOperation(name,filters,methods,locations) =>
        new DeclaredOperation(LocalName(name),filters map (_ map (_ map deps)),locations)
    }
    dones += ((po,ret))
    ret
  }
  def getMethod(pm : ParsedMethod) : GAPMethod = {
    val done = donemethods.get(pm)
    if (done.isDefined) return done.get
    val deps = logGroup {
      (pm.dependencies map { str => (str, reg.parse(str)) }).toMap
    }
    val ret = new GAPMethod(reg.parse(pm.op) match {
      case op : GAPOperation => op
      case _ => throw new ParseError("Method's Operation not an Operation: " + pm.op)
    },pm.filters map (_ map deps),pm.comment,pm.rank)
    donemethods += ((pm,ret))
    ret
  }

  /*
  case class ParsedMethod(op : String, filters : List[List[String]], comment : String, rank : Int)
case class ParsedOperation(name : String, filters : List[List[List[String]]], methods : List[ParsedMethod], locations : (String,Int))
  extends ParsedObject
  */

  object reg {
    val regTester = """Tester\((\w+)\)""".r
    val regCatCollection = """CategoryCollections\((\w+)\)""".r
    val regSetter = """Setter\((\w+)\)""".r
    val regHas = """Has(\w+)""".r
    val regSet = """Set(\w+)""".r
    val regGet = """Get\((\w+)\)""".r

    def inner(s : String) : String = s match {
      case regTester(r) => inner(r)
      case regCatCollection(r) => inner(r)
      case regSetter(r) => inner(r)
      case regHas(r) => inner(r)
      case regSet(r) => inner(r)
      case regGet(r) => inner(r)
      case _ => s
    }

    def parse(s : String) : GAPObject = s match {
      case "IsBool" => IsBool
      case regTester(a) => Tester(parse(a))
      case regCatCollection(a) => CategoryCollections(parse(a))
      case regSetter(a) => Setter(parse(a))
      case regHas(a) => Has(parse(a) match {
        case at : GAPAttribute => at
        case _ => throw new ParseError("Expected GAPAttribute; returned " + parse(a).getClass + ": " + a)
      })
      case regSet(a) => GapSet(parse(a))
      case regGet(a) => parse("Is" + a)
      case "IS_SSORT_LIST" => parse("IsSortedList")
      case "IS_NSORT_LIST" => parse("IsNSortedList")
      case "IS_POSS_LIST" => parse("IsPositionsList")
      case "LENGTH" => parse("Length")
      case _ => getObject(all.find(_.name == s).getOrElse(throw new ParseError("GAP Object " + s + " not found")))
    }
    val regs = List(regTester,regCatCollection,regSetter,regHas,regSet,regGet)
  }

}

sealed abstract class GAPObject {
  def getInner : DeclaredObject
  def toTerm : Term
}
sealed abstract class DeclaredObject extends GAPObject {
  val path : GlobalName
  def getInner = this
  def toTerm = OMS(path)
}
trait GAPFilter extends GAPObject
class DeclaredFilter(val name : LocalName, val implied : List[GAPObject]) extends DeclaredObject with GAPFilter {
  lazy val path : GlobalName = ???
}
trait GAPCategory extends GAPObject
class DeclaredCategory(val name : LocalName, val implied : List[GAPObject]) extends DeclaredObject with GAPCategory {
  lazy val path : GlobalName = ???
}
class GAPRepresentation(val name : LocalName, val implied : List[GAPObject]) extends DeclaredObject {
  lazy val path : GlobalName = ???
}
class GAPMethod(val operation: GAPOperation, val filters : List[List[GAPObject]], val comment : String, val rank : Int)
trait GAPOperation extends GAPObject
class DeclaredOperation(val name : LocalName, val filters : List[List[List[GAPObject]]],
                        val locations : (String,Int)) extends DeclaredObject with GAPOperation {
  private val steps = locations._1.replace("/home/makx/ac/gap/","").split("/").toList//.map(SimpleStep).toList
  private val subpath : LocalName = if (steps.length>1) LocalName(steps.init.map(SimpleStep)) else LocalName("")

  val parent : MPath = {if (subpath != LocalName("")) GAP._base / subpath else GAP._base } ? {
    if (steps.nonEmpty) LocalName(steps.last.split("\\.").toList.head) else
      throw new ParseError("No theory name deducible from " + name)
  }
  val path = parent ? name

  val arity : Option[Int] = None
}
trait GAPAttribute extends GAPObject {
  val returntype : Option[GAPObject] = None
}
class DeclaredAttribute(val name : LocalName, val filters: List[GAPObject]) extends DeclaredObject with GAPAttribute {
  lazy val path : GlobalName = ???
}
trait GAPProperty extends GAPAttribute
class DeclaredProperty(name : LocalName, implied : List[GAPObject], val istrue : Boolean) extends DeclaredAttribute(name,implied) with GAPProperty

object IsBool extends DeclaredCategory(LocalName("IsBool"),Nil) {
  override lazy val path = GAP.IsBool
}
case class CategoryCollections(obj: GAPObject) extends GAPCategory {
  def getInner = obj.getInner
  def toTerm = Apply(GAP.catcollection,obj.toTerm)
}
object Setter {
  def apply(obj : GAPObject) = GapSet(obj)
}
case class GapSet(obj: GAPObject) extends GAPOperation {
  def getInner = obj.getInner
  def toTerm = Apply(GAP.setter,obj.toTerm)
}
object Tester {
  def apply(obj : GAPObject) = Has(obj)
}
case class Has(obj: GAPObject) extends GAPProperty {
  def getInner = obj.getInner
  def toTerm = Apply(GAP.has,obj.toTerm)
}


/*
sealed abstract class GAPObject {
  val namestr: String
  val dependencies: List[String]

  protected var deps: Set[GAPObject] = Set()
  private var computed = false
  private var depvalue = -1

  def getInner : Haspath

  def toTerm : Term

  def implications(all: List[GAPObject]): Set[GAPObject] = {
    if (!computed) {
      deps = dependencies.map(ref =>
        if (ref == "<<unknown>>") this
        else reg.parse(ref)(all)
      ).toSet - this
      computed = true
      deps
    } else deps
  }
  /*
  override def toString = this.getClass.getSimpleName + " " + name + impls.map(s =>
    "\n - " + s.getClass.getSimpleName + " " + s.name.toString).mkString("")
    */
  def depweight(all: List[GAPObject]) : Int = {
    if (depvalue < 0) {
      depvalue = implications(all).map(_.depweight(all)).sum
      depvalue
    }
    else depvalue
  }
}

trait Haspath extends GAPObject {
  val path : GlobalName
}

sealed abstract class Property extends GAPObject

case class GAPMethod(op : String, filterstr : List[List[String]], comment : String, rank : Int) extends GAPObject {
  val namestr = op + "_" + filterstr.toString
  val dependencies = op :: filterstr.flatten
  def filters(implicit all : List[GAPObject]) : List[List[GAPObject]] = filterstr.map(_.map(reg.parse).filter(_.getInner != this))
  val arity = filterstr.length
  lazy val operation = deps.find(_.namestr == op).getOrElse(throw new ParseError(op + " not found in deps of method"))
  def toTerm = operation.toTerm
  val returntype : Option[GAPObject] = None
  def getInner = deps.find(_.namestr == op).get.getInner

  override def toString = "Method for " + op + ": " + filterstr.map(f => "(" + f.mkString(",") + ")").mkString(",")
}

case class GAPProperty(namestr : String, impliedstr : List[String], isTrue :Boolean = false) extends Property with Haspath {
  val dependencies = impliedstr
  def filters(implicit all : List[GAPObject]) : List[GAPObject] = impliedstr.map(reg.parse).filter(_.getInner != this)
  override def toString = {if (isTrue) "True Property " else "Property "} + name + ": " + impliedstr.mkString(",")

  lazy val subpath : String = ???
  lazy val theory : LocalName = ???
  lazy val name = LocalName(namestr)
  lazy val path = {if (subpath!="") GAP._base / subpath else GAP._base } ? theory ? name
  def toTerm = OMS(path)
  def getInner = this
}

sealed abstract class Operation extends GAPObject

case class GAPOperation(namestr : String, filterstr : List[List[List[String]]], methods : List[GAPMethod], locations : (String,Int)) extends Operation with Haspath {
  val dependencies : List[String] = filterstr.flatMap(_.flatten)
  def filters(implicit all : List[GAPObject]) : List[List[List[GAPObject]]] = filterstr.map(_.map(_.map(reg.parse).filter(_.getInner != this)))
  override def toString = "Operation " + namestr + ": " + filterstr.map(l => " - " + l.map(_.toString).mkString(",")).mkString("\n") +
    methods.mkString("\n  ")

  val arity : Option[Int] = if (methods.isEmpty && filterstr.forall(_.forall(_.isEmpty))) Some(0)
    else if (methods.nonEmpty && methods.forall(_.arity == methods.head.arity)) Some(methods.head.arity)
    else if (filterstr.isEmpty) Some(0) else Some(filterstr.head.length)

  private val steps = locations._1.replace("/home/makx/ac/gap/","").split("/").toList//.map(SimpleStep).toList
  val subpath : LocalName = if (steps.length>1) LocalName(steps.init.map(SimpleStep)) else LocalName("")
  val theory : LocalName = if (steps.nonEmpty) LocalName(steps.last.split("\\.").toList.head) else
    throw new ParseError("No theory name deducible from " + namestr)
  val name = LocalName(namestr)
  val path = {if (subpath != LocalName("")) GAP._base / subpath else GAP._base } ? theory ? name
  def toTerm = OMS(path)
  def getInner = this
}

sealed abstract class Category extends GAPObject

case class GAPCategory(namestr : String, impliedstr : List[String]) extends Category with Haspath {
  val dependencies = impliedstr
  def filters(implicit all : List[GAPObject]) : List[GAPObject] = impliedstr.map(reg.parse).filter(_.getInner != this)
  override def toString = "Category " + namestr + ": " + impliedstr.mkString(",")

  lazy val subpath : String = ???
  lazy val theory : LocalName = ???
  lazy val name = LocalName(namestr)
  lazy val path = {if (subpath!="") GAP._base / subpath else GAP._base } ? theory ? name
  def toTerm = OMS(path)
  def getInner = this
}

case class GAPAttribute(namestr : String, filterstr : List[String]) extends Haspath {
  val dependencies = filterstr
  def filters(implicit all : List[GAPObject]) : List[GAPObject] = filterstr.map(reg.parse).filter(_.getInner != this)
  override def toString = "Attribute " + name + ": " + filterstr.mkString(",")

  lazy val subpath : String = ???
  lazy val theory : LocalName = ???
  lazy val name = LocalName(namestr)
  lazy val path = {if (subpath!="") GAP._base / subpath else GAP._base } ? theory ? name
  val returntype : Option[GAPObject] = None
  def toTerm = OMS(path)
  def getInner = this
}

case class GAPRepresentation(namestr : String, impliedstr : List[String]) extends Haspath {
  val dependencies = impliedstr
  def filters(implicit all : List[GAPObject]) : List[GAPObject] = impliedstr.map(reg.parse).filter(_.getInner != this)
  override def toString = "Representation " + name + ": " + impliedstr.mkString(",")

  lazy val subpath : String = ???
  lazy val theory : LocalName = ???
  lazy val name = LocalName(namestr)
  lazy val path = {if (subpath!="") GAP._base / subpath else GAP._base } ? theory ? name
  def toTerm = OMS(path)
  def getInner = this
}

case class GAPFilter(namestr : String, impliedstr : List[String]) extends Haspath {
  val dependencies = impliedstr
  def filters(implicit all : List[GAPObject]) : List[GAPObject] = impliedstr.map(reg.parse).filter(_.getInner != this)
  override def toString = "Filter " + name + ": " + impliedstr.mkString(",")

  lazy val subpath : String = ???
  lazy val theory : LocalName = ???
  lazy val name = LocalName(namestr)
  lazy val path = {if (subpath!="") GAP._base / subpath else GAP._base } ? theory ? name
  def toTerm = OMS(path)
  def getInner = this
}

object IsBool extends GAPCategory("IsBool",Nil) {
  override def toTerm = GAP.IsBool
}

case class CategoryCollections(obj: GAPObject) extends Category {
  val namestr = obj.namestr + ".catcollection"
  val dependencies = List()
  override def getInner = obj.getInner
  override def toString = "Collection of " + obj.toString
  def toTerm = Apply(GAP.catcollection,obj.toTerm)
}

object Setter {
  def apply(obj : GAPObject) = GapSet(obj)
}

case class GapSet(obj: GAPObject) extends Operation {
  val namestr = obj.namestr + ".Setter"
  val dependencies = List()
  override def getInner = obj.getInner
  override def toString = "Setter of " + obj.toString
  def toTerm = Apply(GAP.setter,obj.toTerm)
}

object Tester {
  def apply(obj : GAPObject) = Has(obj)
}

case class Has(obj: GAPObject) extends Property {
  val namestr = "Has" + obj.namestr
  val dependencies = List()
  override def getInner = obj.getInner
  override def toString = "Has " + obj.toString
  def toTerm = Apply(GAP.has,obj.toTerm)
}
*/