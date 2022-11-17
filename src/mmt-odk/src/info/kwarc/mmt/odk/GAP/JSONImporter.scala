package info.kwarc.mmt.odk.GAP

import info.kwarc.mmt.api.archives.{BuildResult, BuildTask, Importer, RedirectableDimension}
import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.frontend.Logger
import info.kwarc.mmt.api.objects.{OMS, Term}
import info.kwarc.mmt.api.{LocalName, _}
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.lf.Apply

import scala.collection.mutable
import scala.util.Try

sealed abstract class ParsedObject {
  val name : String
  val aka : List[String]
  val dependencies: List[String]
  val tp : String
}
case class ParsedMethod(op : String, filters : List[List[String]], comment : String, rank : Int) {
  val dependencies = op :: filters.flatten.distinct
}
case class ParsedProperty(name : String, aka : List[String], implied : List[String], isTrue :Boolean = false,location : (String,Int),methods:List[ParsedMethod]) extends ParsedObject {
  val dependencies = implied
  val tp = "GAPProperty"
}
case class ParsedOperation(name : String, aka : List[String], filters : List[List[List[String]]], methods : List[ParsedMethod], location : (String,Int))
  extends ParsedObject {
  val dependencies = filters.flatten.flatten.distinct
  val tp = "GAPOperation"
}
case class ParsedCategory(name : String, aka : List[String], implied : List[String],location : (String,Int)) extends ParsedObject {
  val dependencies = implied
  val tp = "GAPCategory"
}
case class ParsedAttribute(name : String, aka : List[String], filters : List[String],location : (String,Int),methods:List[ParsedMethod]) extends ParsedObject {
  val dependencies = filters
  val tp = "GAPAttribute"
}
case class ParsedRepresentation(name : String, aka : List[String], implied : List[String],location : (String,Int)) extends ParsedObject {
  val dependencies = implied
  val tp = "GAPRepresentation"
}
case class ParsedFilter(name : String, aka : List[String], implied : List[String],location : (String,Int)) extends ParsedObject {
  val dependencies = implied
  val tp = "GAPFilter"
}

case class ParsedConstructor(name : String, aka : List[String],location : (String,Int),filters: List[String],methods:List[ParsedMethod]) extends ParsedObject {
  override val dependencies: List[String] = filters
  val tp = "Constructor"
}

case class ParsedDefinedFilter(name : String, aka : List[String], conjof : List[String],location : (String,Int)) extends ParsedObject {
  val dependencies = conjof
  val tp = "GAPDefinedFilter"
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
      case e: utils.ExtractError =>
        println("utils.ExtractError")
        println(e.msg)
        sys.exit()
    }
    log(s"${all.length} Objects parsed.")
    log("Converting...")
    all foreach (po => {
      val done = dones.get(po)
      if (done.isEmpty) {
        getObject(po)
      }
    })
    log("Done.")

    log("Searching for duplicates:")
    val ops = dones.values.collect {
      case op : DeclaredOperation => op
    }.toList
    val values = dones.values.filter{
      case at : DeclaredAttribute if ops.exists(o => o.name.toString == at.name.toString) => false
      case _ => true
    }.toList
    val newvalues = (values ::: ops).distinct collect {case ob: DeclaredObject => ob}
    val duplicates = newvalues.collect({
      case s : DeclaredObject => newvalues.filter({
        case r : DeclaredObject if r.name == s.name => true
        case _ => false
      })
    }).filter(_.length > 1)
    duplicates.foreach(s => log(" - " + s.mkString("\n    - ")))
    if (duplicates.nonEmpty) return BuildResult.empty

    val conv = new Translator(controller, bf, index,this)
    log(s"${newvalues.length} Objects parsed.")
    conv(newvalues)
    BuildResult.empty
  }

  private def parse(obj : JSONObject): Unit = {
    val givenname = obj.getAsString("name")

    val tp = obj.getAsString("type")
    if (tp == "GAP_Function") return ()

    val names = givenname :: {
      obj("aka") match {
        case Some(ar: JSONArray) => ar.values.map(_.asInstanceOf[JSONString].value).toList
        case _ => Nil
      }
    }
    val name = names.find(s => !s.contains("_") && !s.contains(" ") && s.toLowerCase != s && s.toUpperCase != s).getOrElse(names.head)
    val aka = names.filter(_ != name)

    reg.regs foreach {r =>
      name match {
        case "IsBool" => return ()
        case "IsObject" => return ()
        case r(s) => return ()
        case _ =>
      }
    }
    // TODO Delete after Locations are added
    /*
    if (tp == "GAP_AndFilter") {
      val conjs = obj.getAsList(classOf[String],"conjunction_of").filter(redfilter(name))
      all ::= ParsedDefinedFilter(name,aka,conjs)
      return ()
    }
    */


    val locations = try {
      val locobj = try { obj.getAsList(classOf[JSONObject],"locations").head } catch {
        case e : Exception => obj.getAs(classOf[JSONObject],"location")
      }
      (locobj.getAsString("file"),Try(locobj.getAsInt("line").toInt).getOrElse(0))
    } catch {
      case ParseError(_) => ("",0)
    }
    // TODO: don't just take the head!

    val (ret,eaten : List[String]) = tp match {
      case "GAP_AndFilter" =>
        val conjs = obj.getAsList(classOf[String],"conjunction_of").filter(redfilter(name))
        (ParsedDefinedFilter(name,aka,conjs,locations),List("conjunction_of"))
      case "GAP_Property" | "GAP_TrueProperty"| "Property" =>
        val impls = Try(obj.getAsList(classOf[String],"implied")).getOrElse(
          obj.getAsList(classOf[String],"filters")).filter(redfilter(name))
        val methods = doMethods(obj.getAs(classOf[JSONObject],"methods"),name)
        allmethods :::= methods
        (ParsedProperty(name,aka,impls,if (tp == "GAP_TrueProperty") true else false,locations,methods),List("implied","filters","methods"))

      case "GAP_Operation"| "Operation" =>
        val filters = obj.getAsList(classOf[JSONArray],"filters") map (_.values.toList map {
          case JSONArray(ls@_*) => (ls.toList map {
            case JSONString(s) => s
            case _ => throw new ParseError("GAP_Operation: filters is not an Array of Arrays of Strings!")
          }).filter(redfilter(name))
          case _ => throw new ParseError("GAP_Operation: filters is not an Array of JSONArrays!")
        })
        val methods = doMethods(obj.getAs(classOf[JSONObject],"methods"),name)
        allmethods :::= methods
        (ParsedOperation(name,aka,filters,methods,locations),List("filters","methods"))

      case "GAP_Attribute"| "Attribute" =>
        val filters = obj.getAsList(classOf[String],"filters").filter(redfilter(name))
        val methods = doMethods(obj.getAs(classOf[JSONObject],"methods"),name)
        allmethods :::= methods
        (ParsedAttribute(name,aka,filters,locations,methods),List("filters","methods"))

      case "GAP_Representation" =>
        val implied = obj.getAsList(classOf[String],"implied").filter(redfilter(name))
        (ParsedRepresentation(name,aka,implied,locations),List("implied"))

      case "GAP_Category" =>
        val implied = obj.getAsList(classOf[String],"implied").filter(redfilter(name))
        (ParsedCategory(name,aka,implied,locations),List("implied"))

      case "GAP_Filter" =>
        val implied = obj.getAsList(classOf[String],"implied").filter(redfilter(name))
        (ParsedFilter(name,aka,implied,locations),List("implied"))

      case "Constructor" =>
        val filters = obj.getAsList(classOf[String],"filters").filter(redfilter(name))
        val methods = doMethods(obj.getAs(classOf[JSONObject],"methods"),name)
        allmethods :::= methods
        (ParsedConstructor(name,aka,locations,filters,methods),List("filters","methods"))

      case "Function" =>
        // TODO?
        return
      case _ => throw new ParseError("Type not yet implemented: " + tp + " in " + obj)
    }
    val missings = obj.map.filter(p => !("name" :: "type" :: "aka" :: "location" :: "locations" :: eaten).contains(p._1.value))
    if (missings.nonEmpty) println("Type " + tp + " has additional fields " + missings) // throw new ParseError("Type " + tp + " has additional fields " + missings)
    // log("Added: " + ret)
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
    val rank = obj.getAsInt("rank").toInt
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
      case ParsedFilter(name,aka,implied,loc) =>
        new DeclaredFilter(LocalName(name),implied map deps,loc)
      case ParsedRepresentation(name,aka,implied,loc) =>
        new GAPRepresentation(LocalName(name),implied map deps,loc)
      case ParsedProperty(name,aka,implied,istrue,loc,methods) =>
        new DeclaredProperty(LocalName(name),implied map deps,istrue,loc)
      case ParsedAttribute(name,aka,filters,loc,methods) =>
        new DeclaredAttribute(LocalName(name), filters map deps,loc)
      case ParsedCategory(name,aka,implied,locations) =>
        new DeclaredCategory(LocalName(name), implied map deps,locations)
      case ParsedOperation(name,aka,filters,methods,locations) =>
        new DeclaredOperation(LocalName(name),filters map (_ map (_ map deps)),locations)
      case ParsedDefinedFilter(name,aka,conjof,location) =>
        new DefinedFilter(LocalName(name), conjof map deps,location)
      case ParsedConstructor(name, aka, location, filters, methods) =>
        new Constructor(LocalName(name),filters map deps,location)
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
    abstract class reg {
      def unapply(s : String) : Option[String]
    }
    object regTester extends reg {
      val reg = """Tester\((.+)\)""".r
      def unapply(s : String) : Option[String] = s match {
        case reg(r) => Some(r)
        case _ => None
      }
    }
    object regCatCollection extends reg {
      val reg = """CategoryCollections\((.+)\)""".r
      def unapply(s : String) : Option[String] = s match {
        case reg(r) => Some(r)
        case _ => None
      }
    }
    object regCatFamily extends reg {
      val reg = """CategoryFamily\((.+)\)""".r
      def unapply(s : String) : Option[String] = s match {
        case reg(r) => Some(r)
        case _ => None
      }
    }
    object regSetter extends reg {
      val reg = """Setter\((.+)\)""".r
      def unapply(s : String) : Option[String] = s match {
        case reg(r) => Some(r)
        case _ => None
      }
    }
    object regGet extends reg {
      val reg = """Get\((.+)\)""".r
      def unapply(s : String) : Option[String] = s match {
        case reg(r) => Some(r)
        case _ => None
      }
    }
    object regAnd {
      def unapply(s : String) : Option[(String,String)] = {
        if (!(s.startsWith("(") && s.endsWith(")"))) return None
        var i = 1
        var brackets = 0
        var start = ""
        var done = false
        while (i < s.length - 1 && !done) {
          if (s.substring(i).startsWith("(")) brackets += 1
          if (s.substring(i).startsWith(")")) brackets -= 1
          if (brackets == 0 && s.substring(i).startsWith(" and ")) {
            done = true
            i = i + 5
          } else {
            start = start + s(i)
            i += 1
          }
        }
        Some((start,s.substring(i).dropRight(1)))
      }
    }
    object regHas extends reg {
      val reg = """Has(.+)""".r
      def unapply(s : String) : Option[String] = s match {
        case reg(r) => Some(r)
        case _ => None
      }
    }
    object regSet extends reg {
      val reg = """Set(.+)""".r
      def unapply(s : String) : Option[String] = s match {
        case reg(r) => Some(r)
        case _ => None
      }
    }

    def inner(s : String) : String = s match {
      case regTester(r) => inner(r)
      case regCatCollection(r) => inner(r)
      case regCatFamily(r) => inner(r)
      case regSetter(r) => inner(r)
      case regHas(r) => inner(r)
      case regSet(r) => inner(r)
      case regGet(r) => inner(r)
      //case regAnd(r1,r2) => inner(r1) ::: inner(r2)
      case _ => s
    }

    def parse(s : String) : GAPObject = s match {
      case "IsBool" => IsBool
      case "IsObject" => IsObject
      case regTester(a) =>
        val ret = parse(a)
        ret match {
          case attr : GAPAttribute => Tester(attr)
          case _ => throw new ParseError("Expected GAPAttribute; returned " + ret)
        }
      case regCatFamily(a) => CategoryFamily(parse(a))
      case regCatCollection(a) => CategoryCollections(parse(a))
      case regSetter(a) => Setter(parse(a))
      case regHas(a) => Has(parse(a) match {
        case at : GAPAttribute => at
        case _ => throw new ParseError("Expected GAPAttribute; returned " + parse(a).getClass + ": " + a)
      })
      case regSet(a) => GapSet(parse(a))
      case regGet(a) => parse("Is" + a)
      case regAnd(r1,r2) =>
        val (ret1,ret2) = (parse(r1),parse(r2))
        val (f1,f2) = (ret1,ret2) match {
          case (a : GAPFilter, b : GAPFilter) => (a,b)
          case _ => throw ParseError("Not Filters: " + ret1 + " and " + ret2)
        }
        And(f1,f2)
      case "IS_SSORT_LIST" => parse("IsSortedList")
      case "IS_NSORT_LIST" => parse("IsNSortedList")
      case "IS_POSS_LIST" => parse("IsPositionsList")
      case "LENGTH" => parse("Length")
      case _ => getObject(all.find(_.name == s).getOrElse(all.find(_.aka contains s).getOrElse(
        throw new ParseError("GAP Object " + s + " not found"))))
    }
    val regs : List[reg] = List(regTester,regCatCollection,regSetter,regHas,regSet,regGet,regCatFamily)
  }

}

sealed abstract class GAPObject {
  def getInner : List[DeclaredObject]
  def toTerm : Term
}
sealed abstract class DeclaredObject extends GAPObject {
  val name : LocalName
  val locations : (String,Int)
  private lazy val steps = locations._1.replace("/home/makx/ac/gap/","").split("/").toList//.map(SimpleStep).toList
  private lazy val subpath : LocalName = if (steps.length>1) LocalName(steps.init.map(SimpleStep)) else LocalName("")
/*
  lazy val parent : MPath = {if (subpath != LocalName("")) GAP._base / subpath else GAP._base } ? {
    if (steps.nonEmpty) LocalName(steps.last.split("\\.").toList.head) else
      throw new ParseError("No theory name deducible from " + name)
  }
  lazy val path = parent ? name
  */
  lazy val parent : MPath = GAP.importbase ? "lib"
  lazy val path = parent ? name
  def getInner = List(this)
  def toTerm = OMS(path)

  val dependencies : List[DeclaredObject]
}
trait GAPFilter extends GAPObject
class DeclaredFilter(val name : LocalName, val implied : List[GAPObject], val locations : (String,Int))
  extends DeclaredObject with GAPFilter {
  override def toString = "DeclaredFilter " + name + " " + locations
  val dependencies = implied.flatMap(_.getInner).distinct
}

class DefinedFilter(val name : LocalName, val conjs : List[GAPObject], val locations : (String,Int)) extends DeclaredObject with GAPFilter {
  override def toString = "DefinedFilter " + name + " " + locations
  val dependencies = conjs.flatMap(_.getInner).distinct
  def defi : Option[Term] = if (conjs.isEmpty) {
    None
  } else if (conjs.length == 1) Some(Translator.objtotype(conjs.head)) else
    Some(conjs.init.foldRight(Translator.objtotype(conjs.last))((o,t) => GAP.termconj(Translator.objtotype(o),t)))
}

trait GAPCategory extends GAPFilter
class DeclaredCategory(val name : LocalName, val implied : List[GAPObject], val locations : (String,Int))
  extends DeclaredObject with GAPCategory {
  override def toString = "DeclaredCategory " + name + " " + locations
  val dependencies = implied.flatMap(_.getInner).distinct
}
class GAPRepresentation(val name : LocalName, val implied : List[GAPObject], val locations : (String,Int))
  extends DeclaredObject with GAPFilter {
  override def toString = "Representation " + name + " " + locations
  val dependencies = implied.flatMap(_.getInner).distinct
}
class GAPMethod(val operation: GAPOperation, val filters : List[List[GAPObject]], val comment : String, val rank : Int)
trait GAPOperation extends GAPAttribute
class DeclaredOperation(val name : LocalName, val filters : List[List[List[GAPObject]]],
                        val locations : (String,Int)) extends DeclaredObject with GAPOperation {

  override def toString = "DeclaredOperation " + name + " " + locations
  val arity : Option[Int] = None
  val dependencies = filters.flatten.flatten.flatMap(_.getInner).distinct
}
trait GAPAttribute extends GAPObject {
  val returntype : Option[GAPObject] = None
}
class DeclaredAttribute(val name : LocalName, val filters: List[GAPObject], val locations : (String,Int))
  extends DeclaredObject with GAPAttribute {
  override def toString = "DeclaredAttribute " + name + " " + locations
  val dependencies = filters.flatMap(_.getInner).distinct
}
class Constructor (val name : LocalName, val filters: List[GAPObject], val locations : (String,Int))
  extends DeclaredObject {
  override def toString: String = "Constructor " + name + " " + locations
  override val dependencies: List[DeclaredObject] = filters.flatMap(_.getInner).distinct
  val returntype : Option[GAPObject] = None
}
trait GAPProperty extends GAPAttribute
class DeclaredProperty(name : LocalName, implied : List[GAPObject], val istrue : Boolean, locations : (String,Int))
  extends DeclaredAttribute(name,implied,locations) with GAPProperty {
  override def toString = "DeclaredProperty " + name + " " + locations
}

object IsBool extends DeclaredCategory(LocalName("IsBool"),Nil,("",0)) {
  override lazy val path = GAP.IsBool
}
object IsObject extends DeclaredCategory(LocalName("IsObject"),Nil,("",0)) {
  override lazy val path = GAP.IsObject
}
case class CategoryCollections(obj: GAPObject) extends GAPCategory {
  def getInner = obj.getInner
  def toTerm = Apply(GAP.catcollection,Translator.objtotype(obj))
}
case class CategoryFamily(obj : GAPObject) extends GAPCategory {
  def getInner = obj.getInner
  def toTerm = Apply(GAP.catfamily,Translator.objtotype(obj))
}
object Setter {
  def apply(obj : GAPObject) = GapSet(obj)
}
case class GapSet(obj: GAPObject) extends GAPOperation {
  def getInner = obj.getInner
  def toTerm = Apply(GAP.setter,Translator.objtotype(obj))
}
object Tester {
  def apply(obj : GAPAttribute) = Has(obj)
}
case class Has(obj: GAPAttribute) extends GAPFilter {
  def getInner = obj.getInner
  def toTerm = Apply(GAP.has,obj.toTerm)
}

case class And(obj1 : GAPFilter, obj2 : GAPFilter) extends GAPFilter {
  def getInner = deconjoin.flatMap(_.getInner).distinct
  //def toTerm = GAP.conj(obj1.toTerm,obj2.toTerm)
  val deconjoin : List[GAPFilter] = (obj1 match {
    case and @ And(a,b) => and.deconjoin
    case _ => List(obj1)
  }) ::: (obj2 match {
    case and @ And(a,b) => and.deconjoin
    case _ => List(obj2)
  })
  def toTerm = deconjoin.init.foldRight[Term](Translator.objtotype(deconjoin.last))((o,t) => GAP.termconj(
    Translator.objtotype(o),t))
}
