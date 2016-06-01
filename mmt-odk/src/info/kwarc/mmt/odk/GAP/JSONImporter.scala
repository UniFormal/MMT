package info.kwarc.mmt.odk.GAP

import info.kwarc.mmt.api.archives.{BuildResult, BuildTask, Importer, RedirectableDimension}
import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.frontend.Logger
import info.kwarc.mmt.api.objects.{OMS, Term}
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.lf.Apply

import scala.util.Try

class JSONImporter extends Importer {
  def toplog(s : => String) = log(s)
  def toplogGroup[A](a : => A) = logGroup(a)
  val reporter = this.report
  val key = "gap-omdoc"
  def inExts = List("json")
  override def logPrefix = "gap"
  override def inDim = RedirectableDimension("gap")
  lazy val reader = new GAPReader(this)
  // reader.export = 100
  // reader.file = Some(File("/home/raupi/lmh/MathHub/ODK/GAP/gap/bitesize.json"))

  def importDocument(bf: BuildTask, index: Document => Unit): BuildResult = {
    //     if (bf.inFile.filepath.toString < startAt) return
    val d = bf.inFile.name
    val e = try {
      log("reading...")
      val read = File.read(bf.inFile) // .replace("\\\n","")

      reader(read)
    } catch {
      case utils.ExtractError(msg) =>
        println("utils.ExtractError")
        println(msg)
        sys.exit
    }
    log(reader.all.length + " Objects parsed")

    /*
    val conv = new PVSImportTask(controller, bf, index)
    e match {
      case d: pvs_file =>
        conv.doDocument(d)
      case m: syntax.Module =>
        conv.doDocument(pvs_file(List(m)))
      //conv.doModule(m)
    }
    */
    val conv = new Translator(controller, bf, index,this)
    conv(reader)
    BuildResult.empty
  }
}

class GAPReader(log : JSONImporter) {

  var properties : List[GAPProperty] = Nil
  var categories : List[GAPCategory] = List(IsBool)
  var attributes : List[GAPAttribute] = Nil
  // var tester : List[GAPTester] = Nil
  var representations : List[GAPRepresentation] = Nil
  var filter : List[GAPFilter] = Nil
  var operations : List[GAPOperation] = Nil

  def all = (properties ++ operations ++ attributes ++ representations ++ categories ++ filter).distinct //++ categories ++ attributes ++ tester ++ representations ++ filter).distinct

  var export = 0
  var file : Option[File] = None
  private var counter = 0
  private var exportstr : List[JSON] = Nil

  private def doMethods(j : JSONObject, op : String) : List[GAPMethod] = {
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

  private def doMethod(j : JSONObject, op : String) : GAPMethod = {
    val comment = j("comment").getOrElse("") match {
      case JSONString(s) => s
      case _ => throw new ParseError("comment not a JSONString:" + j)
    }
    val filters = j("filters").getOrElse(throw new ParseError("filters in method not found: " + j)) match {
      case seq : JSONArray => seq.toList.map(_ match {
        case ls : JSONArray => ls.toList.map(s => s match {
          case JSONString(flt) => flt
          case _ => throw new ParseError("filter " + s + " in method not a JSONString: " + j)
        })
        case _ => throw new ParseError("filters in method not a JSONArray: " + j)
      })
      case _ => throw new ParseError("filters in method not a JSONArray: " + j)
    }
    val rank = j("rank").getOrElse(JSONInt(0)) match {
      case JSONInt(i) => i
      case _ => throw new ParseError("rank of method not an Integer: " + j)
    }
    GAPMethod(op,filters,comment,rank)
  }

  private def convert(j : JSON) : Unit = log.toplogGroup {
    j match {
      case obj : JSONObject =>
        if (file.isDefined) {
          if (counter==export) {
            counter = 0
            exportstr::=j
          } else counter+=1
        }
        val name = obj("name") match {
          case Some(s: JSONString) => s.value
          case _ => throw new ParseError("Name missing or not a JSONString: " + obj)
        }
        reg.regs foreach {r =>
          name match {
            case r(s) => return ()
            case _ =>
          }
        }
        if (name == "IsBool") return ()

        val tp = obj("type") match {
          case Some(s : JSONString) => s.value
          case _ => throw new ParseError("Type missing or not a JSONString: " + obj)
        }
        if (tp == "GAP_Property") {
          val impls = Try(obj("implied") match {
            case Some(l: JSONArray) => l.values.toList map (_ match {
              case s:JSONString => s.value
              case _ => throw new ParseError("implied not a List of JSONStrings: " + obj + "\n" + l.values.getClass)
            })
            case _ => throw new ParseError("implied missing in " + obj)
          }).getOrElse(obj("filters") match {
            case Some(l: JSONArray) => l.values.toList map (_ match {
              case s:JSONString => s.value
              case _ => throw new ParseError("filters not a List of JSONStrings: " + obj + "\n" + l.values.getClass)
            })
            case _ => throw new ParseError("filters missing in " + obj)
          })
          val missings = obj.map.filter(p => p._1.value!="name" && p._1.value!="implied" && p._1.value!="type"
          && p._1.value!="filters")
          if (missings.nonEmpty) throw new ParseError("Type " + tp + " has additional fields " + missings)
          val ret = GAPProperty(name,impls)
          log.toplog("Added: " + ret)
          properties      ::= ret
        }
        else if (tp == "GAP_Operation") {
          val filters = obj("filters") match {
            case Some(l: JSONArray) => l.values.toList.map(_ match {
              case a: JSONArray => a.values.toList.map(_ match {
                case b : JSONArray => b.values.toList.map(_ match {
                  case s : JSONString => s.value
                  case _ => throw new ParseError("filter illdefined in Operation: " + obj)
                })
                case _ => throw new ParseError("filter illdefined in Operation: " + obj)
              })
              case _ => throw new ParseError("filter illdefined in Operation: " + obj)
            })
            case _ => throw new ParseError("filters missing in " + obj)
          }
          val methods = obj("methods") match {
            case Some(o: JSONObject) => doMethods(o,name)
            case _ => throw new ParseError("Method in Operation not a JSONObject: " + obj)
          }
          val locations = obj("locations") match {
            case Some(ls : JSONArray) =>
              val list = ls.values
              if (list.length > 1) throw new ParseError("Several loations in Operation: " + name + ": " + list)
              else ls.head match {
                case o : JSONObject =>
                  (o("file"),o("line")) match {
                    case (Some(JSONString(fl)),Some(JSONInt(i))) => (fl,i)
                    case _ => throw new ParseError("Locations in Operation ill-formed: " + obj)
                  }
                case _ => throw new ParseError("Location head in Operation not a JSONObject: " + obj)
              }
            case _ => throw new ParseError("Locations in Operation not a JSONArray: " + obj)
          }
          val missings = obj.map.filter(p => p._1.value!="name" && p._1.value!="filters" && p._1.value!="type"
          && p._1.value!="methods" && p._1.value!="locations")
          if (missings.nonEmpty) throw new ParseError("Type " + tp + " has additional fields " + missings)
          val ret = GAPOperation(name,filters,methods,locations)
          log.toplog("Added: " + ret)
          operations      ::= ret
        }
        else if (tp == "GAP_Attribute") {
          val filters = obj("filters") match {
            case Some(l: JSONArray) => l.values.toList match {
              case ls: List[JSONString] => ls.map(_.value)
              case _ => throw new ParseError("filters not a List of JSONStrings: " + obj + "\n" + l.values.getClass)
            }
            case _ => throw new ParseError("filters missing in " + obj)
          }
          val missings = obj.map.filter(p => p._1.value!="name" && p._1.value!="filters" && p._1.value!="type")
          if (missings.nonEmpty) throw new ParseError("Type " + tp + " has additional fields " + missings)
          val ret = GAPAttribute(name,filters)
          log.toplog("Added: " + ret)
          attributes      ::= ret
        }
        else if (tp == "GAP_Representation") {
          val impls = obj("implied") match {
            case Some(l: JSONArray) => l.values.toList match {
              case ls: List[JSONString] => ls.map(_.value)
              case _ => throw new ParseError("implied not a List of JSONStrings: " + obj + "\n" + l.values.getClass)
            }
            case _ => throw new ParseError("implied missing in " + obj)
          }
          val missings = obj.map.filter(p => p._1.value!="name" && p._1.value!="implied" && p._1.value!="type")
          if (missings.nonEmpty) throw new ParseError("Type " + tp + " has additional fields " + missings)
          val ret = GAPRepresentation(name,impls)
          log.toplog("Added: " + ret)
          representations      ::= ret
        }
        else if (tp == "GAP_Category") {
          val impls = obj("implied") match {
            case Some(l: JSONArray) => l.values.toList match {
              case ls: List[JSONString] => ls.map(_.value)
              case _ => throw new ParseError("implied not a List of JSONStrings: " + obj + "\n" + l.values.getClass)
            }
            case _ => throw new ParseError("implied missing in " + obj)
          }
          val missings = obj.map.filter(p => p._1.value!="name" && p._1.value!="implied" && p._1.value!="type")
          if (missings.nonEmpty) throw new ParseError("Type " + tp + " has additional fields " + missings)
          val ret = GAPCategory(name,impls)
          log.toplog("Added: " + ret)
          categories      ::= ret
        }
        else if (tp == "GAP_Filter") {
          val impls = obj("implied") match {
            case Some(l: JSONArray) => l.values.toList match {
              case ls: List[JSONString] => ls.map(_.value)
              case _ => throw new ParseError("implied not a List of JSONStrings: " + obj + "\n" + l.values.getClass)
            }
            case _ => throw new ParseError("implied missing in " + obj)
          }
          val missings = obj.map.filter(p => p._1.value!="name" && p._1.value!="implied" && p._1.value!="type")
          if (missings.nonEmpty) throw new ParseError("Type " + tp + " has additional fields " + missings)
          val ret = GAPFilter(name,impls)
          log.toplog("Added: " + ret)
          filter      ::= ret
        }
        else throw new ParseError("Type not yet implemented: " + tp + " in " + obj)
      case _ => throw new ParseError("Not a JSON Object: " + j)
    }
  }

  def apply(json: JSON) {
    json match {
      case obj : JSONArray =>
        obj.values.foreach(p => convert(p))// try { convert(p) } catch {case e:Throwable => println("Failure in " + p + "\n" + e.getMessage)})
        if (file.isDefined) File.write(file.get,JSONArray(exportstr:_*).toString)
      case _ => throw new ParseError("Not a JSON Object")
    }
  }

  def apply(read:String): Unit = log.toplogGroup {
    log.toplog("JSON Parsing...")

    val parsed = JSON.parse(read)
    log.toplog("ToScala...")
    apply(parsed)
  }

}

object reg {
  val regTester = """Tester\((\w+)\)""".r
  val regCatCollection = """CategoryCollections\((\w+)\)""".r
  val regSetter = """Setter\((\w+)\)""".r
  val regHas = """Has(\w+)""".r
  val regSet = """Set(\w+)""".r
  val regGet = """Get\((\w+)\)""".r

  def parse(s : String)(implicit allobjs : List[GAPObject]) : GAPObject = s match {
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
    case _ => allobjs.find(_.namestr == s).getOrElse(throw new ParseError("GAPObject " + s + " not found!"))

  }

  val regs = List(regTester,regCatCollection,regSetter,regHas,regSet,regGet)
}

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
  override def toString = "Operation " + name + ": " + filterstr.map(l => " - " + l.map(_.toString).mkString(",")).mkString("\n") +
    methods.mkString("\n  ")

  val arity : Option[Int] = if (methods.isEmpty && filterstr.forall(_.forall(_.isEmpty))) Some(0)
    else if (methods.nonEmpty && methods.forall(_.arity == methods.head.arity)) Some(methods.head.arity) else
    throw new ParseError("Arity doesn't match! " + toString)

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
  override def toString = "Category " + name + ": " + impliedstr.mkString(",")

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