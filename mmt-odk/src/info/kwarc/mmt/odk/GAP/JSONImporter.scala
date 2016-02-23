package info.kwarc.mmt.odk.GAP

import info.kwarc.mmt.api.archives.{RedirectableDimension, BuildResult, BuildTask, Importer}
import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.{ParseError, utils}
import info.kwarc.mmt.api.utils._

import scala.util.Try

object JSONImporter extends Importer {
  val key = "gap-omdoc"
  def inExts = List("json")
  override def inDim = RedirectableDimension("gap")
  val reader = new GAPReader
  // reader.export = 100
  // reader.file = Some(File("/home/raupi/lmh/MathHub/ODK/GAP/gap/bitesize.json"))

  def importDocument(bf: BuildTask, index: Document => Unit): BuildResult = {
    //     if (bf.inFile.filepath.toString < startAt) return
    val d = bf.inFile.name
    val e = try {
      println("reading...")
      val read = File.read(bf.inFile) // .replace("\\\n","")

      reader(read)
    } catch {
      case utils.ExtractError(msg) =>
        println(msg)
        sys.exit
    }
    println(reader.all.length + " Objects parsed")

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
    val conv = new Translator(controller, bf, index)
    conv(reader)
    BuildResult.empty
  }
}

abstract class GAPObject {
  val name: String
  val dependencies: List[String]
  private var deps: Set[GAPObject] = Set()
  private var computed = false
  private var depvalue = -1

  def getInner = this

  private val reg1 = """Tester\((\w+)\)""".r
  private val reg2 = """CategoryCollections\((\w+)\)""".r

  def implications(all: List[GAPObject]): Set[GAPObject] = {
    if (!computed) {
      def fromName(s: String): GAPObject = s match {
        case reg1(s2) => Tester(fromName(s2))
        case reg2(s2) => CategoryCollections(fromName(s2))
        case _ => all.find(_.name == s).getOrElse {
          throw new ParseError("GAP Object not found in import: " + s)
        }
      }
      deps = dependencies.map(ref =>
        if (ref == "<<unknown>>") this
        else {
          try {
            fromName(ref)
          } catch {
            case ParseError(s) =>
              println(s + " in " + ref)
              this
            //throw new ParseError(s + " in " + ref)

          }
        }
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

case class GAPProperty(name : String, implied : List[String], isTrue :Boolean = false) extends GAPObject {
  val dependencies = implied
}
case class GAPOperation(name : String, filters : List[List[List[String]]], methods : List[JSONObject]) extends GAPObject {
  val dependencies : List[String] = filters.flatMap(_.flatten)
}
case class GAPCategory(name : String, implied : List[String]) extends GAPObject {
  val dependencies = implied
}
case class GAPAttribute(name : String, filters : List[String]) extends GAPObject {
  val dependencies = filters
}
// case class GAPTester( name : String, implied : List[String]) extends GAPObject
case class GAPRepresentation(name : String, implied : List[String]) extends GAPObject {
  val dependencies = implied
}
case class GAPFilter(name : String, implied : List[String]) extends GAPObject {
  val dependencies = implied
}

case class Tester(obj : GAPObject) extends GAPObject {
  val name = obj.name + ".tester"
  val dependencies = List()
  override def getInner = obj.getInner
  override def toString = name
}
case class CategoryCollections(obj: GAPObject) extends GAPObject {
  val name = obj.name + ".catcollection"
  val dependencies = List()
  override def getInner = obj.getInner
  override def toString = name
}

class GAPReader {

  var properties : List[GAPProperty] = Nil
  var categories : List[GAPCategory] = Nil
  var attributes : List[GAPAttribute] = Nil
  // var tester : List[GAPTester] = Nil
  var representations : List[GAPRepresentation] = Nil
  var filter : List[GAPFilter] = Nil
  var operations : List[GAPOperation] = Nil

  def all = (properties ++ operations ++ attributes ++ representations ++ categories ++ filter).distinct //++ categories ++ attributes ++ tester ++ representations ++ filter).distinct

  private val reg1 = """Tester\((\w+)\)""".r
  private val reg2 = """CategoryCollections\((\w+)\)""".r
  var export = 0
  var file : Option[File] = None
  private var counter = 0
  private var exportstr : List[JSON] = Nil

  private def convert(j : JSON) {
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
        name match {
          case reg1(s) => return
          case reg2(s) => return
          case _ =>
        }
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
          properties      ::= GAPProperty(name,impls)
          val missings = obj.map.filter(p => p._1.value!="name" && p._1.value!="implied" && p._1.value!="type"
          && p._1.value!="filters")
          if (missings.nonEmpty) throw new ParseError("Type " + tp + " has additional fields " + missings)
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
          val methods = obj("methods").toList.map(_ match {
            case o: JSONObject => o
            case _ => throw new ParseError("Method in Operation not a JSONObject: " + obj)
          })
          operations      ::= GAPOperation(name,filters,methods)
          val missings = obj.map.filter(p => p._1.value!="name" && p._1.value!="filters" && p._1.value!="type"
          && p._1.value!="methods")
          if (missings.nonEmpty) throw new ParseError("Type " + tp + " has additional fields " + missings)
        }
        else if (tp == "GAP_Attribute") {
          val filters = obj("filters") match {
            case Some(l: JSONArray) => l.values.toList match {
              case ls: List[JSONString] => ls.map(_.value)
              case _ => throw new ParseError("filters not a List of JSONStrings: " + obj + "\n" + l.values.getClass)
            }
            case _ => throw new ParseError("filters missing in " + obj)
          }
          attributes      ::= GAPAttribute(name,filters)
          val missings = obj.map.filter(p => p._1.value!="name" && p._1.value!="filters" && p._1.value!="type")
          if (missings.nonEmpty) throw new ParseError("Type " + tp + " has additional fields " + missings)
        }
        else if (tp == "GAP_Representation") {
          val impls = obj("implied") match {
            case Some(l: JSONArray) => l.values.toList match {
              case ls: List[JSONString] => ls.map(_.value)
              case _ => throw new ParseError("implied not a List of JSONStrings: " + obj + "\n" + l.values.getClass)
            }
            case _ => throw new ParseError("implied missing in " + obj)
          }
          representations      ::= GAPRepresentation(name,impls)
          val missings = obj.map.filter(p => p._1.value!="name" && p._1.value!="implied" && p._1.value!="type")
          if (missings.nonEmpty) throw new ParseError("Type " + tp + " has additional fields " + missings)
        }
        else if (tp == "GAP_Category") {
          val impls = obj("implied") match {
            case Some(l: JSONArray) => l.values.toList match {
              case ls: List[JSONString] => ls.map(_.value)
              case _ => throw new ParseError("implied not a List of JSONStrings: " + obj + "\n" + l.values.getClass)
            }
            case _ => throw new ParseError("implied missing in " + obj)
          }
          categories      ::= GAPCategory(name,impls)
          val missings = obj.map.filter(p => p._1.value!="name" && p._1.value!="implied" && p._1.value!="type")
          if (missings.nonEmpty) throw new ParseError("Type " + tp + " has additional fields " + missings)
        }
        else if (tp == "GAP_Filter") {
          val impls = obj("implied") match {
            case Some(l: JSONArray) => l.values.toList match {
              case ls: List[JSONString] => ls.map(_.value)
              case _ => throw new ParseError("implied not a List of JSONStrings: " + obj + "\n" + l.values.getClass)
            }
            case _ => throw new ParseError("implied missing in " + obj)
          }
          filter      ::= GAPFilter(name,impls)
          val missings = obj.map.filter(p => p._1.value!="name" && p._1.value!="implied" && p._1.value!="type")
          if (missings.nonEmpty) throw new ParseError("Type " + tp + " has additional fields " + missings)
        }
        else throw new ParseError("Type not yet implemented: " + tp + " in " + obj)
        /*
        val impls = obj("implied") match {
          case Some(l: JSONArray) => l.values.toList match {
            case ls: List[JSONString] => ls.map(_.value)
            case _ => throw new ParseError("implied not a List of JSONStrings: " + obj + "\n" + l.values.getClass)
          }
          case _ => obj("filters") match {
            case Some(l: JSONArray) => l.values.toList match {
              case ls: List[JSONString] => ls.map(_.value)
              case _ => throw new ParseError("filters not a List of JSONStrings: " + obj + "\n" + l.values.getClass)
            }
            case _ => throw new ParseError("implied/filters missing or not a JSONArray: " + obj)
          }
        }
        if (tp=="GAP_Property")             properties      ::= GAPProperty(name,impls)
        else if (tp=="GAP_TrueProperty")    properties      ::= GAPProperty(name,impls,true)
        else if (tp=="GAP_Category")        categories      ::= GAPCategory(name,impls)
        else if (tp=="GAP_Attribute")       attributes      ::= GAPAttribute(name,impls)
        else if (tp=="GAP_Tester")          tester          ::= GAPTester(name,impls)
        else if (tp=="GAP_Representation")  representations ::= GAPRepresentation(name,impls)
        else if (tp=="GAP_Filter")          filter          ::= GAPFilter(name,impls)
        else throw new ParseError("Type not yet implemented: " + tp + "in" + obj)


        */
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

  def apply(read:String): Unit = {
    println("JSON Parsing...")

    val parsed = JSON.parse(read)
    println("ToScala...")
    apply(parsed)
  }

}