package info.kwarc.mmt.api.utils

import info.kwarc.mmt.api.ParseError

import scala.collection.mutable
import scala.util.Try

/**
 * straightforward API for JSON objects
 */
sealed abstract class JSON extends ScalaTo {
  override def toString: String = toCompactString

  def toJSON = this
  def toXML = JSONXML.jsonToXML(this)

  /** turns this JSON Object into a compact (no-spaces) string */
  def toCompactString: String = toFormattedString("")

  /** turns this JSON object into a formatted string */
  def toFormattedString(indent: String): String
  
  /** derefences a path in a JSON element */
  def apply(selectors: JSON.Selector*): Option[JSON] = {
    val oneStep = selectors.headOption match {
      case None => return Some(this)
      case Some(s) => (this,s) match {
        case (j: JSONObject, Left(s)) => j(s)
        case (j: JSONArray, Right(i)) => if (i >= 0 && i < j.values.length) Some(j(i)) else None
        case _ => None
      }
    }
    oneStep flatMap {j => j(selectors.tail:_*)}
  }
}

case object JSONNull extends JSON {
  def toFormattedString(indent: String): String = "null"
}

sealed abstract class JSONValue(val tp: ConcreteBaseType) extends JSON {
   def value: Any
   def toFormattedString(indent: String): String = value.toString
}

case class JSONInt(value: BigInt) extends JSONValue(IntType)
case class JSONFloat(value: BigDecimal) extends JSONValue(FloatType)
case class JSONBoolean(value: Boolean) extends JSONValue(BooleanType)
case class JSONString(value: String) extends JSONValue(StringType) {
  override def toFormattedString(indent: String): String = {
    val escaped = JSON.quoteString(value) //also escapes / to \/ which is allowed but weird
    "\"" + escaped + "\""
  }
}

case class JSONArray(values: JSON*) extends JSON {
  def toFormattedString(indent: String): String = {
    val (start, sep, end) = {
      if(indent != ""){
        ("[\n"+indent, ",\n"+indent, "\n]")
      } else {
        ("[", ",", "]")
      }
    }
    values.map( v =>
      JSON.addIndent(v.toFormattedString(indent), indent)
    ).mkString(start, sep, end)
  }
  
  // needed for Python bridge
  def iterator = values.iterator
  
  def apply(i: Int) = values(i)
}

object JSONArray {
   implicit def toList(j: JSONArray): List[JSON] = j.values.toList 
}


case class JSONObject(map: List[(JSONString, JSON)]) extends JSON {
  def toFormattedString(indent: String): String = {
    val (start, sep, end) = {
      if(indent != ""){
        ("{\n"+indent, ",\n"+indent, "\n}")
      } else {
        ("{", ",", "}")
      }
    }
    val spaces = if (indent != "") " " else ""
    map.map { case (k, v) =>
      k.toFormattedString(indent) + ":" + spaces + JSON.addIndent(v.toFormattedString(indent), indent)
    }.mkString(start, sep, end)
  }

  def apply(s: JSONString): Option[JSON] = map.find(_._1 == s).map(_._2)
  def apply(s: String): Option[JSON] = apply(JSONString(s))

  // needed for Python bridge
  def iterator = map.iterator.map(_._1)
  
  def getAs[A](cls: Class[A], s : String) : A = {
    val ret = apply(s).getOrElse(throw new ParseError("Field \"" + s + "\" not defined in JSONObject " + this)) match {
      case j : JSONValue => j.value
      case other => other
    }
    ret match {
      case j: A@unchecked if cls.isInstance(j) => j
      case _ => throw new ParseError("getAs Error: A=" + A.toString + ", j:" + ret.getClass)
    }
  }
  def getAsString(s : String) = try {
    getAs(classOf[String], s)
  } catch {
    case e : Exception => ""
  }
  def getAsInt(s : String) : BigInt = apply(s).getOrElse(throw new ParseError("Field \"" + s + "\" not defined in JSONObject " + this)) match {
    case j: JSONInt => j.value
    case _ => throw ParseError("Field \"" + s + "\" is not a JSONInt in " + this)
  }
  def getAsFloat(s : String) : BigDecimal = apply(s).getOrElse(throw new ParseError("Field \"" + s + "\" not defined in JSONObject " + this)) match {
    case j: JSONFloat => j.value
    case _ => throw ParseError("Field \"" + s + "\" is not a JSONInt in " + this)
  }
  def getAsList[A](cls: Class[A],s : String) : List[A] = {
    val ret = apply(s).getOrElse(throw ParseError("Field \"" + s + "\" not defined in JSONObject " + this)) match {
      case j : JSONArray => j.values.toList
      case _ => throw ParseError("Field \"" + s + "\" not da JSONArray " + this)
    }
    ret map {
      case j : JSONValue if cls.isInstance(j.value) => j.value match {
        case v: A@unchecked => v
        case _ => throw new Exception("Impossible")
      }
      case j : A@unchecked if cls.isInstance(j) => j
      case j => throw ParseError("getAs Error: A=" + cls.toString + ", j:" + j.getClass + " = " + j)
    }
  }
}

object JSONObject {
   def apply(cases: (String,JSON)*): JSONObject = JSONObject(cases.toList map {case (k,v) => (JSONString(k),v)})
   implicit def toList(j: JSONObject): List[(JSONString,JSON)] = j.map
}

object JSONConversions {
   implicit def fromString(s: String) = JSONString(s)
   implicit def fromInt(i: Int) = JSONInt(i)
   implicit def fromList(l: List[(JSONString,JSON)]) : JSONObject = JSONObject(l)
   implicit def fromList(l: List[JSON]) = JSONArray(l :_*)
   implicit def fromConvertibleList[A <% JSON](l: List[A]) = JSONArray(l.map(implicitly[A => JSON]):_*)
}

object JSON {
   // for quick testing
   /*
   def main(args: Array[String]) {
      val j = JSON.parse(""" { "xx":["a", 1, -5, 1.23, -2.5E-5, true, false, null],
        "bla" : "\\\\", "foo" : [{} ], "fooo": []} """)
      println(j)
   }*/

   case class JSONError(s: String) extends java.lang.Exception(s)

   /** to select a field in an JSONObject or a value in a JSONArray */
   type Selector = Union[String,Int]
   
   def parse(s: String) : JSON = {
      val u = new Unparsed(s, msg => throw JSONError(msg))
      val j = parse(u)
      u.trim
      if (u.empty)
         j
      else throw JSONError("additional characters after successful parse")
   }

   def parse(s: Unparsed) : JSON = {
      val c = s.trim.head
      c match {
         case 'n' => parseNull(s)
         case 't'|'f' => parseBoolean(s)
         case nc if nc == '-' || nc.isDigit => parseNum(s)
         case '"' => parseString(s)
         case '{' => parseObject(s)
         case '[' => parseArray(s)
         case 'I' => parseNum(s)
         case _ =>
           throw JSONError("Illegal starting character for JSON: " + c + " in " + s.remainder.subSequence(0,200))
      }
   }


   def parseNull(s: Unparsed) = {
      s.drop("null")
      JSONNull
   }

   def parseBoolean(s: Unparsed): JSONBoolean = {
      val b = if (s.head == 'f') {
         s.drop("false")
         false
      } else {
         s.drop("true")
         true
      }
      JSONBoolean(b)
   }

   def parseNum(s: Unparsed): JSONValue = {
      val e = "(?: e|e\\+|e-|E|E\\+|E-)"
      val mt = s.takeRegex(s"(-)?(\\d+)(\\.\\d+)?($e\\d+)?")
      val List(sgn, main, frac, exp) = mt.subgroups
      val jn = if (frac == null && exp == null) {
         val f = if (sgn == null) 1 else -1
         JSONInt(f * BigInt(main))
      } else
         JSONFloat(BigDecimal(mt.matched))
      jn
   }

  def parseString(s : Unparsed) = {
    s.drop("\"")
    val sb = new mutable.StringBuilder()
    var instring = true
    while(instring) {
      if (s.empty)
        throw JSONError("unclosed string")
      s.next() match {
        case '\\' =>
          if (s.empty)
            throw JSONError("unclosed escaped")
          s.next() match {
            case n@('"' | '\\' | '/') => sb.addOne(n)
            case 'b' => sb.addOne('\b')
            case 'f' => sb.addOne('\f')
            case 'n' => sb.addOne('\n')
            case 'r' => sb.addOne('\r')
            case 't' => sb.addOne('\t')
            case 'u' =>
              val hex = s.getnext(4).toString
              s.drop(4)
              val char = Integer.parseInt(hex, 16).toChar
              sb.addOne(char)
            case c => throw JSONError("Illegal starting character for JSON string escape: " + c)
          }
        case '"' => instring = false
        case o => sb.addOne(o)
      }
    }
    JSONString(sb.toString())
  }
/*
   def parseString(s: Unparsed) = {
     s.drop("\"")
     val (p,closed) = s.takeUntilChar('"', '\\')
     if (!closed)
       throw JSONError("unclosed string")
      var escaped = p
      var unescaped = ""
      while (escaped.nonEmpty) {
        val first = escaped(0)
        val (next, length) = if (first != '\\') {
          (first.toString,1)
        } else {
            if (escaped.length <= 1)
               throw JSONError("unclosed escaped")
             val second = escaped(1)
          second match {
               case '"' | '\\' | '/' => (second.toString, 2)
               case 'b' => ("\b", 2)
               case 'f' => ("\f", 2)
               case 'n' => ("\n", 2)
               case 'r' => ("\r", 2)
               case 't' => ("\t", 2)
               case 'u' =>
                 val hex = escaped.substring(2,6)
                 val char = Integer.parseInt(hex, 16).toChar
                 (char, 6)
               case _ => throw JSONError("Illegal starting character for JSON string escape: " + escaped(1))
            }
        }
         unescaped += next
       escaped = escaped.substring(length)
      }
     JSONString(unescaped)
   }
*/
   def parseObject(s: Unparsed): JSONObject = {
      s.trim
      s.drop("{")
      parseOpenObject(s,Nil)
   }

   def parseOpenObject(s: Unparsed, seen: List[(JSONString,JSON)]): JSONObject = {
      s.trim
      if (s.head == '}') {
         s.drop("}")
         JSONObject(seen.reverse)
      } else {
         s.trim
         val key = parseString(s)
         s.trim
         s.drop(":")
         val value = parse(s)
         s.trim
         val first = (key,value)
         val c = s.head
         if (c == ',') {
            s.next
         }
         if (c != ',' && c != '}')
            throw JSONError("expected ',' or '}', found " + c + " ")
       parseOpenObject(s, first::seen)
      }
   }

   def parseArray(s: Unparsed): JSONArray = {
      s.trim
      s.drop("[")
      parseOpenArray(s, Nil)
   }
   def parseOpenArray(s: Unparsed, seen: List[JSON]): JSONArray = {
      s.trim
      if (s.head == ']') {
         s.drop("]")
         JSONArray(seen.reverse:_*)
      } else {
         val first = parse(s)
         s.trim
         val c = s.head
         if (c == ',') {
          s.next
        }
         if (c != ',' && c != ']')
          throw JSONError("expected ',' or ']', found " + c)
         parseOpenArray(s, first::seen)
      }
   }
   def addIndent(formatted: String, indent: String): String = {
     formatted.split("\n").zipWithIndex
       .map(si => {
         if(si._2 > 0){
           indent + si._1
         } else {
           si._1
         }
       }).mkString("\n")
   }
  /**
    * This function can be used to properly quote Strings for JSON output.
    *
    * adapted from JSONFormat.quoteString in Scala 2.12.4; which has now been deprecated
    * but licensed under BSD 3-Clause Scala License <https://www.scala-lang.org/license/>.
    */
  def quoteString(s : String): String = s.map {
    case '"'  => "\\\""
    case '\\' => "\\\\"
    /*case '/'  => "\\/" redundant */
    case '\b' => "\\b"
    case '\f' => "\\f"
    case '\n' => "\\n"
    case '\r' => "\\r"
    case '\t' => "\\t"
    /* We'll unicode escape any control characters. These include:
     * 0x0 -> 0x1f  : ASCII Control (C0 Control Codes)
     * 0x7f         : ASCII DELETE
     * 0x80 -> 0x9f : C1 Control Codes
     *
     * Per RFC4627, section 2.5, we're not technically required to
     * encode the C1 codes, but we do to be safe.
     */
    case c if (c >= '\u0000' && c <= '\u001f') || (c >= '\u007f' && c <= '\u009f') => "\\u%04x".format(c.toInt)
    case c => c
  }.mkString
}

/** retrieve JSON from a URL */
object JSONFromURL {
  def apply(url: String) : Option[JSON] = {
    println(s"getting json from ${url}")
    Try(io.Source.fromURL(url)).toOption.map { a =>
      Try(JSON.parse(a.toBuffer.mkString)) match {
        case util.Failure(t: Throwable) =>
          throw ParseError(url.toString).setCausedBy(t)
        case util.Success(j) => j
      }
    }
  }
}

/** converts between XML and JSON following the jsonML specification */
object JSONXML {
  import scala.xml._
  
  /** encodes xml elements as JSON */
  def xmlToJSON(e: Node): JSON = {
    e match {
      case e: Elem =>
        val atts = e.attributes.asAttrMap.iterator map {case (k,v) => (JSONString(k), JSONString(v))} 
        JSONArray(JSONString(e.label) :: JSONObject(atts.toList) :: (e.child.toList map xmlToJSON) :_*)
      case e: Text => JSONString(e.text)
    }
  }
  
  /** inverse of xmlToJSON (partial) */
  def jsonToXML(j: JSON): Node = {
    j match {
      case JSONArray(JSONString(l), JSONObject(atts), child @_*) =>
        var attsX: MetaData = Null
        atts foreach {
          case (JSONString(k), v: JSONValue) =>
            attsX = new UnprefixedAttribute(k,v.toString, attsX)
          case _ => throw JSON.JSONError(s"attribute value must be JSON values")
        }
        val childX = child map jsonToXML
        Elem(null, l, attsX, TopScope, childX.isEmpty, childX:_*)
      case j: JSONValue => Text(j.value.toString)
      case _ => throw JSON.JSONError(s"array or text expected")
    }
  }
}

abstract class ConcreteType {
  def lub(that: ConcreteType): ConcreteType = {
    if (that == this) this
    else if (that == AnyType) AnyType
    else if (that == NullType) OptionType(this)
    else AnyType
  }
  def sub(that: ConcreteType) = (this lub that) == that
}

abstract class ConcreteBaseType extends ConcreteType
case object IntType extends ConcreteBaseType
case object FloatType extends ConcreteBaseType
case object BooleanType extends ConcreteBaseType
case object StringType extends ConcreteBaseType

case object AnyType extends ConcreteType {
  override def lub(that: ConcreteType) = AnyType
}
case object NullType extends ConcreteType {
  override def lub(that: ConcreteType) = that match {
    case NullType | OptionType(_) => that
    case AnyType => AnyType
    case _ => OptionType(that)
  }
}

case class ListType(entry: ConcreteType) extends ConcreteType {
  override def lub(that: ConcreteType) = that match {
    case ListType(thatEntry) => ListType(entry lub thatEntry)
    case _ => super.lub(that)
  }
}
case class OptionType(entry: ConcreteType) extends ConcreteType {
  override def lub(that: ConcreteType) = that match {
    case OptionType(thatEntry) => OptionType(entry lub thatEntry)
    case NullType => this
    case _ => super.lub(that)
  }
}
case class VectorType(entry: ConcreteType, length: Int) extends ConcreteType {
  override def lub(that: ConcreteType) = that match {
    case that: VectorType =>
      if (this.length == that.length) VectorType(this.entry lub that.entry, length)
      else AnyType
    case _ => super.lub(that)
  }
}
case class TupleType(entries: List[ConcreteType]) extends ConcreteType {
  def arity = entries.length
  override def lub(that: ConcreteType) = that match {
    case TupleType(thatEntries) if entries.length == thatEntries.length =>
      val lubs = (entries zip thatEntries) map {case (a,b) => a lub b}
      TupleType(lubs)
    case _ => super.lub(that)
  }
}
case class RecordType(fields: List[(String,ConcreteType)]) extends ConcreteType {
  def arity = fields.length
  def apply(s: String): Option[ConcreteType] = listmap(fields, s)
  def keys = fields.map(_._1)
  override def lub(that: ConcreteType) = that match {
    case that: RecordType =>
      val lubKeys = inter(this.keys, that.keys)
      val lubFields = lubKeys map {k => (k, this(k).get lub that(k).get)}
      RecordType(lubFields.toList)
    case _ => super.lub(that)
  }
}

object ConcreteType {
  def leastUpperBound(entries: ConcreteType*): ConcreteType = entries.foldLeft(AnyType) {case (a,b) => a lub b}
}

/** tuple and vector types are inhabited by arrays, but arrays are never infered as such */
object JSONTyping {
  def infer(j: JSON): ConcreteType = j match {
    case j: JSONValue => j.tp
    case JSONNull => NullType
    case j: JSONArray =>
      val tps = j.values map infer
      val lub = ConcreteType.leastUpperBound(tps:_*)
      ListType(lub)
    case j: JSONObject =>
      val fieldTypes = j.map map {case (k,v) => (k.value, infer(v))}
      RecordType(fieldTypes)
  }
  def check(j: JSON, tp: ConcreteType): Boolean = {
    tp match {
      case AnyType => true
      case NullType | _:ConcreteBaseType =>
        infer(j) == tp
      case OptionType(a) =>
        j == JSONNull || check(j,a)
      case ListType(a) =>
        j match {
          case JSONArray(vs@_*) => vs.forall {v => check(v,a)}
          case _ => false
        }
      case VectorType(a,l) =>
        j match {
          case JSONArray(vs@_*) => vs.length == l && vs.forall {v => check(v,a)}
          case _ => false
        }
      case RecordType(fieldTypes) =>
        j match {
          case j: JSONObject =>
            fieldTypes.forall {case (k,a) =>
               val jk = j(k).getOrElse(return false)
               check(jk, a)
            }
          case _ => false
        }
      case TupleType(as) =>
        j match {
          case JSONArray(vs@_*) =>
            if (vs.length != as.length) return false
            (vs zip as).forall {case (v,a) => check(v,a)}
          case _ => false
        }
    }
  }
}