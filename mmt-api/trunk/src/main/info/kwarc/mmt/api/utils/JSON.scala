package info.kwarc.mmt.api.utils

import scala.util.parsing.json.JSONFormat

/**
 * straightforward API for JSON objects
 */
sealed abstract class JSON

case object JSONNull extends JSON {
  override def toString = "null"
}

sealed abstract class JSONValue extends JSON {
   def value: Any
   override def toString = value.toString
}
sealed abstract class JSONNumber extends JSONValue 

case class JSONInt(value: Int) extends JSONNumber

case class JSONFloat(value: Double) extends JSONNumber

case class JSONBoolean(value: Boolean) extends JSONValue

case class JSONString(value: String) extends JSONValue {
  override def toString = "\"" + JSONFormat.quoteString(value) + "\""
}

case class JSONArray(values: JSON*) extends JSON {
  override def toString = values.mkString("[", ", ", "]")
}

object JSONArray {
   implicit def toList(a: JSONArray) = a.values.toList
   implicit def fromList(l: List[JSON]) = JSONArray(l :_*)
}

case class JSONObject(map: List[(JSONString, JSON)]) extends JSON {
  override def toString =
    map.map { case (k, v) => k.toString + " : " + v.toString}.mkString("{", ",\n", "}")
  def apply(s: String): Option[JSON] = map.find(_._1 == JSONString(s)).map(_._2)
}

object JSONObject {
   implicit def toList(o: JSONObject) = o.map.toList
   implicit def fromList(l: List[(JSONString,JSON)]) = JSONObject(l)
   def apply(cases: (String,JSON)*): JSONObject = JSONObject(cases.toList map {case (k,v) => (JSONString(k),v)})
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
   
   def parse(s: String) : JSON = {
      val u = new Unparsed(s, msg => throw JSONError(msg))
      val j = parse(u)
      u.trim
      if (u.remainder.isEmpty)
         j
      else throw JSONError("additional characters after successful parse")
   }
   
   def parse(s: Unparsed) : JSON = {
      val c = s.trim.head
      c match {
         case 'n' => parseNull(s)
         case 't'|'f' => parseBoolean(s)
         case c if c == '-' || c.isDigit => parseNum(s)
         case '"' => parseString(s)
         case '{' => parseObject(s)
         case '[' => parseArray(s)
         case _ => throw JSONError("Illegal starting character for JSON")
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
   
   def parseNum(s: Unparsed): JSONNumber = {
      val e = "(?: e|e\\+|e-|E|E\\+|E-)"
      val mt = s.next(s"(-)?(\\d+)(\\.\\d+)?($e\\d+)?")
      val List(sgn, main, frac, exp) = mt.subgroups
      val jn = if (frac == null && exp == null) {
         val f = if (sgn == null) 1 else -1
         JSONInt(f * main.toInt)
      } else
         JSONFloat(mt.matched.toDouble)
      jn
   }
   
   def parseString(s: Unparsed) = {
      s.drop("\"")
      val (p,closed) = s.next('"', '\\') {rest =>
         rest(0) match {
            case c if c == '"' || c == '\\' => (c.toString, c.toString)
            case 'b' => ("b", "\b")
            case 'f' => ("f", "\f")
            case 'n' => ("n", "\n")
            case 'r' => ("r", "\r")
            case 't' => ("t", "\t")
            case 'u' => (rest.substring(0,5), "u"+rest.substring(1,4)) //TODO make char
            case _ => throw JSONError("Illegal starting character " + rest(0) + " for JSON")
         }
      }
      if (!closed)
         throw JSONError("unclosed string")
      JSONString(p)
   }

   def parseObject(s: Unparsed): JSONObject = {
      s.trim
      s.drop("{")
      parseOpenObject(s)
   }
   
   def parseOpenObject(s: Unparsed): JSONObject = {
      s.trim
      if (s.head == '}') {
         s.drop("}")
         JSONObject()
      } else {
         s.trim
         val key = parseString(s)
         s.trim
         s.drop(":")
         val value = parse(s)
         s.trim
         val first = (key,value)
         val c = s.next
         if (c == ',') {
            val rest = parseOpenObject(s)
            first :: rest
         } else if (c == '}') {
            JSONObject(List(first))
         } else throw JSONError("expected ',' or '}', found " + c + " " + c.isWhitespace)
      }
   }
   
   def parseArray(s: Unparsed): JSONArray = {
      s.trim
      s.drop("[")
      parseOpenArray(s)
   }
   def parseOpenArray(s: Unparsed): JSONArray = {
      s.trim
      if (s.head == ']') {
         s.drop("]")
         JSONArray()
      } else {
         val first = parse(s)
         s.trim
         val c = s.next
         if (c == ',') {
            val rest = parseOpenArray(s)
            first :: rest
         } else if (c == ']') {
            JSONArray(first)
         } else throw JSONError("expected ',' or ']', found " + c)
      }
   }
}