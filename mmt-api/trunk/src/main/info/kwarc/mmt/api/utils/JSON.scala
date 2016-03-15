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
  override def toString = {
    val escaped = JSONFormat.quoteString(value) //also escapes / to \/ which is allowed but weird
    "\"" + escaped + "\""
  }
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
      if (u.empty)
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
     val (p,closed) = s.next('"', '\\')
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
}