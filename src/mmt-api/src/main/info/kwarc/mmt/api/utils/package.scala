package info.kwarc.mmt.api

import utils._

/**
  * This package defines various MMT-independent high-level APIs.
  */
package object utils {
   /** converts a string to an integer, returns None on format exception */
   def stringToInt(s: String) = try {Some(s.toInt)} catch {case _: Exception => None}
  
   /** splits a string at a separator (returns Nil for the empty string) */
   def stringToList(s: String, sep: String = "\\s") = s.split(sep).toList match {
      case List("") => Nil
      case l => l
   }
   
   /** splits a string at whitespace, quoted segments may contain whitespace, \" for quote */
   def splitAtWhitespace(s: String): List[String] = {
      var segments : List[String] = Nil
      var current = ""
      var inquoted = false
      var todo = s
      def done {
        if (current.nonEmpty) {
          segments ::= current
          current = ""
        }
      }
      while (todo.nonEmpty) {
        if (todo.startsWith("\\\"")) {
          current += '"'
          todo = todo.substring(2)
        } else {
          val c = todo(0)
          todo  = todo.substring(1)
          if (inquoted) {
            if (c == '"') {
              done
              inquoted = false
            } else {
              current += c
            }
          } else {
            if (c == '"') {
              inquoted = true
            } else if (c.isWhitespace) {
              done
            } else {
              current += c
            }
          }
        }
      }
      done
      segments.reverse
   }
   
   /** turns a list into a string  by inserting a separator */
   def listToString[A](l: List[A], sep: String) = l.map(_.toString).mkString(sep)
   
   /** repeats a strings a number of times, optionally with a separtor */
   def repeatString(s: String, n: Int, sep: String = "") = Range(0,n).map(_ => s).mkString(sep)

   /** inserts a separator element in between all elements of a list */
   def insertSep[A](l: List[A], sep: A) = if (l.isEmpty) Nil else l.flatMap(a => List(a,sep)).init

   /** slurps an entire stream into a string */
   def readFullStream(is: java.io.InputStream) = scala.io.Source.fromInputStream(is, "UTF-8").mkString
}
