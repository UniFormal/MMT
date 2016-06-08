package info.kwarc.mmt.api

import utils._

/**
  * This package defines various MMT-independent high-level APIs.
  */
package object utils {
   def stringToInt(s: String) = try {Some(s.toInt)} catch {case _: Exception => None}
  
   def stringToList(s: String, sep: String = "\\s") = s.split(sep).toList match {
      case List("") => Nil
      case l => l
   }
   def listToString[A](l: List[A], sep: String) = {}
   
   def repeatString(s: String, n: Int, sep: String = "") = Range(0,n).map(_ => s).mkString(sep)

   def insertSep[A](l: List[A], sep: A) = if (l.isEmpty) Nil else l.flatMap(a => List(a,sep)).init
   
   def readFullStream(is: java.io.InputStream) = scala.io.Source.fromInputStream(is, "UTF-8").mkString
}
