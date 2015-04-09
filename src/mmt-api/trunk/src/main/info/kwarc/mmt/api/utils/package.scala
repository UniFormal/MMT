package info.kwarc.mmt.api

import utils._

/**
  * This package defines various MMT-independent high-level APIs. 
  */
package object utils {
   def stringToList(s: String, sep: String = "\\s") = s.split(sep).toList match {
      case List("") => Nil
      case l => l
   }
}