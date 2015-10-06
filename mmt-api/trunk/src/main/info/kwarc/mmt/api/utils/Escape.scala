package info.kwarc.mmt.api.utils

object Escape {
   /**
    * encode all occurrences of certain characters in a string
    */
   def apply(s: String, escapes: (Char,String)*) : String = {
      var in = s
      var out = ""
      // invariant: s = out + in
      while (in != "") {
         val c = in(0)
         val e = escapes.find(_._1 == c).map(_._2).getOrElse(c.toString)
         in = in.substring(1)
         out = out + e
      }
      out
   }
}