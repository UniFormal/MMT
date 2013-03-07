package info.kwarc.mmt.api.utils

class NamespaceMap {
   var default : URI = URI("")
   val prefixes = new scala.collection.mutable.ListMap[String,URI]
   /** expands a CURIE into a URI */
   def expand(s: String): String = {
      val u = URI(s)
      if (u.scheme.isDefined && u.authority.isEmpty) {
         val p = u.scheme.get
         prefixes.get(p) match {
            case Some(r) => r.toString + s.substring(p.length+1)
            case None => s
         }
      } else s
   }
   /** compactifies an URI into a CURIE */
   def compact(s: String): String = {
      val long = URI(s)
      prefixes.foreach {
         case (p,base) => if (base <= long) return p + ":" + long.toString.substring(base.toString.length)
      }
      return s
   }
}

object NamespaceMap {
   def legalPrefix(s: String) = s.indexOf(":") == -1  //too generous
}