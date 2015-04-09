package info.kwarc.mmt.api

import utils._

case class NamespaceMap(base: Path, prefixes: List[(String,URI)] = Nil) {
   /** change the base */
   def apply(newBase: Path) = copy(base = newBase)
   /** change the base (relative to this NamespaceMap) */
   def base(s: String) = copy(base = Path.parse(s, this))
   /** resolve a prefix */
   def get(p: String) = prefixes.find(_._1 == p).map(_._2)
   /** define a new prefix (URI is relative to default) */
   def add(p: String, u: URI): NamespaceMap = copy(prefixes = (p,default.resolve(u))::prefixes)
   /** define a new prefix as a string (which is relative to this NamespaceMap) */
   def add(p: String, u: String): NamespaceMap = add(p, URI(expand(u)))
   /** default namespace (URI of base) */
   def default = base.doc.uri // = prefixes.getOrElse("", URI(""))
   /** expands a CURIE into a URI */
   def expand(s: String): String = {
      val u = URI(s)
      if (u.scheme.isDefined && u.authority.isEmpty) {
         val p = u.scheme.get
         get(p) match {
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

import scala.xml._

object NamespaceMap {
   def legalPrefix(s: String) = s.indexOf(":") == -1  //too generous
   def fromXML(n: Node) = {
      var nm: List[(String,URI)] = Nil
      xml.namespaces(n.scope).foreach {case (p,u) => nm ::= ((p,URI(u)))}
      NamespaceMap(mmt.mmtbase, nm)
   }
   def empty = apply(mmt.mmtbase)
}