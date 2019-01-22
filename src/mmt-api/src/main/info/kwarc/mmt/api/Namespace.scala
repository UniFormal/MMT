package info.kwarc.mmt.api

import documents._
import utils._

case class NamespaceMap(base: Path, meta: Option[MPath] = None, prefixes: List[(String,URI)] = Nil) {
   /** change the base */
   def apply(newBase: Path) = copy(base = newBase)
   /** change the base (relative to this NamespaceMap) */
   def base(s: String): NamespaceMap = copy(base = Path.parse(s, this))
   /** set the fixed meta-theory */
   def meta(mt: MPath) = copy(meta = Some(mt))
   /** resolve a prefix */
   def get(p: String) = prefixes.find(_._1 == p).map(_._2)
   /** define a new prefix (URI is relative to default) */
   def add(p: String, u: URI): NamespaceMap = copy(prefixes = (p,default.resolve(u))::prefixes)
   /** takes note of an instruction, returns the new map */
   def process(ii: InterpretationInstruction): NamespaceMap = {
     ii match {
       case Namespace(_, ns) => apply(ns)
       case NamespaceImport(_, pr,ns) => add(pr, ns.uri)
       case FixedMeta(_, mt) => meta(mt)
     }
   }
   /** union of two namespace map, entries in that override the ones in this */
   def ++(that: NamespaceMap) = NamespaceMap(that.base, meta, that.prefixes ::: prefixes)
   /** define a new prefix as a string (which is relative to this NamespaceMap) */
   def add(p: String, u: String): NamespaceMap = add(p, URI(expand(u)))
   /** default namespace (URI of base) */
   def default = base.doc.uri
   /** expands a CURIE in a string into a URI */
   def expand(s: String): String = expand(URI(s)).toString
   /** expands a CURIE into a URI */
   def expand(u: URI): URI =
      if (u.scheme.isDefined && u.authority.isEmpty) {
         val uS = u.copy(scheme = None)
         val p = u.scheme.get
         get(p) match {
            case Some(r) => URI(r.toString + uS.toString)
            case None => u
         }
      } else u
   /** compactifies a URI into a CURIE */
   def compact(s: String): String = {
      val long = URI(s)
      prefixes.foreach {
         case (p,base) => if (base <= long) return p + ":" + long.toString.substring(base.toString.length)
      }
      return s
   }
   /** parses, expands CURIE and resolves against base */
   def resolve(u : String) = base.doc.uri resolve expand(utils.URI(u))
}

import scala.xml._

object NamespaceMap {
   def legalPrefix(s: String) = s.indexOf(":") == -1  //too generous
   def fromXML(n: Node) = {
      var nm: List[(String,URI)] = Nil
      xml.namespaces(n.scope).foreach {case (p,u) => nm ::= ((p,URI(u)))}
      NamespaceMap(mmt.mmtbase, None, nm)
   }
   def empty = NamespaceMap(mmt.mmtbase)
}
