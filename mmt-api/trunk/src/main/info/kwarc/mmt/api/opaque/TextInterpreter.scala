package info.kwarc.mmt.api.opaque

import info.kwarc.mmt.api._
import objects._
import parser._
import presentation._
import utils.Unparsed

import scala.xml._

/** see [[OpaqueText]] */
abstract class TextFragment
case class TermFragment(value: Term) extends TextFragment {
   override def toString = "$" + value.toString + "$"
}
case class StringFragment(value: String) extends TextFragment {
   override def toString = value
}

/** text fragments intermixed with [[Obj]]ects */
class OpaqueText(val parent: DPath, val fragments: List[TextFragment]) extends OpaqueElement {
   def format = "text"
   def raw: NodeSeq = fragments.map {
      case StringFragment(s) => scala.xml.Text(s)
      case TermFragment(t) => t.toNode
   }
   override def toString = fragments.map(_.toString).mkString
}

class TextInterpreter extends OpaqueElementInterpreter[OpaqueText]
                      with OpaqueTextParser[OpaqueText]
                      with OpaqueHTMLPresenter[OpaqueText] {
   def format = "text"
   override def isApplicable(f: String) = super.isApplicable(f) || f == "T"
   
   def fromNode(parent: DPath, nsMap: NamespaceMap, nodes: NodeSeq): OpaqueText = {
      val frags = nodes.toList map {
         case scala.xml.Text(s) => StringFragment(s)
         case n @ <OMOBJ>{_*}</OMOBJ> =>
            val t = Obj.parseTerm(n, nsMap)
            TermFragment(t)
      }
      new OpaqueText(parent, frags)
   }
   
   def fromString(oP: ObjectParser, parent: DPath, pu: ParsingUnit)(implicit eh: ErrorHandler): OpaqueText = {
      val errorFun = (msg: String) => throw ParseError(msg)
      var fragments: List[TextFragment] = Nil
      val u = new Unparsed(pu.term, errorFun)
      var term = false
      while (u.remainder.nonEmpty) {
         val begin = u.getSourcePosition
         val s = u.next('$', '\\') {s => (s(0).toString,s(0).toString)}
         val end = u.getSourcePosition
         if (s.nonEmpty) {
            val frag = if (term) {
               val srcref = pu.source.copy(region = SourceRegion(begin, end))
               val sU = ParsingUnit(srcref, pu.context, s, pu.nsMap)
               val t = oP(sU)
               TermFragment(t)
            } else
               StringFragment(s)
            fragments ::= frag
         }
         term = !term
      }
      if (term)
         errorFun("unclosed term")
      new OpaqueText(parent, fragments.reverse)
   }
   
   def toHTML(oP: ObjectPresenter, oe: OpaqueText)(implicit rh : RenderingHandler) {
      oe.fragments.foreach {
         case TermFragment(t) =>
            oP(t, None)
         case StringFragment(s) =>
            rh(s)
      }
   }

}