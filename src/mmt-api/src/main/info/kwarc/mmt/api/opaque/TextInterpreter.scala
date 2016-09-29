package info.kwarc.mmt.api.opaque

import info.kwarc.mmt.api._
import objects._
import parser._
import presentation._
import checking._
import symbols._
import utils.Unparsed

import scala.xml._

/** see [[OpaqueText]] */
abstract class TextFragment

// TODO remove the onCheck method, once structure is decoupled from object parsing
class TermFragment(val index: Int, val tc: TermContainer, val onCheck: () => Unit) extends TextFragment {
   def comp = OtherComponent("ext-"+index.toString)
   override def toString = "$" + tc.get.map(_.toString).getOrElse(tc.read.getOrElse("")) + "$"
}

case class StringFragment(value: String) extends TextFragment {
   override def toString = value
}

/** text fragments intermixed with [[Obj]]ects */
class OpaqueText(val parent: DPath, val fragments: List[TextFragment]) extends OpaqueElement {
   def format = "text"
   def raw: NodeSeq = fragments.map {
      case StringFragment(s) => scala.xml.Text(s)
      case tf: TermFragment => tf.tc.get match {
         case Some(t) => t.toOBJNode
         case None => <unparsed>{tf.tc.parsed}</unparsed>
      }
   }
   override def toString = fragments.map(_.toString).mkString
   
   override def getComponents = fragments flatMap {
      case tf: TermFragment =>
         val dc = DeclarationComponent(tf.comp, tf.tc)
         List(dc)
      case _:StringFragment => Nil
   }
   
   def compatibilityKey = fragments.collect {
      case f: StringFragment => f
   }
   
   override def compatible(that: StructuralElement) = that match {
      case that: OpaqueText =>
         this.compatibilityKey == that.compatibilityKey
      case _ => false
   }
}

class TextInterpreter extends OpaqueElementInterpreter
                         with OpaqueTextParser with OpaqueChecker with OpaqueTextPresenter with OpaqueHTMLPresenter {
   type OE = OpaqueText
   override def logPrefix = "opaque_text"
   
   def format = "text"
   override def isApplicable(f: String) = super.isApplicable(f) || f == "T"
   
   def fromNode(parent: DPath, nsMap: NamespaceMap, nodes: NodeSeq): OpaqueText = {
      var i = -1
      val frags = nodes.toList map {
         case scala.xml.Text(s) => StringFragment(s)
         case n @ <OMOBJ>{_*}</OMOBJ> =>
            val t = Obj.parseTerm(n, nsMap)
            val tc = TermContainer(t)
            i += 1
            new TermFragment(i, tc, () => ())
         case e => StringFragment(e.text)
      }
      new OpaqueText(parent, frags)
   }
   
   def fromString(oP: ObjectParser, parent: DPath, pu: ParsingUnit)(implicit eh: ErrorHandler): OpaqueText = {
      val errorFun = (msg: String) => throw ParseError(msg)
      def absPos(p: SourcePosition) = SourcePosition(pu.source.region.start.offset+p.offset, -1,-1)
      var fragments: List[TextFragment] = Nil
      val u = new Unparsed(pu.term, errorFun)
      var i = -1
      var term = false
      while (!u.empty) {
         val begin = u.getSourcePosition
         var fragS = u.takeUntilChar('$', '\\') // {s => (s(0).toString,s(0).toString)}
         log((if (term) "term: " else "text: ") + fragS)
         val end = u.getSourcePosition
         if (fragS.nonEmpty) {
            val frag = if (term) {
               val srcref = pu.source.copy(region = SourceRegion(absPos(begin), absPos(end)))
               val sU = ParsingUnit(srcref, pu.context, fragS, pu.nsMap)
               // we cannot parse at this point because opaque elements may have forward references
               val tc = TermContainer(fragS)
               val onCheck = () => {
                  log("parsing with context: " + pu.context.toString)
                  tc.parsed = oP(sU).toTerm
               }
               i += 1
               new TermFragment(i, tc, onCheck)
            } else
               StringFragment(fragS)
            fragments ::= frag
         }
         term = !term
      }
      new OpaqueText(parent, fragments.reverse)
   }
   
   def check(oC: ObjectChecker, context: Context, rules: RuleSet, oe : OpaqueElement)(implicit ce: CheckingEnvironment) {
      val ot = downcast(oe)
      ot.fragments foreach {
         case f: StringFragment =>
         case f: TermFragment =>
            log("checking " + oe.toString)
            f.onCheck()
            f.tc.parsed.foreach {t =>
               val cu = CheckingUnit.byInference(Some(oe.path $ f.comp), context, t)
               oC(cu, rules)
            }
      }
   }

   def toString(oP: ObjectPresenter, oe: OpaqueElement)(implicit rh: RenderingHandler) {
      val ot = downcast(oe)
      ot.fragments.foreach {
         case tf: TermFragment =>
            tf.tc.get match {
               case Some(t) => oP(t, None)
               case None => rh << "\"" + tf.tc.read + "\""
            }
         case StringFragment(s) =>
            rh(s)
      }
   }
   
   def toHTML(oP: ObjectPresenter, oe: OpaqueElement)(implicit rh : RenderingHandler) {
      val ot = downcast(oe)
      ot.fragments.foreach {
         case tf: TermFragment =>
            tf.tc.get match {
               case Some(t) => oP(t, None)
               case None => rh(<pre>{tf.tc.read}</pre>)
            }
         case StringFragment(s) =>
            val escaped = scala.xml.Utility.escape(s)
            val formatted = escaped.replace("\n\n", "<p/>").replace("\n","<br/>")
            rh(formatted)
      }
   }

}