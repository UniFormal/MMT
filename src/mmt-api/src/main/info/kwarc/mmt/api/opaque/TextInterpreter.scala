package info.kwarc.mmt.api.opaque

import info.kwarc.mmt.api._
import objects._
import parser._
import presentation._
import checking._
import symbols._
import utils.Unparsed

import scala.xml._

class TextInterpreter extends OpaqueElementInterpreter
                         with OpaqueTextParser with OpaqueChecker with OpaqueTextPresenter with OpaqueHTMLPresenter {
   type OE = OpaqueText
   override def logPrefix = "opaque_text"

   /** the format, override as needed */
   val format = OpaqueText.defaultFormat
   override val formatAlias = List("T")
   /** the escape characters used to embed MMT object, override as needed */
   val escapes = OpaqueText.defaultEscapes

   private def fromFragments(parent: DPath, frags: List[TextFragment]) = {
      val tf = if (frags.length == 1) frags.head else ScopeFragment(frags)
      new OpaqueText(parent, format, tf)
   }
   def fromNode(parent: DPath, nsMap: NamespaceMap, nodes: NodeSeq): OpaqueText = {
      val frags = OpaqueText.fromNode(nsMap, nodes)
      fromFragments(parent, frags)
   }

   def fromString(oP: ObjectParser, parent: DPath, pu: ParsingUnit)(implicit eh: ErrorHandler): OpaqueText = {
      val frags = OpaqueText.fromString(oP, pu, escapes)
      fromFragments(parent, frags)
   }

   def check(oC: ObjectChecker, context: Context, rules: RuleSet, oe : OpaqueElement)(implicit ce: CheckingEnvironment): Unit = {
      val ot = downcast(oe)
      // TODO turn into a single object, parse and check in one go
      log("opaque element: " + ot.text)
      val fragmentsInContext = ot.text.subobjects(OpaqueContext(Nil))
      log("object fragments: " + fragmentsInContext.toString)
      fragmentsInContext foreach {case (opaqueCont,of) =>
        // subobjects are ordered such that all fragments in local context have been checked already
        val localContext = opaqueCont.toContext
        log("local context is: " + localContext)
        of.delayedParse(localContext)
        of.tc match {
          case tc: TermContainer => tc.parsed.foreach {t =>
            val cu = CheckingUnit.byInference(Some(oe.path $ of.comp), context ++ localContext, t)
            oC(cu, rules)
          }
          case cc: ContextContainer => cc.parsed.foreach {c =>
            val cu = CheckingUnit(Some(oe.path $ of.comp), context ++ localContext, cc.unknowns, IsContext(Stack(cc.free), c))
            oC(cu, rules)
          }
        }
      }
   }

   def toString(oP: ObjectPresenter, oe: OpaqueElement)(implicit rh: RenderingHandler): Unit = {
      val ot = downcast(oe)
      ot.text.toString(oP)(rh, escapes)
   }

   def toHTML(oP: ObjectPresenter, oe: OpaqueElement)(implicit rh : RenderingHandler): Unit = {
      val ot = downcast(oe)
      ot.text.toHTML(oP)(rh, oe)
   }
}
