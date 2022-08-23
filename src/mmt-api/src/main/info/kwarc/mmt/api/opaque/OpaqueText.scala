package info.kwarc.mmt.api.opaque

import info.kwarc.mmt.api._
import objects._
import utils._
import parser._
import presentation._
import checking._
import symbols._

import scala.xml.{Node,NodeSeq,Text,Elem}

/**
 * opaque contents using flat text structure intermixed with [[Obj]]ects
 *
 * string representation: {...} for variable scopes, $...$ for MMT objects
 * XML representation: <scope> for variable scopes, <OMOBJ> or <unparsed> for MMT objects
 */
class OpaqueText(val parent: DPath, val format: String, val text: TextFragment) extends OpaqueElement {
   def raw = text.raw
   override def toString = format + ": " + text.toString()

   /** components are the objects in left-to-right order, they're numbered starting from 0 */
   override def getComponents = text.getComponents

   override def getComponentContext(key: ComponentKey) = {
     MyList(text.subobjects(Nil)) mapFind {case (opCon, frag) =>
       if (frag.comp == key) Some(opCon.toContext) else None
     } getOrElse Context.empty
   }

   /** used to check if a changed opaque element can be merged into tihs one */
   def compatibilityKey = text.removeTerms

   override def compatible(that: StructuralElement) = that match {
      case that: OpaqueText =>
         this.compatibilityKey == that.compatibilityKey
      case _ => false
   }
}

object OpaqueText {
  case class Bracket(begin: Char, end: Char) {
    def wrapAround(s: String) = begin + s + end
  }
  case class Escapes(scope: Bracket, decl: Bracket, obj: Bracket, unparsed: Bracket) {
    def nonTextBegin = List(scope,decl,obj).map(_.begin)
  }

  val defaultEscapes = Escapes(Bracket('{', '}'), Bracket('[',']'), Bracket('$', '$'), Bracket('"', '"'))
  val defaultFormat = "text"

  def fromNode(nsMap: NamespaceMap, nodes: NodeSeq): List[TextFragment] = {
     var i = -1
     def doOneNode(n: Node): TextFragment = n match {
         case Text(s) => StringFragment(s)
         case <scope>{child @_*}</scope> =>
           val frags = child map doOneNode
           ScopeFragment(frags.toList)
         case n @ <OMOBJ>{_*}</OMOBJ> =>
            val t = Obj.parse(n, nsMap)
            val (decl, oc) = t match {
              case t: Term => (false, TermContainer(t))
              case c: Context => (true, ContextContainer(c))
            }
            i += 1
            new ObjFragment(i, decl, oc, _ => ())
         case e => StringFragment(e.text)
     }
     nodes.toList map doOneNode
  }

  def fromString(oP: ObjectParser, pu: ParsingUnit, escapes: Escapes)(implicit eh: ErrorHandler): List[TextFragment] = {
      val errorFun = (msg: String) => throw ParseError(msg)
      def absPos(p: SourcePosition) = SourcePosition(pu.source.region.start.offset+p.offset, -1,-1)
      val u = new Unparsed(pu.term, errorFun)
      var i = -1
      // parsing functions
      def parseList: List[TextFragment] = {
        var frags: List[TextFragment] = Nil
        while (!u.empty && u.head != escapes.scope.end) {
          val c = u.head
          val f = if (escapes.nonTextBegin.contains(c)) {
            u.next
            if (c == escapes.obj.begin) {
              parseObj(false)
            } else if (c == escapes.decl.begin) {
              parseObj(true)
            } else if (c == escapes.scope.begin) {
              parseScope
            } else {
              null // impossible
            }
          } else if (escapes.unparsed.begin == c) {
            u.next
            var seen = ""
            while (!u.empty && u.head != escapes.unparsed.end) {
              seen += u.next
            }
            if (!u.empty) u.next
            StringFragment(seen)
          } else {
            parseText
          }
          frags ::= f
        }
        frags.reverse
      }
      def parseScope: ScopeFragment = {
        val frags = parseList
        if (u.empty) {
          errorFun("unclosed scope")
        } else {
          u.drop(escapes.scope.end.toString)
        }
        ScopeFragment(frags)
      }
      def parseObj(decl: Boolean): ObjFragment = {
        val begin = u.getSourcePosition
        val bracket = if (decl) escapes.decl else escapes.obj
        val (s,endFound) = u.takeUntilChar(bracket.end, '\\')
        if (!endFound) errorFun("unclosed object")
        val end = u.getSourcePosition
        val srcref = pu.source.copy(region = SourceRegion(absPos(begin), absPos(end)))

        // we cannot parse at this point because opaque elements may have forward references
        val tc = if (decl) ContextContainer(s) else TermContainer(s)
        i += 1
        def delayedParse(localCon: Context): Unit = {
          val contextNot = if (decl) Some(Context.parsingRule) else None
          val pU = ParsingUnit(srcref, pu.context ++ localCon, s, pu.iiContext, contextNot)
          val pr = oP(pU)
          tc match {
            case tc: TermContainer =>
               tc.parsed = pr.toTerm
            case cc: ContextContainer =>
               val cont: Context = pr.term match {
                  case Context.AsTerm(cont) =>
                    cont
                  case _ =>
                    eh(SourceError("opaque-text", srcref, "not a context"))
                    Context.empty
                }
                cc.parsed = cont
                cc.unknowns = pr.unknown
                cc.free = pr.free
          }
        }
        new ObjFragment(i, decl, tc, delayedParse _)
      }
      def parseText: StringFragment = {
        var seen = ""
        while (true) {
          if (!u.empty && !(escapes.scope.end::escapes.unparsed.begin::escapes.nonTextBegin).contains(u.head)) {
            seen += u.next
          } else {
            return StringFragment(seen)
          }
        }
        StringFragment("") // impossible
      }

      val fs = parseList
      if (!u.empty) {
        errorFun("unexpected input: " + u.remainder)
      }
      fs
  }
}

import OpaqueText._

/** a list object fragments representing declarations, innermost first */
case class OpaqueContext(decls: List[ObjFragment]) extends ListWrapper[ObjFragment,OpaqueContext](decls) {
  def companion = OpaqueContext
  /** concatenates the Context's */
  def toContext: Context = {
    decls.flatMap {frag =>
       if (frag.isDecl) {// actually a redundant check
         frag.tc match {
            case cc: ContextContainer =>
              cc.get getOrElse Context.empty
            case _ =>
              throw ImplementationError("not a context") // should be impossible because we check for isDecl
          }
        } else
          Context.empty
    }
  }
}
object OpaqueContext extends ListWrapperCompanion[ObjFragment,OpaqueContext]

/** see [[OpaqueText]] */
abstract class TextFragment {
  // these methods are documented in OpaqueText
  def raw: Node
  def toString(oP: ObjectPresenter)(implicit rh: RenderingHandler, escapes: OpaqueText.Escapes): Unit
  def toHTML(oP: ObjectPresenter)(implicit rh: RenderingHandler, oe: OpaqueElement): Unit
  def getComponents: List[DeclarationComponent]

  /** all subobjects of this fragment paired with the respective context */
  def subobjects(context: OpaqueContext): List[(OpaqueContext,ObjFragment)]
  /** this fragment with all ObjFragments replaced with empty scopes */
  def removeTerms: TextFragment
}

/**
 * introduces a variable scope
 *
 * Scoping rules: An ObjFragment f has access to all declaring ObjFragment's in
 * - proper ancestor scopes
 * - the current scope if they precede f in the following order: declaring left-to-right, then non-declaring left-to-right
 */
case class ScopeFragment(body: List[TextFragment]) extends TextFragment {
  override def toString = defaultEscapes.scope wrapAround body.mkString
  def raw = <scope>{body.map(_.raw)}</scope>
  def getComponents = body.flatMap(_.getComponents)
  def subobjects(context: OpaqueContext) = {
     // first all the declarations in this scope
     val decls = body collect {
       case f: ObjFragment if f.isDecl => f
     }
     val declSOs = decls.zipWithIndex.map {case (d,i) =>
       (context + decls.take(i), d)
     }
     // then a flatMap over the rest
     val fullContext = context + decls
     val otherSOs = body flatMap {
       case f: ObjFragment if f.isDecl => Nil
       case f => f.subobjects(fullContext)
     }
     declSOs:::otherSOs
  }
  def removeTerms = copy(body = body.map(_.removeTerms))
  def toString(oP: ObjectPresenter)(implicit rh: RenderingHandler, escapes: OpaqueText.Escapes): Unit = {
     rh << escapes.scope.begin.toString
     body.foreach {b => b.toString(oP)}
     rh << escapes.scope.end.toString
  }
  def toHTML(oP: ObjectPresenter)(implicit rh: RenderingHandler, oe: OpaqueElement): Unit = {
    body.foreach {b => b.toHTML(oP)}
  }
}

/**
 * a fragment containing a formal object
 * @param index the number of this fragment in the containing [[OpaqueElement]]
 * @param isDecl true if this is a context, i.e., declares variable
 * @param tc the container holding the object
 * @param pu the parsing unit if this was created from sources
 */
class ObjFragment(val index: Int, val isDecl: Boolean, val tc: ObjContainer[_<:Obj], val delayedParse: Context => Unit) extends TextFragment {
   def comp = OtherComponent("object_"+index.toString)
   override def toString = {
     val bracket = if (isDecl) defaultEscapes.decl else defaultEscapes.obj
     bracket.wrapAround(tc.get.map(_.toString).getOrElse(tc.read.getOrElse("")))
   }
   def raw = tc.get match {
       case Some(t) => t.toOBJNode
       case None => <unparsed>{tc.read.getOrElse("")}</unparsed>
   }
   def getComponents = {
     val dc = DeclarationComponent(comp, tc)
     List(dc)
   }
   def subobjects(context: OpaqueContext) = List((context,this))
   def removeTerms = ScopeFragment(Nil)
   def toString(oP: ObjectPresenter)(implicit rh: RenderingHandler, escapes: OpaqueText.Escapes): Unit = {
      tc.get match {
        case None =>
          rh << escapes.unparsed.begin.toString
          rh(tc.read.getOrElse(""))
          rh << escapes.unparsed.end.toString
        case Some(t) =>
          rh << escapes.obj.begin.toString
          oP(t, None)
          rh << escapes.obj.end.toString
      }
  }
  def toHTML(oP: ObjectPresenter)(implicit rh: RenderingHandler, oe: OpaqueElement): Unit = {
    tc.get match {
      case None =>
         rh << "<pre>"
         rh(tc.read.getOrElse(""))
         rh << "</pre>"
       case Some(t) =>
         oP(t, Some(CPath(oe.path, comp)))
    }
  }
}

/** a fragment holding any string */
case class StringFragment(value: String) extends TextFragment {
   override def toString = value
   def raw = scala.xml.Text(value)
   def getComponents = Nil
   def subobjects(context: OpaqueContext) = Nil
   def removeTerms = this
   def toString(oP: ObjectPresenter)(implicit rh: RenderingHandler, escapes: OpaqueText.Escapes): Unit = {
     rh << value
   }
   def toHTML(oP: ObjectPresenter)(implicit rh: RenderingHandler, oe: OpaqueElement): Unit = {
      val escaped = scala.xml.Utility.escape(value)
      val formatted = escaped.replace("\n\n", "<p/>").replace("\n","<br/>")
      rh << formatted
   }
}
