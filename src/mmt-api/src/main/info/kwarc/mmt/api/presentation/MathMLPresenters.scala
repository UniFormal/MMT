package info.kwarc.mmt.api.presentation

import info.kwarc.mmt.api._
import notations._
import frontend._
import symbols._
import objects._
import utils.xml._
import HTMLAttributes._

import scala.xml.{Elem, Node}

@deprecated("use PresentationMathMLPresenter instead", "")
class MathMLPresenter extends PresentationMathMLPresenter{}

class ParallelMathMLPresenter(cmlPresenter: ContentMathMLPresenter) extends AnnotatedMathMLPresenter {
  override val annotators: List[(String, ObjectPresenter)] = List(
    "MathML-Content" -> cmlPresenter
  )

  /** a convenience wrapper for applyAll that returns a pair of (content, presentation) nodes */
  def applyParallel(o: Obj, source: Option[CPath]): (Node, Node) = {
    val res = applyAll(o, source).map(scala.xml.XML.loadString)
    (res.head, res(1))
  }
}

/**
  * Generates MathML with object-level annotations
  */
abstract class AnnotatedMathMLPresenter extends PresentationMathMLPresenter {
  val annotators: List[(String, ObjectPresenter)]

  override protected def dataattibs(implicit pc: PresentationContext) = {
    // insert appropriate ids and xrefs
    var ret = super.dataattibs
    val prefix = pc.hashedOwner.getOrElse("")
    ret ::= "id" -> s"p_${prefix}_$position"
    ret ::= "xref" -> s"c_${prefix}_$position"
    ret
  }

  private var cachedObj: Option[Obj] = None
  private var cached: Map[String, String] = Map()

  protected def setAnnotatedCache(o: Obj, c: Map[String, String]) {
    cachedObj = Some(o)
    cached = c
  }
  protected def clearAnnotatedCache() {
    cachedObj = None
    cached = Map()
  }

  override protected def doMathMLBody(o: Obj)(body: => Unit)(implicit pc: PresentationContext)  {
    pc.out(openTag("semantics", List()))

    body

    // generate annotations
    // <annotation-xml encoding="...">...</annotation-xml>
    annotators.foreach(sp => {
      pc.out(openTag("annotation-xml", List("encoding" -> sp._1)))

      // if we have this value cached, return it
      if((o.asInstanceOf[AnyRef] eq cachedObj.get.asInstanceOf[AnyRef]) && cached.contains(sp._1)) {
        pc.out(cached(sp._1))
      } else {
        sp._2(o, pc.owner)(pc.rh)
      }
      pc.out(closeTag("annotation-xml"))
    })

    pc.out(closeTag("semantics"))
  }

  /**
    * Applies all annotators and the combined object at the same time.
    * - the first annotators.length elements will correspond to the appropriate annotators
    * - the last element corresponds to the fully annotated object
    */
  def applyAll(obj: Obj, origin: Option[CPath]): List[String] = {
    val annots = annotators.map(na => (na._1, na._2.asString(obj, origin)))
    setAnnotatedCache(obj, annots.toMap)
    val res = asString(obj, origin)
    clearAnnotatedCache()
    annots.map(_._2) ::: List(res)
  }
}

/**
  * A presenter for ContentMathML
  */
class ContentMathMLPresenter extends ObjectPresenter {
  def apply(o: Obj, origin: Option[CPath])(implicit rh : RenderingHandler) {
    rh(apply(o, Context(), origin))
  }
  def apply(o: Obj, context: Context, origin: Option[CPath]): Node = {
    recurse(o)(CMLPresentationContext(context, Position(List()), origin.map(_.toPath).map(utils.sha256), false))
  }

  private def recurseNext(po: (Obj, Int))(implicit context: CMLPresentationContext): Elem = {
    recurse(po._1)(context.next(po._2))
  }

  private def recurse(o: Obj)(implicit context: CMLPresentationContext): Elem = {
   var elem = o match {
      case v: VarDecl => recurseVarDecl(v)
      case c: Context => recurseContext(c)
      case s: Sub => recurseSub(s)
      case s: Substitution => recurseContext(s.asContext)
      case id: OMID => recurseOMID(id)
      case bind: OMBINDC => recurseBind(bind)
      case a: OMA => recurseOMA(a)
      case v: OMV => recurseOMV(v)
      case a: OMATTR => recurseOMATTR(a)
      case l: OMLITTrait => recurseOMLITTrait(l)
      case f: OMFOREIGN => recurseOMForeign(f)
      case s: OMSemiFormal => recurseOMSemiFormal(s)
      case l: OML => recurseOML(l)
    }
    if(!context.skipAttrs) {
      val prefix = context.hashedOwner.getOrElse("")
      elem = addAttr(elem, "id",  s"c_${prefix}_${context.position}")
      elem = addAttr(elem, "xref", s"p_${prefix}_${context.position}")
    }
    elem
  }

  private def recurseVarDecl(v: VarDecl)(implicit context: CMLPresentationContext) = {
    <bvar>
      <ci>
        {v.name.toPath}
      </ci>{(v.tp.toList ::: v.df.toList).zipWithIndex.map(recurseNext)}
    </bvar>
  }
  private def recurseContext(c: Context)(implicit context: CMLPresentationContext) = {
    <apply>
      {c.zipWithIndex.map(recurseNext)}
    </apply>
  }
  private def recurseSub(s: Sub)(implicit context: CMLPresentationContext) = {
    <mi name={s.name.toPath}>
      {recurseNext((s.target, 0))}
    </mi>
  }
  private def recurseOMID(id: OMID) = <csymbol>{id.path.toPath}</csymbol>
  private def recurseBind(b: OMBINDC)(implicit context: CMLPresentationContext) = {
    val binder = recurseNext(b.binder, 0)
    val decls = b.context.zipWithIndex.map(vi => recurseNext((vi._1, vi._2 + 1)))
    val scopes = b.scopes.zipWithIndex.map(si => recurseNext((si._1, si._2 + 1 + b.context.length)))
    <apply>{binder}{decls}{scopes}</apply>
  }
  private def recurseOMA(a: OMA)(implicit context: CMLPresentationContext) = {
    val fun = recurseNext(a.fun, 0)
    val args = a.args.zipWithIndex.map(ai => recurseNext((ai._1, ai._2 + 1 )))
    <apply>{fun}{args}</apply>
  }
  private def recurseOMV(v: OMV)(implicit context: CMLPresentationContext) = {
    if (context.context.isDeclared(v.name)) <mws:qvar xmlns:mws="http://www.mathweb.org/mws/ns">{v.name.toPath}</mws:qvar>
    else <ci>{v.name.toPath}</ci>
  }
  private def recurseOMATTR(a: OMATTR)(implicit context: CMLPresentationContext) = {
    <apply><csymbol>OMATTR</csymbol>{List(a.arg, a.key, a.value).zipWithIndex.map(recurseNext)}</apply>
  }
  private def recurseOMLITTrait(l: OMLITTrait) = {
    addAttrOrChild(<cn encoding="mmt-literal">{l.toString}</cn>, "definitionURL", l.synTypeXML)
  }
  private def recurseOMForeign(f: OMFOREIGN) = <apply><csymbol>OMFOREIGN</csymbol>{Node}</apply>
  private def recurseOMSemiFormal(s: OMSemiFormal)(implicit context: CMLPresentationContext): Elem = {
    var j = 0
    <apply><csymbol>OMSemiFormal</csymbol>{s.tokens.map({
      case Text(f, o) => <mtext format={f}>{scala.xml.PCData(o)}</mtext>
      case XMLNode(n) => n
      case Formal(x) =>
        val r = recurseNext((x, j))
        j += 1
        r
    })}</apply>
  }
  private def recurseOML(l: OML)(implicit context: CMLPresentationContext) = {
    <label>{recurse(l.vd)(context.nextVirtual)}</label>
  }
}

case class CMLPresentationContext(context: Context, position: Position, hashedOwner: Option[String], skipAttrs: Boolean) {
  def nextVirtual = CMLPresentationContext(context, position, hashedOwner, true)
  def next(pos: Int) = CMLPresentationContext(context, position / pos, hashedOwner, false)
}

/**
 * This overrides the appropriate methods present objects as MathML.
 *
 * The MathML carries additional attributes as described in [[HTMLAttributes]]
 */
class PresentationMathMLPresenter extends NotationBasedPresenter {

   /** generalized apply method that takes a callback function to determine the css class of a subterm */
   def apply(o: Obj, origin: Option[CPath], style: PresentationContext => String)(implicit rh : RenderingHandler) {
      val pc = preparePresentation(o, origin)
      implicit val pcS = pc.copy(style = Some(style))
      doToplevel(o) {
         recurse(o)
      }
   }

   protected def dataattibs(implicit pc: PresentationContext) = {
      var ret = List(position -> pc.pos.toString)
      pc.source.foreach {r =>
         ret ::= source -> r.toString
      }
      pc.style.foreach {s =>
         ret ::= "class" -> s(pc)
      }
      ret
   }
   override def doIdentifier(p: ContentPath)(implicit pc : PresentationContext) {
      val primaryName = controller.getO(p) match {
         case Some(d: Declaration) =>
           val (pn,_) = d.primaryNameAndAliases
           pn
         case _ => p.name
      }
      pc.html.mo(attributes = (symref -> p.toPath) :: dataattibs) {
        val parts = primaryName.simplify.map {
          case ComplexStep(t) => t.name.toString
          case SimpleStep(s) => s
        }
        pc.html.text(parts.mkString("/"))
      }
   }
   override def doVariable(n: LocalName)(implicit pc : PresentationContext) {
      val vdAtt = pc.context.find(_.decl.name == n) match {
         case None => Nil
         case Some(vd) => List(varref -> vd.declpos.toString)
      }
      pc.html.mi(attributes = vdAtt ::: dataattibs) {
        super.doVariable(n)
      }
   }
   override def doLiteral(l: OMLITTrait)(implicit pc: PresentationContext) {
      val symAtt = l.synType match {
        case OMID(p) => List(symref -> p.toPath)
        case _ => Nil
      }
      pc.html.mn(attributes = symAtt ::: dataattibs) {
        super.doLiteral(l)
      }
   }
   override def doOperator(s: String)(implicit pc : PresentationContext) {
      pc.html.mo {
        super.doOperator(s)
      }
   }
   override def doDelimiter(p: GlobalName, d: Delimiter, implicits: List[Cont])(implicit pc : PresentationContext) {
      val mo = d.text match {
         case " " => element("mspace", ("width" -> ".2em") :: (symref -> p.toPath) :: dataattibs, "")
         case t   => element("mo", (symref -> p.toPath) :: dataattibs, t)
      }
      if (! implicits.isEmpty) {
         pc.html.mrow {
            pc.out(mo)
            implicits.foreach {i =>
               doImplicit {
                 doSpace(1)
                 i()
               }
            }
         }
      } else
         pc.out(mo)
   }
   override def doSpace(level: Int)(implicit pc : PresentationContext) {
      val ms = element("mspace", List(("width", "." + (2*level).toString + "em")), "")
      pc.out(ms)
   }
   override def doToplevel(o: Obj)(body: => Unit)(implicit pc: PresentationContext) {
      val nsAtts = List("xmlns" -> namespace("mathml"))
      val mmtAtts = pc.owner match {
         case None => Nil
         case Some(cp) => List(owner -> cp.parent.toPath, component -> cp.component.toString, position -> "")
      }
      // <mstyle displaystyle="true">
      pc.out(openTag("math", nsAtts ::: mmtAtts))
      doMathMLBody(o)(body)
      pc.out(closeTag("math"))
   }
   protected def doMathMLBody(o: Obj)(body: => Unit)(implicit pc: PresentationContext)  {
     body
   }
   private def bracket(optional: Boolean, open: Boolean) = {
      val cls = if (optional) List(bracketsOpt, bracketsOptHidden) else List(brackets)
      val brack = if (open) "(" else ")"
      element("mo", List(("class", (operator::cls).mkString(" "))), brack)
   }
   /**
    * wraps brackets around argument
    * @param brackets None/Some(true)/Some(false) for no/hidden/shown brackets
    */
   protected def wrapBrackets(brackets: Option[Boolean], body: => Unit)(implicit pc: PresentationContext) {
      pc.out(openTag("mrow", dataattibs))
      brackets foreach {b => pc.out(bracket(b, true))}
      body
      brackets foreach {b => pc.out(bracket(b, false))}
      pc.out(closeTag("mrow"))
   }
   override def doBracketedGroup(body: => Unit)(implicit pc: PresentationContext) {
      wrapBrackets(Some(false), body)
   }
   override def doUnbracketedGroup(body: => Unit)(implicit pc: PresentationContext) {
      wrapBrackets(None, body)
   }
   override def doOptionallyBracketedGroup(body: => Unit)(implicit pc: PresentationContext) {
      wrapBrackets(Some(true), body)
   }
   private def doOptional(group: String, body: => Unit)(implicit pc: PresentationContext) {
      pc.out(openTag("mrow", List(("class", s"$group ${hidden(group)}"))))
      body
      pc.out(closeTag("mrow"))
   }
   override def doImplicit(body: => Unit)(implicit pc: PresentationContext) {
      doOptional(implicitarg, body)
   }
   override def doInferredType(body: => Unit)(implicit pc: PresentationContext) {
      doOptional(reconstructedtype, body)
   }

   /** wraps continuations in mrow to make sure they produce a single element */
   private def R(c: Cont)(implicit pc: PresentationContext) = pc.html.mrow {c()}
   override def doScript(main: => Unit, sup: Option[Cont], sub: Option[Cont], over: Option[Cont], under: Option[Cont])(implicit pc: PresentationContext) {
      import pc.html._
      def underover(mbp: Cont) {
         (under, over) match {
            case (Some(u), Some(o)) => munderover {mbp(); R(u); R(o)}
            case (Some(u), None)    => munder     {mbp(); R(u)}
            case (None, Some(o))    => mover      {mbp(); R(o)}
            case (None,None)        => mbp()
         }
      }
      def subsup() {
         (sub, sup) match {
            case (Some(b), Some(p)) => pc.html.msubsup {main; R(b); R(p)}
            case (Some(b), None)    => pc.html.msub    {main; R(b)}
            case (None, Some(p))    => pc.html.msup    {main; R(p)}
            case (None,None)        => main
         }
      }
      underover {subsup _}
   }
   override def doFraction(above: List[Cont], below: List[Cont], line: Boolean)(implicit pc: PresentationContext) {
      pc.html.mfrac {
         pc.html.mrow(attributes = List("linethickness" -> (if (line) "" else "0px"))) {
            above foreach {a => a()}
         }
         pc.html.mrow {
            below foreach {b => b()}
         }
      }
   }

   override def doTd(ms : List[Cont])(implicit pc: PresentationContext) {
     pc.html.td {
       pc.html.mrow {
         ms foreach {a => a()}
       }
     }
   }

   override def doTr(ms : List[Cont])(implicit pc: PresentationContext) {
     pc.html.tr {
       ms foreach {a => a()}
     }
   }

   override def doTable(ms : List[Cont])(implicit pc: PresentationContext) {
     pc.html.table {
       ms foreach {a => a()}
     }
   }
   override def doSqrt(args : List[Cont])(implicit pc: PresentationContext) {
     pc.html.msqrt {
       pc.html.mrow {
         args foreach{a => a()}
       }
     }
   }

   override def doRootMarker(base : List[Cont], root : List[Cont])(implicit pc: PresentationContext) {
     if(root != Nil){
       pc.html.mroot{
         pc.html.mrow {
         base foreach(a => a())
         }
         doSqrt(base)
       }
     }
     else doSqrt(base)
   }

   override def doNumberMarker(arg : Delim)(implicit pc: PresentationContext) {
     pc.html.mn {
       pc.out(arg.s)
     }
   }

   override def doIdenMarker(arg : Delim)(implicit pc: PresentationContext) {
     pc.html.mi {
       pc.out(arg.s)
     }
   }

   override def doErrorMarker(args: List[Cont])(implicit pc: PresentationContext){
     pc.html.merror{
     args foreach{m => m()}
     }
   }

   override def doPhantomMarker(args: List[Cont])(implicit pc: PresentationContext){
     pc.html.mphantom{
     args foreach {a => a()}
     }
   }
   override def doGlyphMarker(arg : Delim, alt : String = "Failed to load")(implicit pc:PresentationContext){
      pc.html.mglyph(attributes = List("src" -> arg.s, "alt"->alt) ) {}
   }

   override def doLabelMarker(args : List[Cont], label : String) (implicit pc:PresentationContext) {
     pc.html.mlabel{
       pc.html.mtext{
         pc.out(label)
       }
       pc.html.mrow{
         args foreach{m => m()} // have to be TdMarkers
       }
     }
   }

   override def doTextMarker(text : Delim)(implicit pc: PresentationContext){
     pc.html.mtext{
       pc.out(text.s)
     }
   }
}