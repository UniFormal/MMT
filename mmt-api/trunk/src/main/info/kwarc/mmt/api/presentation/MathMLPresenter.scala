package info.kwarc.mmt.api.presentation

import info.kwarc.mmt.api._
import notations._
import frontend._
import objects._

import utils.xml._

import HTMLAttributes._

/**
 * This overrides the appropriate methods present objects as MathML.
 * 
 * The MathML carries additional attributes
 * 
 * math: jobad:owner ([[ContentPath]]) and jobad:component ([[DeclarationComponent]])
 * 
 * mrow, mo, mi, mn: jobad:mmtsrc ([[parser.SourceRef]]) and jobad:mmtref ([[Position]])
 * 
 * mo (from OMS or delimiter): jobad:href ([[ContentPath]])
 * 
 * mi: jobad:varref ([[Position]]) pointing to its declaration 
 */
class MathMLPresenter extends NotationBasedPresenter {
   
   /** generalized apply method that takes a callback function to determine the css class of a subterm */
   def apply(o: Obj, origin: Option[CPath], style: PresentationContext => String)(implicit rh : RenderingHandler) {
      implicit val pc = PresentationContext(rh, origin, Nil, None, Position.Init, Nil, Some(style))
      doToplevel {
         recurse(o)
      }
   }

   protected def jobadattribs(implicit pc: PresentationContext) = {
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
      pc.html.mo(attributes = (symref -> p.toPath) :: jobadattribs) {
        super.doIdentifier(p)
      }
   }
   override def doVariable(n: LocalName)(implicit pc : PresentationContext) {
      val vdAtt = pc.context.find(_.decl.name == n) match {
         case None => Nil
         case Some(vd) => List(varref -> vd.declpos.toString)
      }
      pc.html.mi(attributes = vdAtt ::: jobadattribs) {
        super.doVariable(n)
      }
   }
   override def doLiteral(l: OMLITTrait)(implicit pc: PresentationContext) {
      pc.html.mn(attributes = jobadattribs) {
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
         case " " => element("mspace", ("width" -> ".2em") :: (symref -> p.toPath) :: jobadattribs, "")
         case t   => element("mo", (symref -> p.toPath) :: jobadattribs, t)
      }
      if (! implicits.isEmpty) {
         pc.html.mrow {
            pc.out(mo)
            implicits.foreach {i =>
               doSpace(1)
               doImplicit {i()}
            }
         }
      } else
         pc.out(mo)
   }
   override def doSpace(level: Int)(implicit pc : PresentationContext) {
      val ms = element("mspace", List(("width", "." + (2*level).toString + "em")), "")
      pc.out(ms)
   }
   override def doToplevel(body: => Unit)(implicit pc: PresentationContext) {
      val nsAtts = List("xmlns" -> namespace("mathml"))
      val mmtAtts = pc.owner match {
         case None => Nil
         case Some(cp) => List(owner -> cp.parent.toPath, component -> cp.component.toString, position -> "")
      }
      // <mstyle displaystyle="true">
      pc.out(openTag("math", nsAtts ::: mmtAtts))
      body
      pc.out(closeTag("math"))
   }
   private def bracket(optional: Boolean, open: Boolean) = {
      val cls = if (optional) "opt-brackets opt-brackets-hidden" else "brackets"
      val brack = if (open) "(" else ")"
      element("mo", List(("class", "operator " + cls)), brack)
   }
   /**
    * wraps brackets around argument
    * @param brackets None/Some(true)/Some(false) for no/hidden/shown brackets
    */
   protected def wrapBrackets(brackets: Option[Boolean], body: => Unit)(implicit pc: PresentationContext) {
      pc.out(openTag("mrow", jobadattribs))
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
      pc.out(openTag("mrow", List(("class", s"$group $group-hidden"))))
      body
      pc.out(closeTag("mrow"))
   }
   override def doImplicit(body: => Unit)(implicit pc: PresentationContext) {
      doOptional("implicit-arg", body)
   }
   override def doInferredType(body: => Unit)(implicit pc: PresentationContext) {
      doOptional("reconstructed", body)      
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