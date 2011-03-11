package info.kwarc.mmt.api.presentation
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects.{Position,Obj}
import info.kwarc.mmt.api.utils._
import scala.xml.{Node,NodeSeq}
import scala.collection.mutable._

/** This class collects the parameters that are globally fixed during one presentation task.
 * @param rh the rendering handler that collects the generated output
 * @param nset the style containing the notations 
 */
case class GlobalParams(rh : RenderingHandler, nset : MPath)
/** This class collects the parameters that vary locally during one presentation task.
 * @param pos the current position within the presented expressions
 * @param iPrec the input precedence possible provided by the parent level (used for bracket generation)
 * @param context some information about how to present the free variables that were bound on higher levels
 * @param inObject a flag to indicate whether the presented expression is a declaration or an object
 */
case class LocalParams(pos : Position, iPrec : Option[Precedence], context : List[VarData], inObject : Boolean) {
   def toggle = LocalParams(pos, iPrec, context, ! inObject)
}

/** This class stores information about a bound variable.
 * @param name the variable name
 * @param binder the path of the binder 
 */
case class VarData(name : String, binder : SPath, decl : Position)

/** A special presentable object that is wrapped around the toplevel of the presented expression if it is a declaration.
 * @param c the presented expression
 */
case class StrToplevel(c: Content) extends Content {
   def presentation(lpar : LocalParams) =
      ByNotation(NotationKey(None, Role_StrToplevel), List[Content](c), lpar)
   def toNode = c.toNode
} 
/** A special presentable object that is wrapped around the math objects encountered during presentation
 * @param c the presented object
 * @param pos the position of the object
 */
case class ObjToplevel(c: Obj, pos : Position) extends Content {
   def presentation(lpar : LocalParams) =
      ByNotation(NotationKey(None, Role_ObjToplevel), List[Content](c, XMLLiteral(c.toNodeID(pos+0))), lpar)
   def toNode = c.toNode
}

/** The presentation class
 * @param controller the controller storing all information about MMT expressions and notations
 * @param report the logging handler
 */
class Presenter(controller : frontend.ROController, report : info.kwarc.mmt.api.frontend.Report) { 
   private def log(s : => String) = report("presenter", s)

   /** the main presentation method
    * @param c the presented expressions
    * @param gpar the rendering handler that collects the output and the style to be used
    */
   def apply(c : Content, gpar : GlobalParams) {
      c match {
        case c : objects.Obj => present(ObjToplevel(c, Position.Init), gpar, LocalParams(Position.Init, None, Nil, true))
        case _ => present(StrToplevel(c), gpar, LocalParams(Position.Init, None, Nil, false))
      }
   }

   protected def present(c : Content, gpar : GlobalParams, lpar : LocalParams) {
      def doByNotation(bn : ByNotation) {
         val key = bn.key
         log("notation key: " + key)
         val notation = controller.get(gpar.nset, key)
         log("looked up notation: " + notation)
         val pres = notation.pres
         val presentation =
            (key.role.bracketable, lpar.iPrec) match {
               case (true, None) => NoBrackets(pres)                            //brackets excluded by higher level
               case (true, Some(ip)) =>
                  // error should be impossible as parser forces non-None value for bracketable roles
                  val op = notation.oPrec.getOrElse(throw PresentationError("notation for bracketable role must have precedence"))
                  // case-split according to how much stronger the outer operator binds than the inner one
                  // the stronger the inner operator binds, the less necessary its brackets are
                  // the outer operator decides how a tie is broken
                  (ip.prec - op.prec) match {
                     case Infinite    => Brackets(pres)
                     case Finite(i)   =>
                             if (i > 0)      Brackets(pres)
                        else if (i < 0)      EBrackets(pres, -i) 
                        else if (ip.loseTie) EBrackets(pres, -i)
                        else                 Brackets(pres)
                     case NegInfinite => NoBrackets(pres)
                  }
               case (false, _) => pres                                          //no brackets possible
            }
         log("rendering with components " + bn.components)
         render(presentation, bn.components, List(0), gpar, bn.lpar)
      }
      //transition between object and structural level
      (c, lpar.inObject) match {
         case (c : Obj, false) => present(ObjToplevel(c, lpar.pos), gpar, lpar.toggle)
         case (c : ContentElement, true) => present(StrToplevel(c), gpar, lpar.toggle)
         case _ => c.presentation(lpar) match {
		     case IsLiteral(l) => gpar.rh(l)
		     case bn : ByNotation => doByNotation(bn)
	     }
      }
   }

   protected def render(pres : Presentation, comps : List[Content], 
                        ind : List[Int], gpar : GlobalParams, lpar : LocalParams) {
      def recurse(p : Presentation) {render(p, comps, ind, gpar, lpar)}
      pres match {
		  case Text(text) =>
		      gpar.rh(text)
		  case Element(prefix, label, attributes, children) =>
		      gpar.rh.elementStart(prefix, label)
		      attributes.foreach {case Attribute(prefix, name, value) =>
			      gpar.rh.attributeStart(prefix, name)
			      recurse(value)
			      gpar.rh.attributeEnd
		      }
		      children.foreach(recurse)
		      gpar.rh.elementEnd
		  case PList(items) =>
		      items.foreach(recurse)	
		  case IfPresent(pos, yes, no) =>
		      val p = if (pos < 0) pos + comps.length else pos
		      if (p >= 0 && p < comps.length && comps(p) != Omitted)
		         recurse(yes)
		      else
		         recurse(no)
		  case IfHead(pos, path, yes, no) =>
		      val p = if (pos < 0) pos + comps.length else pos
              val yesno = if (p >= 0 && p < comps.length)
                 comps(p) match {
                   case o : Obj if o.head == Some(path) => yes
                   case _ => no
                 }
              else 
		         no
		      recurse(yesno)
	      case Components(begin, pre, end, post, step, sep, body) =>
		      val l = comps.length
		      var current = if (begin < 0) begin + l else begin
		      val last = if (end < 0) end + l else end
		      if (step > 0 && current > last || step < 0 && current < last) return
		      if (current < 0 || current >= l || last < 0 || last >= l || step == 0)
		         throw new PresentationError("begin/end/step out of bounds in " + this)
		      recurse(pre)
		      render(body, comps, current :: ind, gpar, lpar)
		      current += step
		      if (step > 0 && current <= last || step < 0 && current >= last)
		         recurse(sep)
		      else
		         recurse(post)
		      recurse(Components(current, Presentation.Empty, last, post, step, sep, body))
		  case Index => gpar.rh(lpar.pos.toString)
		  case Neighbor(offset, iPrec) =>
		      val i = ind.head + offset
		      if (i < 0 || i >= comps.length)
		         throw new PresentationError("offset out of bounds")
              val newLpar = LocalParams(lpar.pos + i, iPrec, lpar.context, lpar.inObject)
              present(comps(i), gpar, newLpar)
	      case Nest(begin, end, stepcase, basecase) =>
		      val l = comps.length
		      var current = if (begin < 0) begin + l else begin
		      val last = if (end < 0) end + l else end
              val step = if (current <= last) 1 else -1
		      if (step > 0 && current > last || step < 0 && current < last) return
		      if (current < 0 || current >= l || last < 0 || last >= l || step == 0)
		         throw new PresentationError("begin/end/step out of bounds in " + this)
		      val pres = if (step > 0 && current >= last || step < 0 && current <= last)
		         basecase
		      else
		         stepcase.fill(Nest(current + step, last, stepcase, basecase))
		      render(pres, comps, current :: ind, gpar, lpar)
		  case Id => gpar.rh(lpar.pos.toString)
		  case TheNotationSet =>
		      gpar.rh(gpar.nset.toPath.replace("?","%3F"))
		  case Hole(i, default) => recurse(default)
		  case Fragment(name, args @ _*) =>
             val notation = controller.get(gpar.nset, NotationKey(None, Role_Fragment(name)))
		     val pres = notation.pres.fill(args : _*)
             log("found fragment notation: " + notation)
             recurse(pres)
	  }
   }
}

//TODO: Content.presentation(lpar: LocalParams) : (ByNotation, LocalParams) instead of Content.presentation?
//store current MMTBase in lpar
// present symbols based on whether they are meta, included, local
//presentation item for relativized path; format string?
