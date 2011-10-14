package info.kwarc.mmt.api.presentation
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
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
case class LocalParams(pos : Position, iPrec : Option[Precedence], context : List[VarData], inObject : Boolean, ids : List[(String,String)]) {
   def asContext = Context(context map (_.decl) : _*)
}

/** This class stores information about a bound variable.
 * @param decel the variable declaration
 * @param binder the path of the binder
 * @param declpos the position of the variable declaration 
 */
case class VarData(decl : VarDecl, binder : GlobalName, declpos : Position) {
   /** the variable name */
   def name = decl.name
}

/** A special presentable object that is wrapped around the toplevel of the presented expression if it is a declaration.
 * @param c the presented expression
 */
case class StrToplevel(c: Content) extends Content {
   def presentation(lpar : LocalParams) =
      ByNotation(NotationKey(None, Role_StrToplevel), ContentComponents(List(c)), lpar)
   def toNode = c.toNode
}
/** A special presentable object that is wrapped around the math objects encountered during presentation
 * @param c the presented object
 * @param pos the position of the object
 */
case class ObjToplevel(c: Obj, opath: Option[OPath]) extends Content {
   private def components : List[Content] = {
      val l = opath match {
         case None => List(Omitted, Omitted)
         case Some(p) => List(StringLiteral(p.parent.toPath), StringLiteral(p.component))
      }
      c :: l
   }
   def presentation(lpar : LocalParams) =
      ByNotation(NotationKey(None, Role_ObjToplevel), ContentComponents(components), lpar)
   def toNode = c.toNode
}

/** The presentation class
 * @param controller the controller storing all information about MMT expressions and notations
 * @param report the logging handler
 */
class Presenter(controller : frontend.Controller, report : info.kwarc.mmt.api.frontend.Report) { 
   private def log(s : => String) = report("presenter", s)
   private var nextID : Int = 0 // the next available id (for generating unique ids)
   /** the main presentation method
    * @param c the presented expressions
    * @param gpar the rendering handler that collects the output and the style to be used
    */
   def apply(c : Content, gpar : GlobalParams) {
      c match {
        case c : objects.Obj => present(ObjToplevel(c, None), gpar, LocalParams(Position.Init, None, Nil, true, Nil))
        case _ => present(StrToplevel(c), gpar, LocalParams(Position.Init, None, Nil, false, Nil))
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
      c.presentation(lpar) match {
		   case IsLiteral(l) => gpar.rh(l)
		   case bn : ByNotation => doByNotation(bn)
      }
   }

   protected def render(pres : Presentation, comps : ContentComponents, 
                        ind : List[Int], gpar : GlobalParams, lpar : LocalParams) {
      def resolve(i: CIndex) : Int = comps.resolve(i).getOrElse(throw PresentationError("undefined index: " + i))
      def getComponent(i: CIndex): Content = {
	      val pos = resolve(i)
         val p = if (pos < 0) pos + comps.length else pos
	      comps(p)
      }
      implicit def int2CInxed(i: Int) = NumberedIndex(i)
      def recurse(p : Presentation) {render(p, comps, ind, gpar, lpar)}
      pres match {
		  case Text(text) =>
		      var t = text
		      lpar.ids foreach {
		         case (n,i) => t = t.replace(n, i)
		      }
		      gpar.rh(t)
		  case Element(prefix, label, attributes, children) =>
		      gpar.rh.elementStart(prefix, label)
		      attributes.foreach {case Attribute(prefix, name, value) =>
			      gpar.rh.attributeStart(prefix, name)
			      recurse(value)
			      gpar.rh.attributeEnd
		      }
		      if (comps.length > 0) {
		        comps(0) match {
		          case s : StructuralElement => //TODO this should be called less often
                  gpar.rh.attributeStart("","id")
                  gpar.rh.apply(s.path.toPath)
                  gpar.rh.attributeEnd
		          case OMID(path) => 
                  gpar.rh.attributeStart("","id")
                  gpar.rh.apply(path.toPath)
                  gpar.rh.attributeEnd
		          case _ => 
		        }
		      }
		      children.foreach(recurse)
		      gpar.rh.elementEnd
		  case PList(items) =>
		      items.foreach(recurse)	
		  case If(cind, test, yes, no) =>
		      val pos = resolve(cind)
		      val p = if (pos < 0) pos + comps.length else pos
		      val exists = p >= 0 && p < comps.length
		      val testresult = exists && (test match {
		         case "present" => comps(p) != Omitted
		         case "atomic" => comps(p) match {
		            case OMID(_) => true
		            case OMMOD(_) => true
		            case _ => false
		         }
		      })
		      if (testresult) recurse(yes) else recurse(no)
		  case IfHead(cind, path, yes, no) =>
		      val pos = resolve(cind)
		      val p = if (pos < 0) pos + comps.length else pos
            val yesno = if (p >= 0 && p < comps.length)
               comps(p) match {
                   case o : Obj if o.head == Some(path) => yes
                   case _ => no
               }
            else 
		         no
		      recurse(yesno)
	      case Components(begInd, pre, endInd, post, step, sep, body) =>
		      val begin = resolve(begInd)
		      val end = resolve(endInd)
		      val l = comps.length
		      var current = if (begin < 0) begin + l else begin
		      val last = if (end < 0) end + l else end
		      if (step > 0 && current > last || step < 0 && current < last) return
		      if (current < 0 || current >= l || last < 0 || last >= l || step == 0)
		         throw new PresentationError("begin/end/step out of bounds in " + pres)
		      recurse(pre)
		      render(body, comps, current :: ind, gpar, lpar)
		      current += step
		      if (step > 0 && current <= last || step < 0 && current >= last)
		         recurse(sep)
		      else
		         recurse(post)
		      recurse(Components(NumberedIndex(current), Presentation.Empty, last, post, step, sep, body))
		  case Index => gpar.rh(lpar.pos.toString)
		  case Neighbor(offset, iPrec) =>
		      val i = ind.head + offset
		      if (i < 0 || i >= comps.length)
		         throw new PresentationError("offset out of bounds")
            val newLpar = lpar.copy(pos = lpar.pos + i, iPrec = iPrec)
            comps(i) match {
		         //transition from structural to object level
		         case o: Obj if ! lpar.inObject => present(ObjToplevel(o, comps.getObjectPath(i)), gpar, newLpar.copy(pos = Position.None, inObject = true))
		         case c => present(c, gpar, newLpar)
		      }
	      case Nest(begInd, endInd, stepcase, basecase) =>
		      val begin = resolve(begInd)
            val end = resolve(endInd)
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
		  case GenerateID(name, scope) =>
		     val id = "generated_" + nextID
		     nextID += 1
		     val newlpar = lpar.copy(ids = (name, id) :: lpar.ids)
		     render(scope, comps, ind, gpar, newlpar)
		  case UseID(name) => lpar.ids find (_._1 == name) match {
		      case None => throw PresentationError("undeclared ID: " + name)
		      case Some(i) => i._2
		  }
		  case Fragment(name, args @ _*) =>
             val notation = controller.get(gpar.nset, NotationKey(None, Role_Fragment(name)))
		       val pres = notation.pres.fill(args : _*)
             log("found fragment notation: " + notation)
             recurse(pres)
		  case Compute(iOpt, f) =>
           val o = iOpt match {
              case None => comps.obj.getOrElse(throw PresentationError("no object found"))
              case Some(i) => getComponent(i)
           }
		     if (f != "infer") throw PresentationError("undefined function: " + f)
		     o match {
		        case o: Term =>
		           val meta = controller.notstore.get(gpar.nset).from match {
		              case m : MPath => m
		              case _ => throw PresentationError("no foundation found")
		           }
		           val found = controller.extman.getFoundation(meta).getOrElse(throw PresentationError("no foundation found"))
		           val tp = try {found.infer(o, lpar.asContext)(controller.globalLookup)}
		              catch {case e => OMID(utils.mmt.mmtbase ? "dummy" ? "error")} 
		           present(tp, gpar, lpar)
		        case c => throw PresentationError("cannot infer type of " + c)
		     }
	  }
   }
}

//TODO: Content.presentation(lpar: LocalParams) : (ByNotation, LocalParams) instead of Content.presentation?
//store current MMTBase in lpar
// present symbols based on whether they are meta, included, local
//presentation item for relativized path; format string?
