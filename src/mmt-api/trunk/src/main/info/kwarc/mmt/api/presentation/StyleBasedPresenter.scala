package info.kwarc.mmt.api.presentation
import info.kwarc.mmt.api._
import frontend._
import objects._
import objects.Conversions._
import utils._
import parser._

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
case class LocalParams(ids : List[(String,String)], pos : Position,
                       inObject : Boolean, bracketInfo : Option[BracketInfo], context : List[VarData]) {
   def asContext = Context(context map (_.decl) : _*)
}
object LocalParams {
   val objectTop = LocalParams(Nil, Position.Init, true, None, Nil)
}
/** This class stores information about a bound variable.
 * @param decl the variable declaration
 * @param binder the path of the binder (if atomic)
 * @param declpos the position of the variable declaration 
 */
case class VarData(decl : VarDecl, binder : Option[GlobalName], declpos : Position) {
   /** the variable name */
   def name = decl.name
}

/** A special presentable object that is wrapped around the toplevel of the presented expression if it is a declaration.
 * @param c the presented expression
 */
case class StrToplevel(c: Content) extends Content {
   def components = List(c)
   def role = c.role
   def governingPath = c.governingPath
   def toNode = c.toNode
}
/** A special presentable object that is wrapped around the math objects encountered during presentation
 * @param c the presented object
 * @param opath the position of the object (if known)
 */
case class ObjToplevel(c: Obj, opath: Option[OPath]) extends Content {
   def components = List(c)
   def role = c.role
   def governingPath = c.governingPath
   def toNode = c.toNode
}

/** The presentation class
 * @param controller the controller storing all information about MMT expressions and notations
 * @param report the logging handler
 * @param style the notation style used for presentation
 */
class StyleBasedPresenter(c : Controller, style: MPath) extends Presenter {
   init(c, Nil)
   override val logPrefix = "presenter"
   
   def isApplicable(format: String) = format == style.toPath
   def apply(s : StructuralElement, rh: RenderingHandler) {
      val gpar = GlobalParams(rh, style)
      val lpar = LocalParams.objectTop.copy(inObject = false)
      present(StrToplevel(s), gpar, lpar)
   }
   def apply(o: Obj, rh: RenderingHandler) {
      val gpar = GlobalParams(rh, style)
      val lpar = LocalParams.objectTop
      present(ObjToplevel(o, None), gpar, lpar)
   }

   /** the main presentation method
    * @param c the presented expression
    * @param gpar the parameters that do not change during rendering
    * @param lpar the parameters that change during rendering
    */
   protected def present(c : Content, gpar : GlobalParams, lpar : LocalParams) {
      log("presenting: " + c)
      c match {
         case StrToplevel(c) => 
            val key = NotationKey(None, Role_StrToplevel)
            val notation = controller.get(gpar.nset, key)
            render(notation.presentation, ContentComponents(List(c)), None, List(0), gpar, lpar)
         case ObjToplevel(c, opath) =>
            val key = NotationKey(None, Role_ObjToplevel)
            val notation = controller.get(gpar.nset, key)
            val opComps : List[Content] = opath match {
               case None => List(Omitted, Omitted)
               case Some(p) => List(StringLiteral(p.parent.toPath), StringLiteral(p.component))
            }
            render(notation.presentation, ContentComponents(c :: opComps), None, List(0), gpar, lpar)
         case l: Literal =>
            gpar.rh(l)
         case s: StructuralElement =>
            val key = NotationKey(Some(s.path), s.role)
            val notation = controller.get(gpar.nset, key)
            render(notation.presentation, s.contComponents, None, List(0), gpar, lpar)
         case s:SemiFormalObject =>
            s.components.foreach(c => present(c, gpar, lpar)) //could be much better
         case o1: Obj =>
            val (o, posP, notationOpt) = o1 match {
               case t: Term => getNotation(t)
               case _ => (o1, Position.positions(o1), None)
            }
            //default values
            var key = NotationKey(o.head, o.role)
            var newlpar = lpar
            var comps = o.components
            var namedInds : List[(String,Int)] = Nil
            //some adjustments for certain objects 
            o match {
               //for binders, change newlpar to remember VarData for rendering the bound variables later 
               case OMBINDC(binder,context,_) =>
                  val pOpt = binder match {case OMS(b) => Some(b) case _ => None}
                  val vds = context.zipWithIndex.map {
                      case (v, i) => VarData(v, pOpt, newlpar.pos / posP(i+1))
                  }
                  newlpar = newlpar.copy(context = newlpar.context ::: vds)
               //for bound variables, look up VarData   
               case OMV(name) =>
                  newlpar.context.reverse.zipWithIndex.find(_._1.name == name) match {
                     case Some((VarData(_, binder, pos), i)) =>
                        comps = List(StringLiteral(name.toString), StringLiteral(i.toString), StringLiteral(pos.toString))
                        key = NotationKey(binder, o.role)
                     case None =>
                       comps = List(StringLiteral(name.toString), Omitted, Omitted) // free variable
                  }
               case _ =>
            }
            val presentation = o match {
               case ComplexTerm(p, args, vars, scs) =>
                  val numArgs = args.length
                  val numVars = vars.length
                  val numScs  = scs.length
                  notationOpt match {
                     case Some(notation) =>
                        val pres = notation.presentation(numArgs, numVars, numScs)
                        lpar.bracketInfo match {
                           case None => NoBrackets(pres)
                           case Some(BracketInfo(precOpt, delOpt)) =>
                              val outerPrecedence = precOpt.getOrElse(Precedence.neginfinite)
                              val delimitation = delOpt.getOrElse(0)
                              val brack = Presenter.bracket(outerPrecedence, delimitation, notation)
                              if (brack > 0) Brackets(pres)
                              else if (brack < 0) NoBrackets(pres)
                              else EBrackets(pres)
                        }
                     case None =>
                        // default presentation
                        implicit def convert(i:Int) = NumberedIndex(i)
                        val bi = BracketInfo(Some(Precedence.infinite)) // maximal bracketing since we don't know anything
                        var pres: Presentation = Component(0,bi)
                        if (numArgs > 0) pres += OpSep() + Iterate(1, numArgs, ArgSep(), bi)
                        if (numVars > 0) pres += OpSep() + Iterate(numArgs+1, numArgs+numVars, ArgSep(), bi)
                        if (numScs > 0)  pres += OpSep() + Iterate(numArgs+numVars+1, -1, ArgSep(), bi)
                        Brackets(pres)
                  }
               case _ =>
                  val notation = controller.get(gpar.nset, key)
                  notation.presentation
            }
            val contComps = ContentComponents(comps, Nil, None, Some(o))
            render(presentation, contComps, Some(posP), List(0), gpar, newlpar)
      }
   }
   
   /**
    * transform Presentation into output
    * 
    * @param pres the Presentation to render
    * @param comps the components (content objects) available for recursive rendering
    * @param compPos the positions that the components have within the object to be rendered (defaults to 0, 1, ...)
    *   if given, its length must be the number of components
    * @param ind the list of indices in nested maps
    * @param gpar the global parameters, in particular the RenderingHandler
    * @param lpar the local parameters
    */
   protected def render(pres : Presentation, comps : ContentComponents, compPos: Option[List[Position]], 
                        ind : List[Int], gpar : GlobalParams, lpar : LocalParams) {
      def resolve(i: CIndex) : Int = comps.resolve(i).getOrElse(throw PresentationError("undefined index: " + i))
      def getComponent(i: CIndex): Content = {
         val pos = resolve(i)
         val p = if (pos < 0) pos + comps.length else pos
         comps(p)
      }
      implicit def int2CInxed(i: Int) = NumberedIndex(i)
      def recurse(p : Presentation) {render(p, comps, compPos, ind, gpar, lpar)}
      log("rendering " + pres)
      val pos = compPos.getOrElse(Range(0,comps.length).toList.map(Position(_))) 
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
            render(body, comps, compPos, current :: ind, gpar, lpar)
            current += step
            if (step > 0 && current <= last || step < 0 && current >= last)
               recurse(sep)
            else
               recurse(post)
            recurse(Components(NumberedIndex(current), Presentation.Empty, last, post, step, sep, body))
        case Id => gpar.rh(lpar.pos.toString)
        case Index => gpar.rh(lpar.pos.current.toString)
        case Neighbor(offset, bi) =>
            val i = ind.head + offset
            if (i < 0 || i >= comps.length)
               throw new PresentationError("offset out of bounds")
            comps(i) match {
               case o: Obj =>
                  if (lpar.inObject)
                     present(o, gpar, lpar.copy(pos = lpar.pos / pos(i), bracketInfo = Some(bi)))
                  else 
                     //transition from structural to object level
                     present(ObjToplevel(o, comps.getObjectPath(i)), gpar, LocalParams.objectTop)
               case c => present(c, gpar, lpar)
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
            render(pres, comps, compPos, current :: ind, gpar, lpar)
        case TheNotationSet =>
            gpar.rh(gpar.nset.toPath.replace("?","%3F"))
        case Hole(i, default) => recurse(default)
        case GenerateID(name, scope) =>
           val id = "generated_" + StyleBasedPresenter.getNextID
           val newlpar = lpar.copy(ids = (name, id) :: lpar.ids)
           render(scope, comps, compPos, ind, gpar, newlpar)
        case UseID(name) => lpar.ids find (_._1 == name) match {
            case None => throw PresentationError("undeclared ID: " + name)
            case Some(i) => gpar.rh(i._2)
        }
        case Fragment(name, args @ _*) =>
             val notation = controller.get(gpar.nset, NotationKey(None, Role_Fragment(name)))
             val pres = notation.presentation.fill(args : _*)
             //log("found fragment notation: " + notation)
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
                 val tp = try {found.inference(o, lpar.asContext)(controller.globalLookup)}
                        catch {case _ : Throwable => OMID(utils.mmt.mmtbase ? "dummy" ? "error")}
                 present(tp, gpar, lpar)
              case c => throw PresentationError("cannot infer type of " + c)
           }
     }
   }
}

/** static state for StyleBasedPresenter in order to avoid generating the same ID twice */
object StyleBasedPresenter {
   private var nextID : Int = 0 // the next available id (for generating unique ids)
   def getNextID = {
      nextID += 1
      nextID
   }
}

//TODO: Content.presentation(lpar: LocalParams) : (ByNotation, LocalParams) instead of Content.presentation?
//store current MMTBase in lpar
// present symbols based on whether they are meta, included, local
//presentation item for relativized path; format string?
