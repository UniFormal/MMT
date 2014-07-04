package info.kwarc.mmt.api.presentation

import info.kwarc.mmt.api._
import symbols._
import patterns._
import objects._
import documents._
import modules._
import symbols._
import objects.Conversions._
import notations._
import parser.SourceRef

/** This class stores information about a bound variable.
 * @param decl the variable declaration
 * @param binder the path of the binder (if atomic)
 * @param declpos the position of the variable declaration 
 */
case class VarData(decl : VarDecl, binder : Option[GlobalName], declpos : Position) {
   /** the variable name */
   def name = decl.name
}

case class PresentationContext(rh: RenderingHandler, owner: Option[CPath], ids: List[(String,String)], 
      source: Option[SourceRef], pos : Position, context : List[VarData], style: Option[PresentationContext => String]) {
   def out(s: String) {rh(s)}
   def child(i: Int) = copy(pos = pos / i)
   def child(p: Position) = copy(pos = pos / p)
   def addCon(con: List[VarData]) = copy(context = context ::: con)
   val html = utils.HTML(out _)
}

/** 
 * presents objects using notations
 * 
 * The main methods do not produce any rendering themselves.
 * Instead, they call special methods that may be overridden for customization.
 * The default implementations produce plain text.
 *
 * The bracket placement algorithm is only approximate.
 * It will sometimes put too many and sometimes too few brackets.
 * The latter will confuse the NotationBasedParser, but rarely humans.    
 */
class NotationBasedPresenter extends ObjectPresenter {
   /**
    * called by doDefaultTerm to render symbols
    */
   def doIdentifier(p: ContentPath)(implicit pc: PresentationContext) {
      val s = p match {
         case OMMOD(m) % name => name.toPath  //not parsable if there are name clashes 
         case _ => p.toPath
      }
      pc.out(s)
   }
   /**
    * called by doDefaultTerm to render variables
    */
   def doVariable(n: LocalName)(implicit pc: PresentationContext) {
      pc.out(n.toPath)
   }
   /**
    * called by doDefaultTerm to render literals
    */
   def doLiteral(l: OMLITTrait)(implicit pc: PresentationContext) {
      pc.out(l.toString)
   }
   /**
    * called by various methods to render MMT-level operators, such as ,:=()
    */
   def doOperator(s: String)(implicit pc: PresentationContext) {
      pc.out(s)
   }
   /**
    * called on every delimiter that is rendered through a notation
    * @param p the path of the rendered notation
    * @param d the delimiter
    * @param implicits implicit arguments of the rendered term that are not explicitly placed by the notation (added to first delimiter)
    */
   def doDelimiter(p: GlobalName, d: Delimiter, implicits: List[Cont])(implicit pc: PresentationContext) {
      pc.out(d.text)
   }
   /**
    * called by various methods to render whitespace
    * @param level how big a space to produce, 0 for no space, higher levels also indicate line-breaking points
    */
   def doSpace(level: Int)(implicit pc: PresentationContext) {
      Range(0,level).foreach {_ => pc.out(" ")}
   }
   
   /**
    * called once at the toplevel of every object to be rendered 
    */
   def doToplevel(o : Obj)(body: => Unit)(implicit pc: PresentationContext) {
      body
   }
   
   /**
    * called to wrap around subexpressions that must be bracketed
    * @param body the part between the brackets
    */
   def doBracketedGroup(body: => Unit)(implicit pc: PresentationContext) {
      doOperator("(")
      body
      doOperator(")")
   }
   /**
    * called to wrap around subexpressions that are not bracketed
    * @param body the part between the brackets
    */
   def doUnbracketedGroup(body: => Unit)(implicit pc: PresentationContext) {
      body
   }
   /**
    * called to wrap around subexpressions that could but do not have to be bracketed
    * @param body the part between the brackets
    */
   def doOptionallyBracketedGroup(body: => Unit)(implicit pc: PresentationContext) {
      doUnbracketedGroup(body)
   }
   /**
    * called to wrap around subexpressions that are implicit arguments
    * @param body the argument
    */
   def doImplicit(body: => Unit)(implicit pc: PresentationContext) {}
   /**
    * called to wrap around infered types of bound variables
    * @param body the argument
    */
   def doInferredType(body: => Unit)(implicit pc: PresentationContext) {}
   
   /** auxiliary type for a continuation function */
   type Cont = Unit => Unit
   /** called to render a scripted object - an optional decorated by several optional scripts
    *  
    *  each script is passed as a continuation that must be called at the appropriate place
    *  @param main the object
    *  
    * See [[parser.ScriptMarker]] for the meaning of the scripts
    */
   def doScript(main: => Unit, sup: Option[Cont], sub: Option[Cont], over: Option[Cont], under: Option[Cont])(implicit pc: PresentationContext) {
      def aux(sOpt: Option[Cont], oper: String) {sOpt match {
            case Some(script) => doOperator(oper); script()
            case None =>
      }}
      main
      aux(under, "__")
      aux(over, "^^")
      aux(sub, "_")
      aux(sup, "^")
      doSpace(1)
   }
   
   def doFraction(above: List[Cont], below: List[Cont], line: Boolean)(implicit pc: PresentationContext) {
      doBracketedGroup {
         above.head()
         above.foreach {e => 
            doSpace(1)
            e()
         }
      }
      doOperator("/")
      doBracketedGroup {
         below.foreach {e => 
            doSpace(1)
            e()
         }
      }
   }
   
   def doSqrt(args : List[Cont])(implicit pc: PresentationContext) {
     doOperator("√")
     doBracketedGroup {
       args.head
       args.tail.foreach {e =>
         doSpace(1)
         e()
       }
     }
   }
   
   def doNumberMarker(arg : Delim)(implicit pc: PresentationContext) {
     doOperator("#num_" + arg.s)
   }
   def doIdenMarker(arg : Delim)(implicit pc: PresentationContext) {
     doOperator("#id_" + arg.s)
   }

   
   /** 1 or 2-dimensional notations, true by default */
   def twoDimensional : Boolean = true
   
   /**
    * @param o the object to be presented
    * @return an object o' that is presented instead (e.g., o itself)
    *         one position p for each component c of o' such that the c is the p-subobject of o
    *         a notation to use for presenting o'
    */ 
   implicit protected def getNotation(p: GlobalName): Option[TextNotation] = {
      Presenter.getNotation(controller, p, twoDimensional)
   }
   /**
    * called on objects for which no notation is available
    * @return 1/0/-1 depending on the type of bracketing applied (yes/optional/no)
    */
   def doDefault(o: Obj)(implicit pc: PresentationContext): Int = o match {
      case OMID(p) =>
         doIdentifier(p)
         -1
      case OMV(n) =>
         doVariable(n)
         -1
      case l: OMLITTrait =>
         doLiteral(l)
         -1
      case ComplexTerm(op, subs, con, args) =>
         val vardata = con.map {v => VarData(v, Some(op), pc.pos)}
         doBracketedGroup {
            doIdentifier(op)
            subs.zipWithIndex.foreach {case (s,i) =>
               recurse(s)(pc.child(i+1))
            }
            doSpace(1)
            if (! con.isEmpty) {
               doOperator("[")
               con.zipWithIndex.foreach {case (v,i) =>
                  recurse(v)(pc.child(subs.length+i+1).addCon(vardata.take(i)))
               }
               doOperator("]")
            }
            args.zipWithIndex.foreach {case (t,i) =>
               doSpace(1)
               recurse(t)(pc.child(subs.length+con.length+i+1).addCon(vardata))
            }
         }
         1
      case OMA(f,args) =>
         // only applies in unusual cases where f is not atomic
         doBracketedGroup {
            val comps = (f::args).zipWithIndex
            comps.init.foreach {case (t,i) =>
               recurse(t)(pc.child(i))
               doSpace(1)
            }
            recurse(comps.last._1)(pc.child(comps.last._2))
         }
         1
      case OMBINDC(b,c,s) =>
         // only applies in unusual cases where b is not atomic
         val binder = b match {
            case OMA(OMS(p),_) => Some(p)
            case OMS(p) => Some(p)
            case _ => None
         }
         val vardata = c.map {v => VarData(v, binder, pc.pos)}
         doBracketedGroup {
            recurse(b)(pc.child(0))
            doSpace(1)
            doOperator("[")
            c.zipWithIndex.foreach {case (v,i) =>
               recurse(v)(pc.child(i+1).addCon(vardata.take(i)))
            }
            doOperator("]")
            s.zipWithIndex.foreach {case (t,i) =>
               doSpace(1)
               recurse(t)(pc.child(c.length+i+1).addCon(vardata))
            }
         }
         1
      case OMSemiFormal(parts) => parts.foreach {

            case Formal(t) => recurse(t)
            case objects.Text(format, t) => pc.out(t)
            case XMLNode(n) => pc.out(n.toString)
         }
         1
      case VarDecl(n,tp,df, not) =>
         doVariable(n)
         tp foreach {t =>
            if (metadata.Generated.get(o)) {
               doInferredType {
                  doOperator(":")
                  recurse(t, noBrackets)(pc.child(1))
               }
            } else {
               doOperator(":")
               recurse(t, noBrackets)(pc.child(1))
            }
         }
         df foreach {d =>
            doOperator("=")
            recurse(d, noBrackets)(pc.child(2))
         }
         not foreach {n =>
            doOperator("#")
            doOperator(n.toString) //TODO make nicer
         }
         -1
      case Sub(n,t) =>
          if (n != OMV.anonymous) { //omiting assignment if label is missing 
            doVariable(n)
            doOperator("=")
          }
         recurse(t, noBrackets)
         -1
      case c: Context =>
         if (! c.isEmpty) {
            c.init.foreach {v =>
               recurse(v, noBrackets)
               doOperator(", ")
            }
            recurse(c.last, noBrackets)
         }
         -1
      case s: Substitution =>
         if (! s.isEmpty) {
            s.init.foreach {c =>
               recurse(c, noBrackets)
               doOperator(", ")
            }
            recurse(s.last, noBrackets)
         }
         -1
   }
   
   def apply(o: Obj, origin: Option[CPath])(implicit rh : RenderingHandler) {
      implicit val pc = PresentationContext(rh, origin, Nil, None, Position.Init, Nil, None)
      doToplevel(o) {
         recurse(o)
      }
   }

   /** abbreviation for not bracketing */
   private val noBrackets = (_: TextNotation) => -1
   protected def recurse(obj: Obj)(implicit pc: PresentationContext): Int = recurse(obj, noBrackets)(pc)
   /** 
    *  @param bracket called to determine whether a non-atomic term rendered with a certain notation should be bracketed
    *  @return true if the term was bracketed
    */
   private def recurse(obj: Obj, bracket: TextNotation => Int)(implicit pcOrg: PresentationContext): Int = {
         val pc = pcOrg.copy(source = SourceRef.get(obj))
         val t = obj match {
            case t: Term => t
            case _ => return doDefault(obj)(pc)
         }
         controller.pragmatic.makePragmatic(t) match {
            case None =>
               doDefault(obj)(pc)
            case Some(objP) =>
               val PragmaticTerm(op, subargs, context, args, attrib, not, pos) = objP
               val firstVarNumber = subargs.length+1
               val firstArgNumber = subargs.length+context.length+1
               /*
                * @param ac the component as which the child occurs
                * @param child the child into which we recurse 
                * @param currentPosition the position from where we recurse
                *         -1: left-open argument; 0: middle argument; 1: right-open
                * @return the result of recursing into the child
                */
               def doChild(ac: ArityComponent, child: Obj, currentPosition: Int) = {
                  // the bracketing function
                  val precedence = ac.precedence match {
                    case Some(prec) => prec
                    case None => not.precedence //take the notation precedence as the default
                  }
                  val brack = (childNot: TextNotation) => Presenter.bracket(precedence, currentPosition, childNot)
                  // the additional context of the child
                  val newCont: Context = ac match {
                     case _: ArgumentComponent => context
                     case Var(n,_,_,_) => context.take(n-firstVarNumber)
                     case _ => Nil
                  }
                  val newVarData = newCont.map {v => VarData(v, Some(op), pc.pos)}
                  recurse(child, brack)(pc.child(pos(ac.number.abs)).addCon(newVarData))
               }
               // all implicit arguments that are not placed by the notation, they are added to the first delimiter
               val unplacedImplicits = not.arity.flatImplicitArguments(args.length).filter(i => ! not.fixity.markers.contains(i))
               var unplacedImplicitsDone = false
               /* processes a list of markers left-to-right
                *   for ArgumentMarkers, renders argument via doChild
                *   for DelimiterMarkers, renders delimiter via doDelimiter
                *   for presentation markers, recurses into groups and arranges them according to doXXX methods
                */
               def doMarkers(markers: List[Marker]) {
                  val numDelims = markers.count(_.isInstanceOf[Delimiter])
                  var numDelimsSeen = 0
                  def currentPosition = if (numDelimsSeen == 0) -1 else if (numDelimsSeen == numDelims) 1 else 0
                  // the while loop removes elements from markersLeft until it is empty
                  var markersLeft = markers
                  // the most recently removed marker
                  var previous : Option[Marker] = None
                  while (markersLeft != Nil) {
                     val current = markersLeft.head
                     markersLeft = markersLeft.tail
                     val compFollows = ! markersLeft.isEmpty && markersLeft.head.isInstanceOf[ArgumentMarker]
                     //val delimFollows = ! markersLeft.isEmpty && markersLeft.head.isInstanceOf[parser.Delimiter]
                     current match {
                        case c @ Arg(n,_) =>
                           doChild(c, args(n-firstArgNumber), currentPosition)
                           if (compFollows) doSpace(1)
                        case c @ ImplicitArg(n,_) =>
                           doImplicit {
                              doChild(c, args(n-firstArgNumber), currentPosition)
                              if (compFollows) doSpace(1)
                           }
                        case c @ Var(n, typed, _,_) => //sequence variables impossible due to flattening
                           doChild(c, context(n-firstVarNumber), currentPosition)
                           if (compFollows) doSpace(1)
                        case c @ Subs(n, _) => 
                          doChild(c, subargs(n - 1), currentPosition)
                          if (compFollows) doSpace(1)
                        case AttributedObject =>
                           // we know attributee.isDefined due to flattening
                           //TODO
                        case d: Delimiter =>
                           val dE = d.expand(op)
                           val unpImps = if (unplacedImplicitsDone) Nil else unplacedImplicits map {
                              case c @ ImplicitArg(n,_) =>
                                  (_: Unit) => doChild(c, args(n-firstArgNumber), 0); ()
                           }
                           val letters = dE.text.exists(_.isLetter)
                           if (letters && previous.isDefined) doSpace(1)
                           doDelimiter(op, dE, unpImps)(pc.copy(pos = pc.pos / pos(0)))
                           numDelimsSeen += 1
                           if (letters && !markersLeft.isEmpty) doSpace(1)
                        case s: SeqArg => //impossible due to flattening
                        case GroupMarker(ms) =>
                           doUnbracketedGroup { doMarkers(ms) }
                        case s: ScriptMarker =>
                           def aux(mOpt: Option[Marker]) = mOpt.map {m => (_:Unit) => doMarkers(List(m))} 
                           doScript(doMarkers(List(s.main)), aux(s.sup), aux(s.sub), aux(s.over), aux(s.under))
                        case FractionMarker(a,b,l) =>
                           def aux(m: Marker) = (_:Unit) => doMarkers(List(m)) 
                           doFraction(a map aux, b map aux, l)
                        case SqrtMarker(markers) => 
                           def aux(m: Marker) = (_:Unit) => doMarkers(List(m)) 
                           doSqrt(markers map aux)
                        case NumberMarker(value) => 
                           doNumberMarker(value)
                        case IdenMarker(value) => 
                           doIdenMarker(value)
                        case InferenceMarker =>
                        case _ => //TODO
                     }
                     previous = Some(current)
                  }
               }
               val br = bracket(not)
               val flatMarkers = not.arity.flatten(not.presentationMarkers,context.length, args.length, attrib)
               br match {
                  case n if n > 0 => doBracketedGroup { doMarkers(flatMarkers) }
                  case 0 =>          doOptionallyBracketedGroup { doMarkers(flatMarkers) }
                  case n if n < 0 => doUnbracketedGroup { doMarkers(flatMarkers) }
               }
               br
         }
   }
}