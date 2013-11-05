package info.kwarc.mmt.api.presentation

import info.kwarc.mmt.api._
import symbols._
import patterns._
import objects._
import documents._
import modules._
import symbols._
import objects.Conversions._
import parser._

/** 
 * This trait defines methods for presenting objects using notations.
 * 
 * It is intended to be mixed into Presenters that already define the presentation of the structural levels.
 * 
 * The main methods do not produce any rendering themselves.
 * Instead, they call special methods that may be overridden for customization.
 * The default implementations produce plain text.
 *
 * The bracket placement algorithm is only approximate.
 * It will sometimes put too many and sometimes too few brackets.
 * The latter will confuse the NotationBasedParser, but rarely humans.    
 */
trait NotationBasedPresenter extends ObjectPresenter {
   /**
    * called by doDefaultTerm to render symbols
    */
   def doIdentifier(p: ContentPath)(implicit rh : RenderingHandler) {
      val s = p match {
         case OMMOD(m) % name => name.toPath  //not parsable if there are name clashes 
         case _ => p.toPath
      }
      rh(s)
   }
   /**
    * called by doDefaultTerm to render variables
    */
   def doVariable(n: LocalName)(implicit rh : RenderingHandler) {
      rh(n.toPath)
   }
   /**
    * called by doDefaultTerm to render literals
    */
   def doLiteral(l: OMLiteral)(implicit rh : RenderingHandler) {
      rh(l.toString)
   }
   /**
    * called by various methods to render MMT-level operators, such as ,:=()
    */
   def doOperator(s: String)(implicit rh : RenderingHandler) {
      rh(s)
   }
   /**
    * called on every delimiter that is rendered through a notation
    * @param p the path of the rendered notation
    * @param d the delimiter
    */
   def doDelimiter(p: GlobalName, d: parser.Delimiter)(implicit rh : RenderingHandler) {
      rh(d.text)
   }
   /**
    * called by various methods to render whitespace
    * @param level how big a space to produce, 0 for no space, higher levels also indicate line-breaking points
    */
   def doSpace(level: Int)(implicit rh : RenderingHandler) {
      Range(0,level).foreach {_ => rh(" ")}
   }
   
   def doBracketedGroup(body: => Unit)(implicit rh : RenderingHandler) {
      doOperator("(")
      body
      doOperator(")")
   }
   def doUnbracketedGroup(body: => Unit)(implicit rh : RenderingHandler) {
      body
   }
   def doOptionallyBracketedGroup(body: => Unit)(implicit rh : RenderingHandler) {
      doUnbracketedGroup(body)
   }
   def doImplicit(body: => Unit)(implicit rh : RenderingHandler) {}
   
   /** auxiliary type for a continuation function */
   type Cont = Unit => Unit
   /** called to render a scripted object - an optional decorated by several optional scripts
    *  
    *  each script is passed as a continuation that must be called at the appropriate place
    *  @param main the object
    *  
    * See [[parser.ScriptMarker]] for the meaning of the scripts
    */
   def doScript(main: => Unit, sup: Option[Cont], sub: Option[Cont], over: Option[Cont], under: Option[Cont])(implicit rh : RenderingHandler) {
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
   
   def doFraction(above: List[Cont], below: List[Cont], line: Boolean)(implicit rh : RenderingHandler) {
      doBracketedGroup {
         above.head()
         above.foreach {
            e => doSpace(1)
            e()
         }
      }
      doOperator("/")
      doBracketedGroup {
         below.foreach {
            e => doSpace(1)
            e()
         }
      }
   }
   
   /**
    * @param o the object to be presented
    * @return an object o' that is presented instead (e.g., o itself)
    *         one position p for each component c of o' such that the c is the p-subobject of o
    *         a notation to use for presenting o'
    */ 
   def getNotation(o: Obj): (Obj, List[Position], Option[TextNotation])
   /**
    * called on objects for which no notation is available
    * @return 1/0/-1 depending on the type of bracketing applied (yes/optional/no)
    */
   def doDefault(o: Obj)(implicit rh : RenderingHandler): Int = o match {
      case OMID(p) =>
         doIdentifier(p)
         -1
      case OMV(n) =>
         doVariable(n)
         -1
      case OMA(f,args) =>
         doBracketedGroup {
            val comps = f::args
            comps.init.foreach {t =>
               apply(t, rh)
               doSpace(1)
            }
            apply(comps.last, rh)
         }
         1
      case OMBINDC(b,c,s) =>
         doBracketedGroup {
            apply(b,rh)
            doSpace(1)
            doOperator("[")
            c.foreach {v =>
               apply(v, rh)
            }
            doOperator("]")
            s.foreach {t =>
               doSpace(1)
               apply(t, rh)
            }
         }
         1
      case l: OMLiteral =>
         doLiteral(l)
         -1
      case OMSemiFormal(parts) => parts.foreach {
         case Formal(t) => apply(t, rh)
         case objects.Text(format, t) => rh(t)
         case XMLNode(n) => rh(n.toString)
      }
      1
      //TODO other cases
      case VarDecl(n,tp,df) =>
         doVariable(n)
         tp foreach {t =>
            doOperator(":")
            recurse(t, noBrackets)
         }
         df foreach {d =>
            doOperator("=")
            recurse(d, noBrackets)
         }
         -1
      case Sub(n,t) =>
         doVariable(n)
         doOperator("=")
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

   /** abbreviation for not bracketing */
   private val noBrackets = (_: TextNotation) => -1
   
   def apply(o: Obj, rh : RenderingHandler) {recurse(o, noBrackets)(rh)}
   /** 
    *  @param bracket called to determine whether a non-atomic term rendered with a certain notation should be bracketed
    *  @return true if the term was bracketed
    */
   private def recurse(obj: Obj, bracket: TextNotation => Int)(implicit rh : RenderingHandler): Int = {
            val (objP, _, notOpt) = getNotation(obj)
            notOpt match {
               case None =>
                  doDefault(objP)
               case Some(not) =>
                  val (args, context, scopes, attributee) = objP match { 
                     // try to render using notation, defaults to doDefault for some errors
                     case ComplexTerm(_, args, context, scopes) => (args, context, scopes, None)
                     case OMID(p) => (Nil,Context(),Nil,None)
                     case _ => return doDefault(objP)
                  }
                  if (! not.arity.canHandle(args.length, context.length, scopes.length, attributee.isDefined))
                     return doDefault(objP)
   
                  /*
                   * @param child the child into which we recurse 
                   * @param currentPosition the position from where we recurse
                   *         -1: left-open argument; 0: middle argument; 1: right-open
                   * @return the result of recursing into the child
                   */
                  def doChild(child: Obj, currentPosition: Int) =
                     recurse(child, (childNot: TextNotation) => Presenter.bracket(not.precedence, currentPosition, childNot))
                  /* processes a list of markers left-to-right
                   *   for ArgumentMarkers, renders argument via doChild
                   *   for DelimiterMarkers, renders delimiter via doDelimiter
                   *   for presentation markers, recurses into groups and arranges them according to doXXX methods
                   */
                  def doMarkers(markers: List[Marker]) {
                     val numDelims = markers.count(_.isInstanceOf[parser.Delimiter])
                     var numDelimsSeen = 0
                     def currentPosition = if (numDelimsSeen == 0) -1 else if (numDelimsSeen == numDelims) 1 else 0
                     // the while loop removes elements from markersLeft until it is empty
                     var markersLeft = markers
                     // the most recently removed marker
                     var previous : Option[parser.Marker] = None
                     while (markersLeft != Nil) {
                        val current = markersLeft.head
                        markersLeft = markersLeft.tail
                        val compFollows = ! markersLeft.isEmpty && markersLeft.head.isInstanceOf[parser.ArgumentMarker]
                        //val delimFollows = ! markersLeft.isEmpty && markersLeft.head.isInstanceOf[parser.Delimiter]
                        current match {
                           case Arg(n) if n > 0 =>
                              doChild(args(n-1), currentPosition)
                              if (compFollows) doSpace(1)
                           case ImplicitArg(n) =>
                              doImplicit {
                                 doChild(args(n-1), currentPosition)
                                 if (compFollows) doSpace(1)
                              }
                           case Arg(n) if n < 0 =>
                              doChild(scopes(-n-args.length-context.length-1), currentPosition)
                              if (compFollows) doSpace(1)
                           case Var(n, typed, _) => //sequence variables impossible due to flattening
                              doChild(context(n-args.length-1), currentPosition)
                              if (compFollows) doSpace(1)
                           case AttributedObject =>
                              // we know attributee.isDefined due to flattening
                              attributee.foreach {a =>
                                 doChild(a, currentPosition)
                              }
                              if (compFollows) doSpace(1)
                           case d: parser.Delimiter =>
                              val letters = d.text.exists(_.isLetter)
                              if (letters && previous.isDefined) doSpace(1)
                              doDelimiter(not.name, d)
                              numDelimsSeen += 1
                              if (letters && !markersLeft.isEmpty) doSpace(1)
                           case s: SeqArg => //impossible due to flattening
                           case GroupMarker(ms) =>
                              doMarkers(ms)
                           case s: ScriptMarker =>
                              def aux(mOpt: Option[Marker]) = mOpt.map {m => (_:Unit) => doMarkers(List(m))} 
                              doScript(doMarkers(List(s.main)), aux(s.sup), aux(s.sub), aux(s.over), aux(s.under))
                           case FractionMarker(a,b,l) =>
                              def aux(m: Marker) = (_:Unit) => doMarkers(List(m)) 
                              doFraction(a map aux, b map aux, l)
                           case InferenceMarker =>
                        }
                        previous = Some(current)
                     }
                  }
                  val br = bracket(not)
                  val flatMarkers = not.arity.flatten(not.presentationMarkers,args.length, context.length, scopes.length, attributee.isDefined)
                  br match {
                     case n if n > 0 => doBracketedGroup { doMarkers(flatMarkers) }
                     case 0 =>          doOptionallyBracketedGroup { doMarkers(flatMarkers) }
                     case n if n < 0 => doUnbracketedGroup { doMarkers(flatMarkers) }
                  }
                  br
            }
   }
}

/** a notation-based presenter using the StructureParser syntax and parsing notations
 * 
 * this class must be initialized after instantiation to set the controller
 */
class StructureAndObjectPresenter extends Presenter with NotationBasedPresenter {
   def isApplicable(format: String) = format == "text/notations"
   def apply(e : StructuralElement, rh : RenderingHandler) {apply(e, 0)(rh)}
   def getNotation(o: Obj) = {
      val (oP, pos, ncOpt) = Presenter.getNotation(controller,o)
      (oP, pos, ncOpt.flatMap(_.getParse))
   }
   
   private def apply(e : StructuralElement, indent: Int)(implicit rh : RenderingHandler) {
      def doIndent {
         Range(0,indent).foreach {_ => rh("   ")}
      }
      doIndent
      e match {
         //TODO delimiters
         case d: Document =>
            rh("document " + d.path.toPath + "\n")
            d.getItems foreach {i => apply(i, indent+1)}
         case r: DRef =>
            rh("document " + r.target.toPath)
         case r: MRef =>
            rh("module " + r.target.toPath)
         case c: Constant =>
            rh("constant " + c.name)
            c.alias foreach {a =>
               rh(" @ ")
               rh(a.toPath)
            }
            c.tp foreach {t =>
               rh("\n")
               doIndent
               rh("  : ")
               apply(t, rh)
            }
            c.df foreach {t =>
               rh("\n")
               doIndent
               rh("  = ")
               apply(t, rh)
            }
            c.notC.oneDim foreach {n =>
               rh("\n")
               doIndent
               rh("  # ")
               rh(n.toText)
            }
            c.notC.twoDim foreach {n =>
               rh("\n")
               doIndent
               rh("  ## ")
               rh(n.toText)
            }
         case t: DeclaredTheory =>
            rh("theory " + t.name + " =\n")
            t.getPrimitiveDeclarations.foreach {d => apply(d, indent+1)}
         case v: DeclaredView =>
            rh("view " + v.name + " : ")
            apply(v.from, rh)
            rh(" -> ")
            apply(v.to, rh)
            rh(" =\n")
            v.getPrimitiveDeclarations.foreach {d => apply(d, indent+1)}
         case s: DeclaredStructure =>
            rh("structure " + s.name + " : " + s.fromPath.toPath + " =\n")
            s.getPrimitiveDeclarations.foreach {d => apply(d, indent+1)}
         case t: DefinedTheory =>
            rh("theory " + t.name + " abbrev ")
            apply(t.df, rh)
         case v: DefinedView =>
            rh("view " + v.name + " abbrev ")
            apply(v.df, rh)
         case s: DefinedStructure =>
            rh("structure " + s.name + " : " + s.fromPath.toPath + " abbrev ")
            apply(s.df, rh)
      }
      rh("\n")
   }
}
