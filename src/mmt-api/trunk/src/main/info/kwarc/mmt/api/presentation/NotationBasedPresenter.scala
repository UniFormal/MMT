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
   
   def getNotation(term: Term): (Term, List[Position], Option[TextNotation])
   /**
    * called on terms for which no notation is available
    * @return true if the term was bracketed
    */
   def doDefaultTerm(t: Term)(implicit rh : RenderingHandler): Boolean = {t match {
      case OMID(p) =>
         doIdentifier(p)
         false
      case OMV(n) =>
         doVariable(n)
         false
      case OMA(f,args) =>
         doOperator("(")
         val comps = f::args
         comps.init.foreach {t =>
            apply(t, rh)
            doSpace(1)
         }
         apply(comps.last, rh)
         doOperator(")")
         true
      case OMBINDC(b,c,s) =>
         doOperator("(")
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
         doOperator(")")
         true
      case l: OMLiteral =>
         rh(l.toString)
         false
      case OMSemiFormal(parts) => parts.foreach {
         case Formal(t) => apply(t, rh)
         case objects.Text(format, t) => rh(t)
         case XMLNode(n) => rh(n.toString)
      }
      true
      //TODO other cases
   }}

   /** abbreviation for not bracketing */
   private val noBrackets = (_: TextNotation) => false
   
   def apply(o: Obj, rh : RenderingHandler) {recurse(o, noBrackets)(rh)}
   /** 
    *  @param bracket called to determine whether a non-atomic term rendered with a certain notation should be bracketed
    *  @return true if the term was bracketed
    */
   private def recurse(o: Obj, bracket: TextNotation => Boolean)(implicit rh : RenderingHandler): Boolean = {
       o match {
         case term: Term =>
            val (termP, _, notOpt) = getNotation(term)
            notOpt match {
               case None =>
                  doDefaultTerm(termP)
               case Some(not) =>
                  // try to render using notation, defaults to doDefaultTerm for some errors
                  val ComplexTerm(p, args, context, scopes) = termP
                  if (! not.canHandle(args.length, context.length, scopes.length))
                     return doDefaultTerm(termP)
                  val br = bracket(not)
   
                  /*
                   * @param position the position into which we recurse
                   * @return a function that determines whether the child has to be bracketed
                   */
                  def childrenMustBracket(position: Int) =
                     (childNot: TextNotation) => Presenter.bracket(not.precedence, position, childNot) > 0
                  val (markers,_) = not.flatten(args.length, context.length, scopes.length)
                  // currentPostion: -1: left-open argument; 0: middle argument; 1: right-open
                  val numDelims = markers.count(_.isInstanceOf[parser.Delimiter])
                  var numDelimsSeen = 0
                  def currentPosition = if (numDelimsSeen == 0) -1 else if (numDelimsSeen == numDelims) 1 else 0
                  def doChild(child: Obj) = recurse(child, childrenMustBracket(currentPosition))
                  // the while loop removes elements from markersLeft until it is empty
                  var markersLeft = markers
                  // the most recently removed marker
                  var previous : Option[parser.Marker] = None
                  if (br) doOperator("(")
                  while (markersLeft != Nil) {
                     val current = markersLeft.head
                     markersLeft = markersLeft.tail
                     val compFollows = ! markersLeft.isEmpty && markersLeft.head.isInstanceOf[parser.ArityComponent]
                     val delimFollows = ! markersLeft.isEmpty && markersLeft.head.isInstanceOf[parser.Delimiter]
                     current match {
                        case Arg(n) if n > 0 =>
                           doChild(args(n-1))
                           if (compFollows) doSpace(1)
                        case Arg(n) if n < 0 =>
                           doChild(scopes(-n-args.length-context.length-1))
                           if (compFollows) doSpace(1)
                        case Var(n, typed, _) => //sequence variables impossible due to flattening
                           doChild(context(n-args.length-1))
                           if (compFollows) doSpace(1)
                        case d: parser.Delimiter =>
                           val letters = d.text.exists(_.isLetter)
                           if (letters && previous.isDefined) doSpace(1)
                           doDelimiter(p, d)
                           numDelimsSeen += 1
                           if (letters && !markersLeft.isEmpty) doSpace(1)
                        case s: SeqArg => //impossible due to flattening
                     }
                     previous = Some(current)
                  }
                  if (br) doOperator(")")
                  br
            }
         case VarDecl(n,tp,df, _*) =>
            doVariable(n)
            tp foreach {t =>
               doOperator(":")
               recurse(t, noBrackets)
            }
            df foreach {d =>
               doOperator("=")
               recurse(d, noBrackets)
            }
            false
         case Sub(n,t) =>
            doVariable(n)
            doOperator("=")
            recurse(t, noBrackets)
            false
         case c: Context =>
            if (! c.isEmpty) {
               c.init.foreach {v =>
                  recurse(v, noBrackets)
                  doOperator(", ")
               }
               recurse(c.last, noBrackets)
            }
            false
         case s: Substitution =>
            if (! s.isEmpty) {
               s.init.foreach {c =>
                  recurse(c, noBrackets)
                  doOperator(", ")
               }
               recurse(s.last, noBrackets)
            }
            false
       }
   }
}

/** a notation-based presenter using the StructureParser syntax
 * 
 * this class must be initialized after instantiation to set the controller
 */
class StructureAndObjectPresenter(controller: frontend.Controller) extends Presenter with NotationBasedPresenter {
   def isApplicable(format: String) = format == "text/notations"
   def apply(e : StructuralElement, rh : RenderingHandler) {apply(e, 0)(rh)}
   
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
            c.not foreach {n =>
               rh("\n")
               doIndent
               rh("  # ")
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
