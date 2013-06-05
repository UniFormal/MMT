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
 * presents structural levels according to StructureParser
 *
 * The bracket placement algorithm is only approximate.
 * It will sometimes put too many and sometimes too few brackets.
 * The latter will confuse the NotationBasedParser, but rarely humans.    
 */ 
abstract class StructurePresenter(printDelims: Boolean) extends Presenter {
   def apply(e : StructuralElement, rh : RenderingHandler) = apply(e, 0)(rh)
   
   protected def apply(e : StructuralElement, indent: Int)(implicit rh : RenderingHandler) {
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
         case a: ConstantAssignment =>
            apply(a.toConstant, indent)
         case a: DefLinkAssignment =>
            apply(a.toStructure, indent)
      }
      rh("\n")
   }
}

/** 
 * This trait defines methods for presenting objects using notations.
 * 
 * It is intended to be mixed into Presenters that already define the presentation of the structural levels.
 * 
 * The main methods do not produce any rendering themselves.
 * Instead, they call special methods that may be overridden for customization.
 */
trait NotationBasedPresenter extends Presenter {
   /**
    * called by doDefaultTerm to render symbols
    */
   def doIdentifier(p: ContentPath, rh: RenderingHandler) {
      val s = p match {
         case OMMOD(m) % name => name.toPath  //not parsable if there are name clashes 
         case _ => p.toPath
      }
      rh(s)
   }
   /**
    * called by doDefaultTerm to render variables
    */
   def doVariable(n: LocalName, rh: RenderingHandler) {
      rh(n.toPath)
   }
   /**
    * called by various methods to render MMT-level operators, such as ,:=()
    */
   def doOperator(s: String, rh: RenderingHandler) {
      rh(s)
   }
   /**
    * called on terms for which no notation is available
    */
   def doDefaultTerm(t: Term, rh: RenderingHandler) {t match {
      case OMID(p) => doIdentifier(p, rh)
      case OMV(n) => doVariable(n, rh)
      case OMA(f,args) =>
         doOperator("(", rh)
         val comps = f::args
         comps.init.foreach {t =>
            apply(t, rh)
            doOperator(" ", rh)
         }
         apply(comps.last, rh)
         doOperator(")", rh)
      case OMBINDC(b,c,s) =>
         doOperator("(", rh)
         apply(b,rh)
         doOperator(" [", rh)
         c.foreach {v =>
            apply(v, rh)
         }
         doOperator("]", rh)
         s.foreach {t =>
            doOperator(" ", rh)
            apply(t, rh)
         }
      case l: OMLiteral =>
         rh(l.toString)
      case OMSemiFormal(parts) => parts.foreach {
         case Formal(t) => apply(t, rh)
         case objects.Text(format, t) => rh(t)
         case XMLNode(n) => rh(n.toString)
      }
         
      //TODO other cases
   }}
   /**
    * called on every delimiter that is rendered through a notation
    */
   def doDelimiter(p: GlobalName, d: parser.Delimiter, rh: RenderingHandler) {
      val s = if (d.text.exists(_.isLetter)) " " + d.text + " " else d.text
      rh(s)
   }

   /** abbreviation for not bracketing */
   private val noBrackets = (_: TextNotation) => false
   
   def apply(o: Obj, rh: RenderingHandler) {recurse(o,rh, noBrackets)}

   /** 
    *  @param bracket called to determine whether a non-atomic term rendered with a certain notation should be bracketed
    */
   private def recurse(o: Obj, rh: RenderingHandler, bracket: TextNotation => Boolean) {
       o match {
         case term: Term =>
            val (termP, _, notOpt) = getNotation(term)
            notOpt match {
               case None =>
                  doDefaultTerm(termP, rh)
               case Some(not) =>
                  // try to render using notation, defaults to doDefaultTerm for some errors
                  val ComplexTerm(p, args, context, scopes) = termP
                  if (! not.canHandle(args.length, context.length, scopes.length))
                     return doDefaultTerm(termP, rh)
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
                  def doChild(child: Obj) {recurse(child, rh, childrenMustBracket(currentPosition))}
                  
                  if (br) doOperator("(", rh)
                  markers.foreach {
                     case Arg(n) if n > 0 =>
                        doChild(args(n-1))
                     case Arg(n) if n < 0 =>
                        doChild(scopes(-n-args.length-context.length-1))
                     case Var(n, typed, _) => //sequence variables impossible due to flattening
                        doChild(context(n-args.length-1))
                     case d: parser.Delimiter =>
                        numDelimsSeen += 1
                        doDelimiter(p, d, rh)
                     case s: SeqArg => //impossible due to flattening
                  }
                  if (br) doOperator(")", rh)
            }
         case VarDecl(n,tp,df, _*) =>
               doVariable(n, rh)
               tp foreach {t =>
                  doOperator(":", rh)
                  recurse(t, rh, noBrackets)
               }
               df foreach {d =>
                  doOperator("=", rh)
                  recurse(d, rh, noBrackets)
               }
         case Sub(n,t) =>
               doVariable(n, rh)
               doOperator("=", rh)
               recurse(t, rh, noBrackets)
         case c: Context => c.init.foreach {v =>
               recurse(v, rh, noBrackets)
               doOperator(", ", rh)
            }
            recurse(c.last, rh, noBrackets)  
         case s: Substitution => s.init.foreach {c =>
               recurse(c, rh, noBrackets)
               doOperator(", ", rh)
            }
            recurse(s.last, rh, noBrackets) 
       }
   }
}

/** a notation-based presenter using the StructureParser syntax
 * 
 * this class must be initialized after instantiation to set the controller
 */
class StructureAndObjectPresenter(controller: frontend.Controller) extends StructurePresenter(true) with NotationBasedPresenter {
      def isApplicable(format: String) = format == "text/notations"
}