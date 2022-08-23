package info.kwarc.mmt.api.presentation

import info.kwarc.mmt.api._
import symbols._
import patterns._
import objects._
import documents._
import info.kwarc.mmt.api.utils.XMLEscaping
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
      source: Option[SourceRef], pos : Position, globalContext: Context, context : List[VarData], style: Option[PresentationContext => String]) {
   /** the output stream to print into */
   def out(s: String): Unit = {rh(s)}
   /** convenience method to change the position field */
   def child(i: Int) = copy(pos = pos / i)
   /** convenience method to change the position field */
   def child(p: Position) = copy(pos = pos / p)
   /** the MMT context of the presented object */
   def getContext: Context = {
      globalContext ++ context.map(_.decl)
   }
   /** convenience method to append to the context */
   def addCon(con: List[VarData]) = copy(context = context ::: con)
   /** for convenient HTML output */
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

  def apply(o: Obj, origin: Option[CPath])(implicit rh : RenderingHandler) = {
      implicit val pc = preparePresentation(o, origin)
      doToplevel(o) {
         recurse(o)
      }
  }

  /** called once at the beginning of each presentation, override as needed */
  protected def preparePresentation(o: Obj, origin: Option[CPath])(implicit rh : RenderingHandler) = {
      origin.map(_.parent).foreach {
        case p: ContentPath => if (p.module != p) controller.simplifier(p.module)
        case _ =>
      }
      val globalCont = origin match {
        case None => Context.empty
        case Some(cp) => controller.getO(cp.parent) match {
           case None =>
             Context.empty
           case Some(se) =>
             val c1 = controller.getContext(se)
             val c2 = se.getComponentContext(cp.component)
             c1 ++ c2
        }
      }
      PresentationContext(rh, origin, Nil, None, Position.Init, globalCont, Nil, None)
  }

   /**
    * called once at the toplevel of every object to be rendered
    */
   def doToplevel(o: Obj)(body: => Unit)(implicit pc: PresentationContext): Unit = {
      body
   }

   /**
    * called by doDefaultTerm to render symbols
    *
    * names are given in human-oriented form and not parsable if there are name clashes
    */
   def doIdentifier(p: ContentPath)(implicit pc: PresentationContext): Unit = {
     def getThO(m: MPath): Option[Theory] = controller.getO(m) match {case Some(m: Theory) => Some(m) case _ => None}
     def getProperIncludes(m: MPath) = getThO(m).map {th => th.getAllIncludes.map(_.from).filter(Some(_) != th.meta)} getOrElse Nil
     def declaresTwice(ms: List[MPath], name: LocalName) = (ms.filter(getThO(_).map(_.declares(name)) getOrElse false).length > 1)
     val nameOnly = pc.owner match {
       case Some(CPath(gn: GlobalName, _: TermComponentKey)) =>
         //if (gn.module == p.module) {true} else {
           !(declaresTwice(getProperIncludes(gn.module), p.name))
         //}
       case _ => true
     }
     val s = p match {
         case GlobalName(_, name) => if(nameOnly) {name.toPath} else {"☞"+p.toString}
         case MPath(_, name) => "?" + name.toPath
      }
      pc.out(s)
   }
   /**
    * called by doDefaultTerm to render variables
    */
   def doVariable(n: LocalName)(implicit pc: PresentationContext): Unit = {
      pc.out(n.toPath)
   }
   /**
    * called by doDefaultTerm to render literals
    */
   def doLiteral(l: OMLITTrait)(implicit pc: PresentationContext): Unit = {
      lazy val default = l.toString
      val lS = l match {
        case l: OMLIT => l.rt.lexerExtension match {
          case Some(le) => le.unapply(l)
          case None => default
        }
        case l: UnknownOMLIT => default
      }
      pc.out(lS)
   }
   /**
    * called by various methods to render MMT-level operators, such as ,:=()
    */
   def doOperator(s: String)(implicit pc: PresentationContext): Unit = {
      pc.out(s)
   }
   /**
    * called on every delimiter that is rendered through the notation of a symbol
    * @param p the path of the rendered notation
    * @param d the delimiter
    * @param implicits implicit arguments of the rendered term that are not explicitly placed by the notation (added to first delimiter)
    */
   def doDelimiter(p: GlobalName, d: Delimiter, implicits: List[Cont])(implicit pc: PresentationContext): Unit = {
      pc.out(d.text)
   }
   /**
    * called on every delimiter that is rendered through the notation of a variable
    * @param n the variable name
    * @param d the delimiter
    */
   def doDelimiter(n: LocalName, d: Delimiter)(implicit pc: PresentationContext): Unit = {
      pc.out(d.text)
   }
   /**
    * called by various methods to render whitespace
    * @param level how big a space to produce, 0 for no space, higher levels also indicate line-breaking points
    */
   def doSpace(level: Int)(implicit pc: PresentationContext): Unit = {
      Range(0,level).foreach {_ => pc.out(" ")}
   }

   /** default treatment of complex terms */
   def doComplex(op: GlobalName, subs: Substitution, con: Context, args: List[Term])(implicit pc: PresentationContext): Unit = {
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
                if (i < con.length-1) {
                  doOperator(",")
                  doSpace(1)
                }
             }
             doOperator("]")
          }
          args.zipWithIndex.foreach {case (t,i) =>
             doSpace(1)
             recurse(t)(pc.child(subs.length+con.length+i+1).addCon(vardata))
          }
       }
   }

   /**
    * called to wrap around subexpressions that must be bracketed
    * @param body the part between the brackets
    */
   def doBracketedGroup(body: => Unit)(implicit pc: PresentationContext): Unit = {
      doOperator("(")
      body
      doOperator(")")
   }
   /**
    * called to wrap around subexpressions that are not bracketed
    * @param body the part between the brackets
    */
   def doUnbracketedGroup(body: => Unit)(implicit pc: PresentationContext): Unit = {
      body
   }
   /**
    * called to wrap around subexpressions that could but do not have to be bracketed
    * @param body the part between the brackets
    */
   def doOptionallyBracketedGroup(body: => Unit)(implicit pc: PresentationContext): Unit = {
      doUnbracketedGroup(body)
   }
   /**
    * called to wrap around subexpressions that are implicit arguments
    * @param body the argument
    */
   def doImplicit(body: => Unit)(implicit pc: PresentationContext): Unit = {}
   /**
    * called to wrap around inferred types of bound variables
    * @param body the argument
    */
   def doInferredType(body: => Unit)(implicit pc: PresentationContext): Unit = {}

   //TODO imlement this better
   def doAttributedTerm(t : Term, k : OMID, v : Term)(implicit pc : PresentationContext) = recurse(t)

   /** auxiliary type for a continuation function */
   type Cont = () => Unit
   /** called to render a scripted object - an optional decorated by several optional scripts
    *
    *  each script is passed as a continuation that must be called at the appropriate place
    *  @param main the object
    *
    * See [[notations.ScriptMarker]] for the meaning of the scripts
    */
   def doScript(main: => Unit, sup: Option[Cont], sub: Option[Cont], over: Option[Cont], under: Option[Cont])(implicit pc: PresentationContext): Unit = {
      def aux(sOpt: Option[Cont], oper: String): Unit = {sOpt match {
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

   /** auxiliary function for inserting a separator (such as whitespace) into a list */
   def doListWithSeparator(l: List[Cont], sep: Cont): Unit = {
      if (l.isEmpty) return
      l.head()
      l.tail.foreach {e =>
         sep()
         e()
      }
   }
   def doListWithSpace(l: List[Cont], n: Int = 1)(implicit pc: PresentationContext) =
      doListWithSeparator(l, () => doSpace(n))

   def doFraction(above: List[Cont], below: List[Cont], line: Boolean)(implicit pc: PresentationContext): Unit = {
      doBracketedGroup {
         doListWithSpace(above)
      }
      doOperator("/")
      doBracketedGroup {
         doListWithSpace(below)
      }
   }

   def doTd(ms : List[Cont])(implicit pc : PresentationContext): Unit = {
     doOperator("[&")
      ms foreach {e =>
       doSpace(1)
       e()
     }
     doOperator("&]")
   }

   def doTr(ms : List[Cont])(implicit pc : PresentationContext): Unit = {
     doOperator("[\\")
      ms foreach {e =>
       doSpace(1)
       e()
     }
     doOperator("\\]")
   }

   def doTable(ms : List[Cont])(implicit pc : PresentationContext): Unit = {
     doOperator("[[")
      ms foreach {e =>
       doSpace(1)
       e()
     }
     doOperator("]]")
   }
      //helper method only
   def doSqrt(args : List[Cont])(implicit pc: PresentationContext) = if (args.nonEmpty) {
     doOperator("√")
     doBracketedGroup {
       args.head
       args.tail.foreach {e =>
         doSpace(1)
         e()
       }
     }
   } else {}

   def doRootMarker(base : List[Cont], root : List[Cont])(implicit pc: PresentationContext): Unit ={
     if(root != Nil){
       doOperator("'")
       doBracketedGroup{
         base.head
         base.tail foreach { e =>
           doSpace(1)
           e()
         }
       }
     }
     doSqrt(root)
   }

   def doNumberMarker(arg : Delim)(implicit pc: PresentationContext): Unit = {
     doOperator("#num_" + arg.s)
   }

   def doIdenMarker(arg : Delim)(implicit pc: PresentationContext): Unit = {
     doOperator("#id_" + arg.s)
   }

   def doErrorMarker(args: List[Cont])(implicit pc: PresentationContext): Unit ={
      doOperator("#err_")
      doBracketedGroup {
        args.head
        args.tail.foreach {e =>
          doSpace(1)
          e()
        }
      }
   }

   def doPhantomMarker(args: List[Cont])(implicit pc: PresentationContext): Unit ={
     doOperator("//*")
     doBracketedGroup {
     args.head
     args.tail.foreach {e =>
       doSpace(1)
       e()
     }
   }
     doOperator("*//")
   }

   def doTextMarker(text : Delim)(implicit pc: PresentationContext): Unit ={
     doOperator("/*" + text.s + "*/")
   }

   def doGlyphMarker(src: Delim, alt: String="Failed to Load")(implicit pc: PresentationContext): Unit ={
     doOperator("#glyph_"+src.s)
   }

   def doLabelMarker(args: List[Cont], label : String ) (implicit pc: PresentationContext): Unit ={
    doBracketedGroup {
      args.head
      args.tail.foreach {e=>
        doSpace(1)
        e()
      }
    }
    doOperator( ".label(" +label+ ")" )
   }

   def doWord(s : String)(implicit pc: PresentationContext): Unit = {
     pc.out(s)
   }



   /** 1 or 2-dimensional notations, true by default */
   def twoDimensional : Boolean = true

   /**
    * @param o the object to be presented
    * @return an object o' that is presented instead (e.g., o itself)
    *         one position p for each component c of o' such that the c is the p-subobject of o
    *         a notation to use for presenting o'
    */
   implicit protected def getNotations(p: GlobalName): List[TextNotation] = {
      Presenter.getNotations(controller, p, twoDimensional)
   }
   protected def getAlias(p: GlobalName): List[LocalName] = {
     controller.globalLookup.getO(p) match {
       case Some(d: Declaration) => d.alternativeNames
       case _ => Nil
     }
   }
   /**
    * called on objects for which no notation is available
    * @return 1/0/-1 depending on the type of bracketing applied (yes/optional/no)
    */
   // TODO can this be a default notation
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
        doComplex(op, subs, con, args)
        1
      case OMA(f,args) =>
         // only applies in unusual cases where f is not atomic
         doBracketedGroup {
            recurse(f)(pc.child(0))
            if (args.isEmpty) {
              doOperator("()")
            } else args.zipWithIndex.foreach {case (t,i) =>
               doSpace(1)
               recurse(t)(pc.child(i+1))
            }
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
               if (i < c.length-1) {
                 doOperator(",")
                 doSpace(1)
               }
            }
            doOperator("]")
            s.zipWithIndex.foreach {case (t,i) =>
               doSpace(1)
               recurse(t)(pc.child(c.length+i+1).addCon(vardata))
            }
         }
         1
      case OMATTR(t,k,v) =>
         doAttributedTerm(t, k, v)(pc)
         1
      case OMSemiFormal(parts) => parts.foreach {
            case Formal(t) => recurse(t)
            case objects.Text(format, t) => pc.out(t)
            case XMLNode(n) => pc.out(n.toString)
         }
         1
      case o:OML =>
         doDefault(o.vd)
      case VarDecl(n,f,tp,df, not) =>
         f.foreach {f => doOperator(f); doSpace(1)}
         val include = f contains IncludeVarDecl.feature
         if (!include)
           doVariable(n)
         tp foreach {t =>
            val doIt = () => {
              if (!include) doOperator(":")
              recurse(t, noBrackets)(pc.child(0))
            }
            if (metadata.TagInferredType.get(o)) {
               doInferredType {doIt()}
            } else {
              doIt()
            }
         }
         df foreach {d =>
            val index = if (tp.isDefined) 1 else 0
            doOperator("=")
            recurse(d, noBrackets)(pc.child(index))
         }
         not foreach {n =>
            doOperator("#")
            doOperator(n.toText) //TODO make nicer
         }
         -1
      case Sub(n,t) =>
         if (n != OMV.anonymous) {
            doVariable(n)
            doOperator("=")
         }
         recurse(t, noBrackets)(pc.child(0))
         -1
      case c: Context =>
         if (! c.isEmpty) {
            val declInd = c.zipWithIndex
            declInd.init.foreach {case (v,i) =>
               recurse(v, noBrackets)(pc.child(i))
               doOperator(",")
               doSpace(1)
            }
            val (last, n) = declInd.last
            recurse(last, noBrackets)(pc.child(n))
         }
         -1
      case s: Substitution =>
         if (! s.isEmpty) {
            val declInd = s.zipWithIndex
            declInd.init.foreach {case (s,i) =>
               recurse(s, noBrackets)(pc.child(i))
               doOperator(",")
               doSpace(1)
            }
            val (last, n) = declInd.last
            recurse(last, noBrackets)(pc.child(n))
         }
         -1
      case f@OMFOREIGN(_) => {
        pc.out("<mtext>" + XMLEscaping.apply(f.toString) + "</mtext>")
        -1
      }
   }

   /** abbreviation for not bracketing */
   private val noBrackets = (_: TextNotation) => -1
   protected def recurse(obj: Obj)(implicit pc: PresentationContext): Int = recurse(obj, noBrackets)(pc)
   def doPresentationMarker(m : PresentationMarker, doMarkers : List[Marker] => Unit)(implicit pc: PresentationContext) : Unit = m match {
     case GroupMarker(ms) =>
       doUnbracketedGroup { doMarkers(ms) }
     case s: ScriptMarker =>
       def aux(mOpt: Option[Marker]) = mOpt.map {m => () => doMarkers(List(m))}
       doScript(doMarkers(List(s.main)), aux(s.sup), aux(s.sub), aux(s.over), aux(s.under))
     case FractionMarker(a,b,l) =>
       def aux(m: Marker) = () => doMarkers(List(m))
       doFraction(a map aux, b map aux, l)
     case NumberMarker(value) =>
       doNumberMarker(value)
     case IdenMarker(value) =>
       doIdenMarker(value)
     case ErrorMarker(markers)=>
       def aux(m: Marker) = () => doMarkers(List(m))
       doErrorMarker(markers map aux)
     case GlyphMarker(source,alt) =>
       doGlyphMarker(source,alt)
     case LabelMarker(markers,label) =>
       def aux(m: Marker) = () => doMarkers(List(m))
       doLabelMarker(markers map aux,label)
     case PhantomMarker(markers) =>
       def aux(m: Marker) = () => doMarkers(List(m))
       doPhantomMarker(markers map aux)
     case TextMarker(text) =>
       doTextMarker(text)
     case RootMarker(base, root) =>
       def aux(m: Marker) = () => doMarkers(List(m))
       doRootMarker(base map aux,root map aux)
     case TdMarker(ms) =>
       def aux(m: Marker) = () => doMarkers(List(m))
       doTd(ms map aux)
     case TrMarker(ms) =>
       def aux(m: Marker) = () => doMarkers(List(m))
       doTr(ms map aux)
     case TableMarker(ms) =>
       def aux(m: Marker) = () => doMarkers(List(m))
       doTable(ms map aux)
     case _ => doMarkers(List(m))
   }

   /**
    *  @param bracket called to determine whether a non-atomic term rendered with a certain notation should be bracketed
    *  @return 1 if the term was bracketed
    */
   protected def recurse(obj: Obj, bracket: TextNotation => Int)(implicit pcOrg: PresentationContext): Int = {
         val pc = pcOrg.copy(source = SourceRef.get(obj))
         // recovery if no notation or other problem
         lazy val default = doDefault(obj)(pc)
         obj match {
            case OMS(p) =>
               val not = getNotations(p).find(_.arity.isConstant).getOrElse(return default)
               if (not.arity.isConstant) {
                 def doMarkers(ms : List[Marker]) : Unit = ms match {
                   case Nil => //nothing to do
                   case hd :: tl => hd match {
                     case d: Delimiter =>
                        val dE = d.expand(p, getAlias(p))
                        doDelimiter(p, dE, Nil)
                     case p : PresentationMarker => doPresentationMarker(p, doMarkers)
                     case _ => ImplementationError("missing case in presenter of OMS")
                   }
                   doMarkers(tl)
                 }
                 doMarkers(not.presentationMarkers)
                 -1
               } else return default
            case OMV(n) =>
               val not = pc.context.find(_.name == n).getOrElse(return default).decl.not.getOrElse(return default)
               if (not.arity.isConstant) {
                  not.markers.foreach {
                     case d: Delimiter =>
                        doDelimiter(n, d)
                     case _ => ImplementationError("missing case in presenter of OMV")
                  }
                 -1
               } else return default
            case t @ ComplexTerm(_,_,_,_) =>
              controller.pragmatic.makePragmatic(t) match {
               case None =>
                  return default
               case Some(objP) =>
                  val PragmaticTerm(op, subargs, context, args, not, pos) = objP
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
                        case a: ArgumentComponent =>
                           if (a.number < firstVarNumber) Nil else context
                        case Var(n,_,_,_) => context.take(n-firstVarNumber)
                        case _ => Nil
                     }
                     val newVarData = newCont.zipWithIndex map {case (v,i) =>
                        VarData(v, Some(op), pc.pos / pos(firstVarNumber+i))
                     }
                     recurse(child, brack)(pc.child(pos(ac.number)).addCon(newVarData))
                  }
                  // all implicit arguments that are not placed by the notation, they are added to the first delimiter
                  val unplacedImplicits = not.arity.flatImplicitArguments(args.length).filter(i => ! not.fixity.markers.contains(i))
                  var unplacedImplicitsDone = false
                  /* processes a list of markers left-to-right
                   *   for ArgumentMarkers, renders argument via doChild
                   *   for DelimiterMarkers, renders delimiter via doDelimiter
                   *   for presentation markers, recurses into groups and arranges them according to doX methods
                   */
                  def doMarkers(markers: List[Marker]): Unit = {
                     val numDelims = markers count countsAsDelim
                     var numDelimsSeen = 0
                     def currentPosition = {
                       if (numDelimsSeen == 0) -1
                       else if (numDelimsSeen == numDelims) 1
                       else 0
                     }
                     // the while loop removes elements from markersLeft until it is empty
                     var markersLeft = markers
                     // the most recently removed marker
                     var previous : Option[Marker] = None
                     while (markersLeft != Nil) {
                        val current = markersLeft.head
                        markersLeft = markersLeft.tail
                        if (op.name.toString.contains("make"))
                           true
                        val compFollows = ! markersLeft.isEmpty && markersLeft.head.isInstanceOf[ArgumentMarker]
                        //val delimFollows = ! markersLeft.isEmpty && markersLeft.head.isInstanceOf[parser.Delimiter]
                        current match {
                           case c: Arg =>
                              val child = if (c.number < firstArgNumber) subargs(c.number-1) else args(c.number-firstArgNumber)
                              doChild(c, child, currentPosition)
                              if (compFollows) doSpace(1)
                           case c @ ImplicitArg(n,_) =>
                              val child = if (n < firstArgNumber) subargs(n-1) else args(n-firstArgNumber)
                              doImplicit {
                                 doChild(c, child, currentPosition)
                                 if (compFollows) doSpace(1)
                              }
                           case c @ Var(n, typed, _,_) => //sequence variables impossible due to flattening
                              doChild(c, context(n-firstVarNumber), currentPosition)
                              if (compFollows) doSpace(1)
                           case d: Delimiter =>
                              val dE = d.expand(op, getAlias(op))
                              val unpImps = if (unplacedImplicitsDone) Nil else unplacedImplicits map {
                                 case c @ ImplicitArg(n,_) =>
                                     () => doChild(c, args(n-firstArgNumber), 0); ()
                              }
                              val letters = dE.text.exists(_.isLetter)
                              if (letters && previous.isDefined && !previous.get.isInstanceOf[Delimiter]) doSpace(1)
                              doDelimiter(op, dE, unpImps)(pc.copy(pos = pc.pos / pos(0)))
                              if (letters && !markersLeft.isEmpty && !markersLeft.head.isInstanceOf[Delimiter]) doSpace(1)
                           case s: SeqArg => //impossible due to flattening
                           case GroupMarker(ms) =>
                              doUnbracketedGroup { doMarkers(ms) }
                           case s: ScriptMarker =>
                              def aux(mOpt: Option[Marker]) = mOpt.map {m => () => doMarkers(List(m))}
                              doScript(doMarkers(List(s.main)), aux(s.sup), aux(s.sub), aux(s.over), aux(s.under))
                           case FractionMarker(a,b,l) =>
                              def aux(m: Marker) = () => doMarkers(List(m))
                              doFraction(a map aux, b map aux, l)
                           case TdMarker(ms) =>
                              def aux(m: Marker) = () => doMarkers(List(m))
                              doTd(ms map aux)
                           case TrMarker(ms) =>
                              def aux(m: Marker) = () => doMarkers(List(m))
                              doTr(ms map aux)
                           case TableMarker(ms) =>
                              def aux(m: Marker) = () => doMarkers(List(m))
                              doTable(ms map aux)
                           case NumberMarker(value) =>
                              doNumberMarker(value)
                           case IdenMarker(value) =>
                              doIdenMarker(value)
                           case ErrorMarker(markers)=>
                              def aux(m: Marker) = () => doMarkers(List(m))
                              doErrorMarker(markers map aux)
                           case GlyphMarker(source,alt) =>
                              doGlyphMarker(source,alt)
                           case LabelMarker(markers,label) =>
                              def aux(m: Marker) = () => doMarkers(List(m))
                              doLabelMarker(markers map aux,label)
                           case PhantomMarker(markers) =>
                              def aux(m: Marker) = () => doMarkers(List(m))
                              doPhantomMarker(markers map aux)
                           case TextMarker(text) =>
                              doTextMarker(text)
                           case RootMarker(base, root) =>
                              def aux(m: Marker) = () => doMarkers(List(m))
                              doRootMarker(base map aux,root map aux)
                           case WordMarker(m) => doWord(m)
                           case InferenceMarker =>
                              checking.Solver.infer(controller, pc.getContext, t, None) match {
                                 case Some(tp) =>
                                    //TODO change owner? (currently needed to get the theory into the context)
                                    recurse(tp, _ => -1)(pc.copy(source = None))
                                 case None =>
                                    doOperator("?")
                              }
                        }
                        if (countsAsDelim(current)) {
                          numDelimsSeen += 1
                        }
                        previous = Some(current)
                     }
                  }
                  val br = bracket(not)
                  val flatMarkers = not.arity.flatten(not.presentationMarkers, subargs.length, context.length, args.length)
                  br match {
                     case n if n > 0 => doBracketedGroup { doMarkers(flatMarkers) }
                     case 0 =>          doOptionallyBracketedGroup { doMarkers(flatMarkers) }
                     case n if n < 0 => doUnbracketedGroup { doMarkers(flatMarkers) }
                  }
                  br
            }
            //so that subclasses can override these methods to add special behavior for supported attribute keys
            case OMATTR(t, k, v) =>
              doAttributedTerm(t, k, v)(pc)
            case t =>
              return default
         }
   }
   /** auxiliary function of recurse: whether a marker acts as a delimiter for the purposes of bracket elimination */
   private def countsAsDelim(m: Marker): Boolean = m match {
     case d: Delimiter => true
     case s: ScriptMarker => countsAsDelim(s.main)
     case f: FractionMarker => f.line
     case _ => false
   }
}
