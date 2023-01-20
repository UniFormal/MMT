package info.kwarc.mmt.api.notations

import info.kwarc.mmt.api._
import utils._
import objects._
import Conversions._

/**
 * a Fixity is used by a [[TextNotation]] to arrange arguments and delimiters
 *
 * A Fixity is a high-level construct that elaborates into a list of [[Marker]]s. Parser and present only use the latter.
 */
abstract class Fixity {
   /** the elaboration into markers */
   def markers: List[Marker]
   /** true if this notation can be used for multiple constants without ambiguity because it contains placeholders */
   def isRelative = markers exists {
       case _: PlaceholderDelimiter => true
       case _ => false
   }
   /** wrap first delimiter in instance name */
   def relativize = {
     if (isRelative)
       Some(this)
     else {
       var done = false
       val markersR = markers map {
         case d: Delim if !done =>
           done = true
           InstanceName(d)
         case m => m
       }
       if (done) Some(Mixfix(markersR))
       else None
     }
   }
   /** the string representation to use when serializing notations
    *  pair of "fixity type" and type-specific argument(s)
    */
   def asString: (String,String)
   
   def addInitialImplicits(n: Int): Fixity
}

/**
 *  the default Fixity, which is directly a list of markers
 */
case class Mixfix(markers: List[Marker]) extends Fixity {
   def asString: (String, String) = ("mixfix", markers.mkString(" "))
   def addInitialImplicits(n: Int): Mixfix = {
     val markersM = markers.map {
       case d: Delimiter => d
       case a: Arg => a * {_ + n}
       case a: ImplicitArg => a * {_ + n}
       case v: Var => v.copy(number = v.number + 1)
       // TODO other cases
       case other => throw ImplementationError("undefined case of marker " + other.toString)
     }
     Mixfix(markersM)
   }

  /**
    * Consider a `Constant` c with notation `not: Mixfix`.
    * Suppose we remove the arguments specified in `removedArgs` from the type of `c`.
    * Then you can use this function to suitably transform `not`.
    *
    * @example Suppose we had `pair: {A: tp, B: tp} term -> term -> term # <3, 4>` as the constant.
    *          Assume we want to remove the first two unnecessary arguments, i.e. specify
    *          `removedArgs = Set(0,1)`.
    *          Suppose we then transform the type to `term -> term ->term` by some means (e.g. the diagram operator
    *          by Navid).
    *          To transform the notation, we call this function, i.e. `removeArguments(removedArgs)`,
    *          to ultimately get the new Mixfix notation `<1, 2>`.
    *
    * @param removedArgs One-based (!!) set of argument positions to be removed.
    *                    Note that argument positions in MMT surface syntax are also
    *                    usually given one-based.
    *
    * @return The transformed notation if possible. Otherwise, e.g., if some explicitly listed
    *         argument marker depended on a to-be-removed argument position, [[None]] is returned.
    *         (Rationale: even though we could just remove those argument markers, too, we cannot possibly
    *          determine whether the resulting notation makes sense typographically.)
    */
  def removeArguments(removedArgs: Set[Int]): Option[Mixfix] = {
    if (removedArgs.isEmpty) {
      return Some(this)
    }

    val markersM = markers.flatMap {
      case d: Delimiter => Some(d)

      case a: Arg if removedArgs.contains(a.number) => return None
      // throw away markers for implicit arguments positions that are now removed
      case a: ImplicitArg if removedArgs.contains(a.number) => None

      case a: Arg =>
        val decrement = removedArgs.count(_ < a.number)
        Some(a * {_ - decrement})

      case a: ImplicitArg =>
        val decrement = removedArgs.count(_ < a.number)
        Some(a * {_ - decrement})

      // TODO other cases
      case other => throw ImplementationError("undefined case of marker " + other.toString)
    }
    Some(Mixfix(markersM))
  }
}

object Circumfix extends Fixity {
  def apply(leftDelim: Delim, rightDelim: Delim, argNum: Int, implArgInds: List[Int]) = {
    val explArgMarkers = (0 until argNum).filterNot(implArgInds.contains(_)).toList.map(i => SimpArg(i+1))
    val implArgMarkers = implArgInds.map(i=>ImplicitArg(i+1))
    Mixfix(leftDelim :: implArgMarkers:::explArgMarkers.+:(rightDelim))
  }

  def apply(leftDelim: Delim, rightDelim: Delim, argNum: Int, impl: Int = 0) =
    Mixfix(leftDelim :: (1 until impl+1) .map(ImplicitArg(_)).toList:::(impl+1 until argNum+1) .map (SimpArg(_)).toList.+:(rightDelim))
  override def asString: (String, String) = this.asInstanceOf[Mixfix].asString
  override def markers = this.asInstanceOf[Mixfix].markers
  override def addInitialImplicits(n: Int) = this.asInstanceOf[Mixfix].addInitialImplicits(n)
  def unapply(fix: Fixity) = fix match {
    case mix: Mixfix if (mix.markers.length >= 3) => mix.markers match {
      case Nil => None
      case Delim(lDelim)::tl => (tl.init, tl.last) match {
        case (args: List[SimpArg@unchecked], Delim(rDelim)) => Some((lDelim, rDelim, args.length))
        case _ => None
      }
      case _ => None
    }
    case _ => None
  }
}

// TODO this should not exists as an object and as a case class
// used for Mizar
object PrePostfix extends Fixity {
  def apply(delim: Delimiter, prefixedArgsNum: Int, numArgs: Int, rightArgsBracketed: Boolean, implArgInds: List[Int]) : Mixfix = {
    val bracketed = rightArgsBracketed && (numArgs - prefixedArgsNum - implArgInds.length > 0)
    val explArgMarkers = (0 until numArgs).filterNot(implArgInds.contains(_)).toList.map(i => SimpArg(i+1))
    val implArgMarkers = implArgInds.map(i=>ImplicitArg(i+1))
    val (infixedArgMarkers, suffixedArgsMarkers) = explArgMarkers.splitAt(prefixedArgsNum)
    val suffMarkers: List[Marker] = if (bracketed) {
      Delim("(") :: suffixedArgsMarkers.+:(Delim(")"))
    } else {
      suffixedArgsMarkers
    }
    val markers = implArgMarkers:::infixedArgMarkers ::: delim :: suffMarkers
    Mixfix(markers)
  }
  def apply(delim: Delimiter, prefixedArgsNum: Int, expl: Int, rightArgsBracketed: Boolean = false, impl: Int = 0) : Mixfix = {
    val bracketed = rightArgsBracketed && (expl - prefixedArgsNum > 0)
    val argMarkers = (1 until impl+1).map (ImplicitArg(_)).toList ::: (impl+1 until impl+expl+1).map (SimpArg(_)).toList
    val (infixedArgMarkers, suffixedArgsMarkers) = argMarkers.splitAt(prefixedArgsNum)
    val suffMarkers: List[Marker] = if (bracketed) {
      Delim("(") :: suffixedArgsMarkers.+:(Delim(")"))
    } else {
      suffixedArgsMarkers
    }
    val markers = infixedArgMarkers ::: delim :: suffMarkers
    Mixfix(markers)
  }
  override def asString: (String, String) = this.asInstanceOf[Mixfix].asString
  override def markers = this.asInstanceOf[Mixfix].markers
  override def addInitialImplicits(n: Int) = this.asInstanceOf[Mixfix].addInitialImplicits(n)
  def unapply(fix: Fixity) = fix match {
    case Mixfix(markers) =>
      val (impls, expls) = markers.span(_.isInstanceOf[ImplicitArg])
      val (prefixes, remainingMarkers) = expls.span(_.isInstanceOf[SimpArg])
      val (delim, suffixesP) = (remainingMarkers.head, remainingMarkers.tail)
      val rightArgsBracketed = if (suffixesP.length >= 2) {
        (suffixesP.head == Delim("(") && suffixesP.last == Delim(")"))
      } else {
        false
      }
      val suffixes = if (rightArgsBracketed) {
        suffixesP.tail.init
      } else {
        suffixesP
      }
      ((delim, suffixes) match {
        case (Delim(del), sufs: List[SimpArg@unchecked]) => Some((delim, prefixes.length, prefixes.length + sufs.length, rightArgsBracketed, impls.length))
        case _ => None
      })
    case _ => None
  }
}
/**
 * A SimpleFixity is one out of multiple typical fixities (infix, postfix, etc) characterized by using only a single delimiter.
 *
 * impl and expl do not have to agree with the number of arguments demanded by the type system.
 *  * Notation has more arguments than function type: Notation extensions may handle the extra arguments.
 *    Example: equal : {a:tp} tm (a => a => bool), impl = 1, expl = 2
 *  * Notation has less arguments than function type: Operator returns a function.
 *    Example: union : {a:tp} tm (a set => a set => a set) where a set = a => bool, impl = 1, expl = 2
 */
abstract class SimpleFixity extends Fixity {
   /** number of initial implicit arguments (inferred by parser, skipped by printer) */
   def impl: Int
   /** expl number of subsequent explicit arguments (needed to trigger notation during parsing, rendered by printer) */
   def expl: Int
   /** the delimiter to use */
   def delim: Delimiter
   protected def implArgs = (0 until impl).toList.map(i => ImplicitArg(i+1))
   /** ImplicitArgs Args1 Delim Args2 with Args1.length == beforeOp */
   protected def argsWithOp(beforeOp: Int) =
      (0 until beforeOp).toList.map(i => SimpArg(1+impl+i)) ::: delim ::
      (beforeOp until expl).toList.map(i => SimpArg(1+impl+i))
   protected def simpleArgs = {
      val delimStr = delim match {
         case SymbolName() => Nil
         case _ => List(delim.text)
      }
      (impl :: expl :: delimStr).mkString(" ")
   }
}

/** delimiter after a certain argument */
case class PrePostfix(delim: Delimiter, leftArgs: Int, expl: Int, rightArgsBracketed: Boolean = false, impl: Int = 0)  extends SimpleFixity {
  lazy val markers = if (expl != 0) {
    val leftArgMarkers = (impl until impl+leftArgs).map(i=>SimpArg(i+1)).toList
    val rightArgsMarkers = (impl+leftArgs until impl+expl).map(i=>SimpArg(i+1)).toList
    val suffMarkers: List[Marker] = if (rightArgsBracketed) {Delim("(")::rightArgsMarkers.+:(Delim(")"))} else {rightArgsMarkers}
    leftArgMarkers++(delim::suffMarkers)
  } else argsWithOp(leftArgs) ::: implArgs
  def asString = ("mixfix", simpleArgs)
  def addInitialImplicits(n: Int) = copy(impl = impl+n)
}

/** delimiter followed by the (explicit) arguments */
case class Prefix(delim: Delimiter, impl: Int, expl: Int) extends SimpleFixity {
   lazy val markers = if (expl != 0) argsWithOp(0) else argsWithOp(0) ::: implArgs
   def asString = ("prefix", simpleArgs)
   def addInitialImplicits(n: Int) = copy(impl = impl+n) 
}
/**
 * delimiter after the first (explicit) argument
 *
 * @param assoc None/Some(true)/Some(false) for none, left, right; currently ignored
 */
case class Infix(delim: Delimiter, impl: Int, expl: Int, assoc: Option[Boolean]) extends SimpleFixity {
   lazy val markers = assoc match {
      case Some(true) =>  argsWithOp(1)
      case None =>        argsWithOp(1)
      case Some(false) => argsWithOp(1) // TODO use Delim(_,false)
   }
   def asString = {
      val assocString = assoc match {
         case Some(true) => "-left"
         case None => ""
         case Some(false) => "-right"
      }
      ("infix"+assocString, simpleArgs)
   }
   def addInitialImplicits(n: Int) = copy(impl = impl+n)
}

/** delimiter after the (explicit) arguments */
case class Postfix(delim: Delimiter, impl: Int, expl: Int) extends SimpleFixity {
   lazy val markers = argsWithOp(expl)
   def asString = ("postfix", simpleArgs)
   def addInitialImplicits(n: Int) = copy(impl = impl+n)
}

/** delimiter followed by first and second (explicit) argument with . in between
 *
 * @param assoc merge with nested bindings using the same binder; currently ignored
 *
 * assumes arguments are one variable and one scope; expl must be 1
 */
case class Bindfix(delim: Delimiter, impl: Int, expl: Int, assoc: Boolean) extends SimpleFixity {
   def markers = List(delim, Var(impl+1, true, None), Delim("."), SimpArg(impl+2))
   def asString = {
      val assocString = if (assoc) "-assoc" else ""
      ("bindfix"+assocString, simpleArgs)
   }
   def addInitialImplicits(n: Int) = copy(impl = impl+n)
}

/**
 * parses MixFix and SimpleFixity
 */
object FixityParser {
   private def toInt(s: String) = try {s.toInt} catch {case e: Exception => throw ParseError("number expected, found: " + s)}

   /** infix, infix-right, infix-left, prefix, postfix; followed by number of implicit arguments (defaults to 0)
    *
    *  @param fixityString one of prefix | postfix | infix | infix-left | infix-right | bindfix | bindfix-assoc
    *  @param args
    *   * for mixfix: the list of markers separated by whitespace
    *   * else: impl | delim | impl expl | impl delim | impl expl delim
    *     where impl and expl are natural numbers, delim anything else
    */
   def parse(fixityString: String, args: List[String]): Fixity = {
      if (fixityString == "mixfix")
         return Mixfix(args.map(Marker.parse(_)))
      val (impl, expl, del) = args match {
         case i::e::s:: Nil => (toInt(i), toInt(e), s)
         case n::es:: Nil =>
            val nP = toInt(n)
            try {(nP, toInt(es), "")}
            catch {case _:ParseError => (0, nP, es)}
         case es:: Nil =>
            try {(0, toInt(es), "")}
            catch {case _:ParseError => (0, 0, es)}
         case _ => (0,0,"")
      }
      val delim = if (del != "") Delim(del) else SymbolName()
      fixityString match {
         case "infix"       => Infix(delim, impl, expl, None)
         case "infix-left"  => Infix(delim, impl, expl, Some(true))
         case "infix-right" => Infix(delim, impl, expl, Some(false))
         case "prefix"      => Prefix(delim, impl, expl)
         case "postfix"     => Postfix(delim, impl, expl)
         case "bindfix"     => Bindfix(delim, impl, expl, false)
         case "bindfix-assoc" => Bindfix(delim, impl, expl, true)
      }
   }
}

case class PragmaticTerm(op: GlobalName, subs: Substitution, con: Context, args: List[Term], notation: TextNotation, pos: List[Position]) {
   require(1 + subs.length + con.length + args.length == pos.length, "Positions don't match number of arguments (op, subs, context and args)")
   def term = ComplexTerm(op, subs, con, args)
}

/** A Fixity is a high-level description of a list of markers that can be used for parsing or presentation
 *
 *  It is returned by a FixityParser and used in a TextNotation
 */
abstract class NotationExtension extends Rule {
   /** true if this can be used to destruct a term */
   def isApplicable(t: Term): Boolean
   /** called to construct a term after a notation produced by this was used for parsing */
   def constructTerm(op: GlobalName, subs: Substitution, con: Context, args: List[Term], not: TextNotation)
            (implicit unknown: () => Term): Term
   def constructTerm(fun: Term, args: List[Term]): Term
   /** called to deconstruct a term before presentation */
   def destructTerm(t: Term)(implicit getNotations: GlobalName => List[TextNotation]): Option[PragmaticTerm]
}

/** the standard mixfix notation for a list of [[Marker]]s */
object MixfixNotation extends NotationExtension {
   def isApplicable(t: Term) = true
   def constructTerm(op: GlobalName, subs: Substitution, con: Context, args: List[Term], not: TextNotation)
      (implicit unknown: () => Term) = ComplexTerm(op, subs, con, args)
   def constructTerm(fun: Term, args: List[Term]) = OMA(fun, args)
   def destructTerm(t: Term)(implicit getNotations: GlobalName => List[TextNotation]): Option[PragmaticTerm] = t match {
      case ComplexTerm(op, subs, con, args) =>
        getNotations(op).foreach {not =>
            if (not.canHandle(subs.length, con.length, args.length)) {
              return Some(PragmaticTerm(op, subs, con, args, not, Position.positions(t)))
            }
        }
        return None
      case _ => None
   }
}

/**
 * convenience apply/unapply for a HOAS application operator
 * unapply recurses to uncurry
 */
class HOASApplySpine(app: GlobalName) {
  val applyT = OMS(app)
  def apply(f: Term, args: List[Term]) = OMA(this.applyT, f :: args)
  def unapply(t: Term) : Option[(Term,List[Term], List[Position])] = t match {
    case OMA(this.applyT, f :: a) =>
       val fPos = Position(1)
       val aPos = (1 until 1+a.length).toList.map(i => Position(1+i))
       unapply(f) match {
         case None =>
           Some((f, a, fPos :: aPos))
         case Some((g, b, q)) =>
           val gbPos = q map {x => fPos / x} // nested terms are found as subterms of f
           Some((g, b:::a, gbPos ::: aPos))
       }
    case _ => None
  }  
}

/** tuple of HOAS symbol names */
case class HOAS(apply: GlobalName, bind: GlobalName) {
  val applySpine = new HOASApplySpine(apply)
}

/**
 * OMA(apply, op, args)) <--> OMA(op, args)
 *
 * OMA(apply, op, OMBIND(bind, context, args)) <--> OMBIND(op, context, args)
 *
 * x: OMA(typeAtt, tp) <--> x: tp
 *
 * assumption: HOAS notations do not have arguments before context
 */
class HOASNotation(val hoas: HOAS) extends NotationExtension {
   override def priority = 1
   def isApplicable(t: Term) = t.head match {
      case Some(h) => h == hoas.apply
      case None => false
   }

   def constructTerm(op: GlobalName, subs: Substitution, con: Context, args: List[Term], not: TextNotation)(implicit unknown: () => Term) : Term = {
         // for now: strict form treats substitution as extra arguments
         val subargs = subs.map(_.target)
         if (con.isEmpty)
           hoas.apply(OMS(op) :: subargs ::: args)
         else {
           hoas.apply(OMS(op) :: subargs ::: List(hoas.bind(con,args)))
         }
   }
   def constructTerm(fun: Term, args: List[Term]) = hoas.apply(fun::args)

   def destructTerm(t: Term)(implicit getNotations: GlobalName => List[TextNotation]): Option[PragmaticTerm] = t match {
      case hoas.applySpine(OMS(op), rest, appPos) =>
         val notations = getNotations(op)
         MyList(notations) mapFind {not =>
             if (not.canHandle(0,0,rest.length)) {
               // OMA(apply, op, args)  <-->  OMA(op, args)
               val appTerm = PragmaticTerm(op, Substitution.empty, Context.empty, rest, not, appPos)
               Some(appTerm)
             } else rest.reverse match {
               case OMBINDC(OMS(hoas.bind), con, args) :: _ =>
                  // OMA(apply, op, subs, OMBIND(bind, con, args))  <-->  OMBIND(op@subs, con, args)
                  val subs = rest.init.map(a => Sub(OMV.anonymous, a))
                  val opSubsPos = (0 until 1+subs.length).toList.map(i => Position(1+i))
                  val conArgsPos = (0 until con.length+args.length).toList.map(i => Position(rest.length+1) / (i+1))
                  val bindPos = opSubsPos ::: conArgsPos
                  if (not.canHandle(subs.length, con.length, args.length)) {
                     val bindTerm = PragmaticTerm(op, subs, con, args, not, bindPos)
                     Some(bindTerm)
                  } else
                    None
               case _ => None
             }
         }
      case _ => None
   }
}

/**
 * Church-style higher-order abstract (obj) syntax within LF-style higher-order abstract syntax (meta),
 *
 * e.g.,
 *  apply: tm A=>B -> tm B -> tm B
 *  lam  : (tm A -> tm B) -> tm A=>B
 *
 * assumption: notations give meta-arguments as arguments before context
 */
class NestedHOASNotation(val obj: HOAS, val meta: HOAS) extends NotationExtension {
   override def priority = 2
   def isApplicable(t: Term) = t match {
      case OMA(OMS(meta.apply), OMS(obj.apply) :: _) => true
      case _ => false
   }

   private def metaapplication(op: GlobalName, args: List[Term])(implicit unknown: () => Term) : Term =
      if (args.isEmpty) OMS(op) else OMA(OMS(meta.apply), OMS(op) :: args)

   private def application(f: Term, a: Term)(implicit unknown: () => Term) : Term =
      meta.apply(OMS(obj.apply), unknown(), unknown(), f, a)

   private def application(f: Term, args: List[Term])(implicit unknown: () => Term) : Term =
      args.foldLeft(f) {case (sofar, next) => application(sofar, next)}

   private def binding(vd: VarDecl, scope: Term)(implicit unknown: () => Term): Term =
      meta.apply(OMS(obj.bind), unknown(), unknown(), meta.bind(vd, List(scope)))

   private def binding(con: Context, scope: Term)(implicit unknown: () => Term): Term =
      con.foldRight(scope) {case (next, sofar) => binding(next, sofar)}

   def constructTerm(op: GlobalName, subs: Substitution, con: Context, args: List[Term], not: TextNotation)(implicit unknown: () => Term) : Term = {
         // 2 components are considered to be meta-arguments
         // - the subargs
         // - if there are no variables, the leading implicit args
         // the remaining args are considered object arguments
         val numLeadingImplArgs = if (con.isEmpty) {
            not.arity.arguments.takeWhile(_.isInstanceOf[ImplicitArg]).length
         } else
            0
         val metaargs = subs.map(_.target) ::: args.take(numLeadingImplArgs)
         val objArgs = args.drop(numLeadingImplArgs)
         val opmeta = metaapplication(op, metaargs)
         if (con.isEmpty) {
            application(opmeta, objArgs)
         } else if (objArgs.length == 1) {
            application(opmeta, binding(con, objArgs.head))
         } else throw InvalidNotation("")
   }
   def constructTerm(fun: Term, args: List[Term]) = meta.apply(fun::args)

   private def unapplication(t: Term) : Option[(GlobalName, List[Term], List[Term])] = t match {
      case OMA(OMS(meta.apply), OMS(obj.apply) :: List(_, _, f, a)) => unapplication(f).map {
         case (op, metaArgs, objArgs) => (op, metaArgs, objArgs ::: List(a))
      }
      case OMA(OMS(meta.apply), OMS(op) :: args) =>
         Some((op, args, Nil))
      case OMS(op) => Some((op, Nil, Nil))
      case _ => None
   }
   private def unbinding(t: Term): (Context, Term) = t match {
      case OMA(OMS(meta.apply), OMS(obj.bind) :: List(_,_, OMBIND(OMS(meta.bind), Context(vd), s))) => unbinding(s) match {
         case (con, scope) => (con ++ vd, scope)
      }
      case t => (Context(), t)
   }

   def destructTerm(t: Term)(implicit getNotations: GlobalName => List[TextNotation]): Option[PragmaticTerm] = {
      val (op, metaArgs, objArgs) = unapplication(t).getOrElse(return None)
      getNotations(op).foldLeft[Option[PragmaticTerm]](None) {
        (res,not) => if (res.isDefined) res else {
          val paths = (0 until objArgs.length).toList.map(i => Position((0 until i).toList.map(_ => 4)))
          val objArgPos = paths.reverse.map(p => p / 5)
          val opMetaPath = paths.lastOption.getOrElse(Position.Init) / 4
          val opMetaPos = if (metaArgs.isEmpty) List(opMetaPath)
             else (0 until metaArgs.length+1).toList.map(i => opMetaPath / (i+1))
          val arity = not.arity
          val numLeadingImplArgs = if (arity.variables.isEmpty)
                arity.arguments.takeWhile(_.isInstanceOf[ImplicitArg]).length
             else
                0
          val numSubArgs = metaArgs.length - numLeadingImplArgs
          val subargs = metaArgs.take(numSubArgs)
          val args = metaArgs.drop(numSubArgs) ::: objArgs
          if (not.canHandle(subargs.length,0,args.length)) {
             // List(), List(4), ..., List(4, ..., 4)
             val tP = PragmaticTerm(op, subargs.map(Sub(OMV.anonymous, _)), Nil, args, not, opMetaPos ::: objArgPos)
             Some(tP)
          } else if (objArgs.length == 1) {
             val (con, scope) = unbinding(objArgs.last)
             if (not.canHandle(metaArgs.length, con.length, 1)) {
                // List(), List(4,2), ..., List(4,2,...,4,2)
                val conPaths = (0 until con.length).toList.map(i => (0 until i).toList.flatMap(_ => List(4,2)))
                val conPos = conPaths.map(p => Position(5) / p / 4 / 1)
                val scopePos = Position(5) / conPaths.last / 4 / 2
                val tP = PragmaticTerm(op, metaArgs.map(Sub(OMV.anonymous, _)), con, List(scope), not, opMetaPos ::: conPos ::: List(scopePos))
                Some(tP)
             } else
                None
          } else None
        }
      }
   }
}
