package info.kwarc.mmt.api.notations

import info.kwarc.mmt.api._
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
   /** the string representation to use when serializing notations
    *  pair of "fixity type" and type-specific argument(s) 
    */
   def asString: (String,String)
}

/** 
 *  the default Fixity, which is directly a list of markers
 */
case class Mixfix(markers: List[Marker]) extends Fixity {
   def asString = ("mixfix", markers.mkString(" "))
}

/**
 * A SimpleFixity is one out of multiple typical fixities (infix, postfix, etc) characterized by using only a single delimiter.
 * 
 * impl and expl do not have to agree with the number of arguments demanded by the type system.
 *  * Notation has more arguments than function type: Notation extensions may handle the extra arguments.
 *    Example: equal : {a:tp} tm (a => a => bool), impl = 1, expl = 2
 *  * Notation has less arguments than function type: Operator return a function.
 *    Example: union : {a:tp} tm (a set => a set => a set) where a set = a => bool, impl = 1, expl = 2
 */
abstract class SimpleFixity extends Fixity {
   /** number number of initial implicit arguments (inferred by parser, skipped by printer) */
   def impl: Int
   /** expl number of subsequent explicit arguments (needed to trigger notation during parsing, rendered by printer) */
   def expl: Int
   /** the delimiter to use */
   def delim: Delimiter
   protected def implArgs = (0 until impl).toList.map(i => ImplicitArg(i+1))
   /** ImplicitArgs Args1 Delim Args2 with Args1.length == beforeOp */
   protected def argsWithOp(beforeOp: Int) =
      (0 until beforeOp).toList.map(i => Arg(1+impl+i)) ::: delim ::
      (beforeOp until expl).toList.map(i => Arg(1+impl+i))
   protected def simpleArgs = {
      val delimStr = delim match {
         case SymbolName() => Nil
         case _ => List(delim.text)
      }
      (impl :: expl :: delimStr).mkString(" ")
   }
}

/** delimiter followed by the (explicit) arguments */
case class Prefix(delim: Delimiter, impl: Int, expl: Int) extends SimpleFixity {
   lazy val markers = if (expl != 0) argsWithOp(0) else argsWithOp(0) ::: implArgs
   def asString = ("prefix", simpleArgs)
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
      case Some(false) => argsWithOp(1)
   }
   def asString = {
      val assocString = assoc match {
         case Some(true) => "-left"
         case None => ""
         case Some(false) => "-right"
      }
      ("infix"+assocString, simpleArgs)
   }
}

/** delimiter after the (explicit) arguments */ 
case class Postfix(delim: Delimiter, impl: Int, expl: Int) extends SimpleFixity {
   lazy val markers = argsWithOp(expl)
   def asString = ("postfix", simpleArgs)
}

/** delimiter followed by first and second (explicit) argument with . in between
 *  
 * @param assoc merge with nested bindings using the same binder; currently ignored
 * 
 * assumes arguments are one variable and one scope; expl must be 1
 */
case class Bindfix(delim: Delimiter, impl: Int, expl: Int, assoc: Boolean) extends SimpleFixity {
   def markers = List(delim, Var(impl+1, true, None), Delim("."), Arg(impl+2))
   def asString = {
      val assocString = if (assoc) "-assoc" else ""
      ("bindfix"+assocString, simpleArgs)
   }
}

/**
 * parses MixFix and SimpleFixity
 * 
 * 
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

case class PragmaticTerm(op: GlobalName, subs: Substitution, con: Context, args: List[Term], attribution: Boolean, notation: TextNotation, pos: List[Position]) {
   require(1 + subs.length + con.length + args.length == pos.length, "Positions don't match number of arguments (op, subs, context and args)") 
   def term = ComplexTerm(op, subs, con, args)
}

/** A Fixity is a high-level description of a list of markers that can be used for parsing or presentation
 *  
 *  It is returned by a FixityParser and used in a TextNotation
 */
abstract class NotationExtension {
   def priority: Int
   /** this can strictify notations for this meta-level */
   def applicableLevel: Option[MPath]
   def isApplicable(t: Term): Boolean
   /** called to construct a term after a notation produced by this was used for parsing */
   def constructTerm(op: GlobalName, subs: Substitution, con: Context, args: List[Term], attrib: Boolean, not: TextNotation)
            (implicit unknown: () => Term): Term
   def constructTerm(fun: Term, args: List[Term]): Term
   /** called to deconstruct a term before presentation */
   def destructTerm(t: Term)(implicit getNotation: GlobalName => Option[TextNotation]): Option[PragmaticTerm]
}

/** the standard mixfix notation for a list of [[Marker]]s */
object MixfixNotation extends NotationExtension {
   def priority = 0
   def applicableLevel = None
   def isApplicable(t: Term) = true
   def constructTerm(op: GlobalName, subs: Substitution, con: Context, args: List[Term], attrib: Boolean, not: TextNotation)
      (implicit unknown: () => Term) = ComplexTerm(op, subs, con, args)
   def constructTerm(fun: Term, args: List[Term]) = OMA(fun, args)
   def destructTerm(t: Term)(implicit getNotation: GlobalName => Option[TextNotation]): Option[PragmaticTerm] = t match {
      case ComplexTerm(op, subs, con, args) =>
         getNotation(op) flatMap {not =>
            if (not.arity.canHandle(subs.length, con.length, args.length, false)) {
              Some(PragmaticTerm(op, subs, con, args, false, not, Position.positions(t)))
            }
            else None
         }
      case _ => None
   }
}

case class HOAS(apply: GlobalName, bind: GlobalName, typeAtt: GlobalName)

/**
 * OMA(apply, op, args)) <--> OMA(op, args)
 * 
 * OMA(apply, op, OMBIND(bind, context, args)) <--> OMBIND(op, context, args)
 * 
 * x: OMA(typeAtt, tp) <--> x: tp
 * 
 * assumption: HOAS notations do not have arguments before context
 */
class HOASNotation(val language: MPath, val hoas: HOAS) extends NotationExtension {
   def priority = 1
   def applicableLevel = Some(language)
   def isApplicable(t: Term) = t.head match {
      case Some(h) => List(hoas.apply, hoas.typeAtt) contains h
      case None => false
   }
   
   def constructTerm(op: GlobalName, subs: Substitution, con: Context, args: List[Term], attrib: Boolean, not: TextNotation)
                (implicit unknown: () => Term) : Term = {
      if (attrib) {
         val ptp = if (subs.isEmpty && con.isEmpty && args.isEmpty)
            OMS(op)
         else
            constructTerm(op, subs, con, args, false, not)
         hoas.typeAtt(ptp)
      } else {
         // for now: strict form treats substitution as extra arguments
         val subargs = subs.map(_.target)
         if (con.isEmpty)
            hoas.apply(OMS(op) :: subargs ::: args)
        else {
          hoas.apply(OMS(op) :: subargs ::: List(hoas.bind(con, args)))
        }
      }
   }
   def constructTerm(fun: Term, args: List[Term]) = hoas.apply(fun::args)
   
   def destructTerm(t: Term)(implicit getNotation: GlobalName => Option[TextNotation]): Option[PragmaticTerm] = t match {
      case OMA(OMS(hoas.apply), OMS(op)::rest) =>
         val appPos = (0 until 1+rest.length).toList.map(i => Position(1+i))
         getNotation(op) flatMap {not =>
            if (not.arity.canHandle(0,0,rest.length, false)) {
              // OMA(apply, op, args)  <-->  OMA(op, args)
              val appTerm = PragmaticTerm(op, Substitution(), Context(), rest, false, not, appPos)
              Some(appTerm)
            } else rest.reverse match {
               case OMBINDC(OMS(hoas.bind), con, args) :: _ =>
                  // OMA(apply, op, subs, OMBIND(bind, con, args))  <-->  OMBIND(op@subs, con, args)
                  val subs = rest.init.map(a => Sub(OMV.anonymous, a))
                  val opSubsPos = (0 until 1+subs.length).toList.map(i => Position(1+i))
                  val conArgsPos = (0 until con.length+args.length).toList.map(i => Position(rest.length+1) / (i+1))
                  val bindPos = opSubsPos ::: conArgsPos 
                  if (not.arity.canHandle(subs.length, con.length, args.length, false)) {
                     val bindTerm = PragmaticTerm(op, subs, con, args, false, not, bindPos) 
                     Some(bindTerm)
                  } else
                     None
               case _ => None
            }
         }
      case OMA(OMS(hoas.typeAtt), List(tp)) => tp match {
         case OMS(op) =>
            Some(PragmaticTerm(op, Substitution(), Context(), Nil, true, null, List(Position(1))))
         case OMA(OMS(hoas.apply), OMS(op)::rest) =>
            Some(PragmaticTerm(op, Substitution(), Context(), rest, true, null, (0 until rest.length+1).toList.map(i => Position(1) / (i+1))))
         case _ => None
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
class NestedHOASNotation(language: MPath, obj: HOAS, meta: HOAS) extends NotationExtension {
   def priority = 2
   def applicableLevel = Some(language)
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
   
   def constructTerm(op: GlobalName, subs: Substitution, con: Context, args: List[Term], attrib: Boolean, not: TextNotation
                   )(implicit unknown: () => Term) : Term = {
      if (attrib) {
         val ptp = if (subs.isEmpty && con.isEmpty && args.isEmpty)
            OMS(op)
         else
            constructTerm(op, subs, con, args, false, not)
         meta.apply(OMS(obj.typeAtt), ptp)
      } else {
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
   
   def destructTerm(t: Term)(implicit getNotation: GlobalName => Option[TextNotation]): Option[PragmaticTerm] = {
      if (t.toString == "(LF?apply Kernel?Comb Kernel?bool Kernel?bool (LF?apply Kernel?Comb Kernel?bool (LF?apply Kernel?fun Kernel?bool Kernel?bool) (LF?apply Kernel?equal Kernel?bool) (LF?apply Kernel?Comb Kernel?bool Kernel?bool (LF?apply Kernel?Comb Kernel?bool (LF?apply Kernel?fun Kernel?bool Kernel?bool) bool?/\\ p) q)) p)")
         true
      val (op, metaArgs, objArgs) = unapplication(t).getOrElse(return None)
      val not = getNotation(op).getOrElse(return None)
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
      if (arity.canHandle(subargs.length,0,args.length, false)) {
         // List(), List(4), ..., List(4, ..., 4)
         val tP = PragmaticTerm(op, subargs.map(Sub(OMV.anonymous, _)), Nil, args, false, not, opMetaPos ::: objArgPos)
         Some(tP)
      } else if (objArgs.length == 1) {
         val (con, scope) = unbinding(objArgs.last)
         if (arity.canHandle(metaArgs.length, con.length, 1, false)) {
            // List(), List(4,2), ..., List(4,2,...,4,2)
            val conPaths = (0 until con.length).toList.map(i => (0 until i).toList.flatMap(_ => List(4,2)))
            val conPos = conPaths.map(p => Position(5) / p / 4 / 1)
            val scopePos = Position(5) / conPaths.last / 4 / 2
            val tP = PragmaticTerm(op, metaArgs.map(Sub(OMV.anonymous, _)), con, List(scope), false,
                                   not, opMetaPos ::: conPos ::: List(scopePos))
            Some(tP)
         } else
            None
      } else None
   }
}
