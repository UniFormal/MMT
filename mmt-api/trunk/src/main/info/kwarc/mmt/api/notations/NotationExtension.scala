package info.kwarc.mmt.api.notations

import info.kwarc.mmt.api._
import objects._
import Conversions._

abstract class Fixity {
   def markers: List[Marker]
   def asString: (String,String)
}
// TODO Delim("%w") does not work because parser does not handle whitespace sequence separator; terms end up being parsed as default applications
case class Mixfix(markers: List[Marker]) extends Fixity {
   def asString = ("mixfix", markers.mkString(" "))
}

abstract class SimpleFixity extends Fixity {
   def impl: Int
   def expl: Int
   def delim: Delimiter
   def simpleArgs = {
      val delimStr = delim match {
         case Delim(s) => List(s)
         case SymbolName(name) => Nil
      }
      (impl :: expl :: delimStr).mkString(" ")
   }
}

case class Prefix(delim: Delimiter, impl: Int, expl: Int) extends SimpleFixity {
   lazy val markers = List(delim, SeqArg(impl+1, Delim("%w")))
   def asString = ("prefix", simpleArgs)
}
case class Infix(delim: Delimiter, impl: Int, expl: Int, assoc: Option[Boolean]) extends SimpleFixity {
   lazy val markers = assoc match {
      case Some(true) =>  List(Arg(impl+1),delim,Arg(impl+2))
      case None =>        List(Arg(impl+1),delim,Arg(impl+2))
      case Some(false) => List(Arg(impl+1),delim,Arg(impl+2))
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
case class Postfix(delim: Delimiter, impl: Int, expl: Int) extends SimpleFixity {
   lazy val markers = List(SeqArg(impl+1, Delim("%w")), delim)
   def asString = ("postfix", simpleArgs)
}
case class Bindfix(delim: Delimiter, impl: Int, expl: Int, assoc: Boolean) extends SimpleFixity {
   def markers = List(delim, Var(impl+1, true, None), Arg(impl+2))
   def asString = {
      val assocString = if (assoc) "-assoc" else ""
      ("bindfix"+assocString, simpleArgs)
   }
}

/**
 * A FixityParser that provides infix, prefix, etc., each with some implicit arguments
 */
object FixityParser {
   private def toInt(s: String) = try {s.toInt} catch {case e: Exception => throw ParseError("number expected, found: " + s)}
   
   /** infix, infix-right, infix-left, prefix, postfix; followed by number of implicit arguments (defaults to 0) */
   def parse(name: GlobalName, fixityString: String, args: List[String]): Fixity = {
      if (fixityString == "mixfix")
         return Mixfix(args.map(Marker.parse(name, _)))
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
      val delim = if (del != "") Delim(del) else SymbolName(name)
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
   def term = ComplexTerm(op, subs, con, args)
}

/** A Fixity is a high-level description of a list of markers that can be used for parsing or presentation
 *  
 *  It is returned by a FixityParser and used in a TextNotation
 */
abstract class NotationExtension {
   def priority: Int
   def isApplicable(m: Option[MPath]): Boolean
   def isApplicable(t: Term): Boolean
   /** called to construct a term after a notation produced by this was used for parsing */
   def constructTerm(op: GlobalName, subs: Substitution, con: Context, args: List[Term], attrib: Boolean)
            (implicit unknown: () => Term): Term
   def constructTerm(fun: Term, args: List[Term]): Term
   /** called to deconstruct a term before presentation */
   def destructTerm(t: Term)(implicit getNotation: GlobalName => Option[TextNotation]): Option[PragmaticTerm]
}

/** the standard mixfix notation for a list of [[Marker]]s */
object MixfixNotation extends NotationExtension {
   def priority = 0
   def isApplicable(m: Option[MPath]) = m.isEmpty
   def isApplicable(t: Term) = true
   def constructTerm(op: GlobalName, subs: Substitution, con: Context, args: List[Term], attrib: Boolean)
      (implicit unknown: () => Term) = ComplexTerm(op, subs, con, args)
   def constructTerm(fun: Term, args: List[Term]) = OMA(fun, args)
   def destructTerm(t: Term)(implicit getNotation: GlobalName => Option[TextNotation]): Option[PragmaticTerm] = t match {
      case ComplexTerm(op, subs, con, args) =>
         getNotation(op) flatMap {not =>
            if (not.arity.canHandle(subs.length, con.length, args.length, false))
               Some(PragmaticTerm(op, subs, con, args, false, not, Position.positions(t)))
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
 */
class HOASNotation(meta: MPath, val hoas: HOAS) extends NotationExtension {
   def priority = 1
   def isApplicable(m: Option[MPath]) = m == Some(meta)
   def isApplicable(t: Term) = t.head match {
      case Some(h) => List(hoas.apply, hoas.typeAtt) contains h
      case None => false
   }
   
   def constructTerm(op: GlobalName, subs: Substitution, con: Context, args: List[Term], attrib: Boolean)
                (implicit unknown: () => Term) : Term = {
      if (attrib) {
         val ptp = if (subs.isEmpty && con.isEmpty && args.isEmpty)
            OMS(op)
         else
            constructTerm(op, subs, con, args, false)
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
              val appTerm = PragmaticTerm(op, Substitution(), Context(), rest, false, not, appPos)
              Some(appTerm)
            } else rest.reverse match {
               case OMBINDC(OMS(hoas.bind), con, args) :: _ =>
                  // last argument is binder
                  val subs = rest.init.map(a => Sub(OMV.anonymous, a))
                  val bindPos = Position(1) :: (0 until con.length+args.length).toList.map(i => Position(rest.length+1) / (i+1))  
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
      }
      case _ => None
   }
}

class NestedHOASNotation(language: MPath, obj: HOAS, meta: HOAS) extends NotationExtension {
   def priority = 2
   def isApplicable(m: Option[MPath]) = m == Some(language)
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
   
   def constructTerm(op: GlobalName, subs: Substitution, con: Context, args: List[Term], attrib: Boolean
                   )(implicit unknown: () => Term) : Term = {
      if (attrib) {
         val ptp = if (subs.isEmpty && con.isEmpty && args.isEmpty)
            OMS(op)
         else
            constructTerm(op, subs, con, args, false)
         meta.apply(OMS(obj.typeAtt), ptp)
      } else {
         // subargs are considered to be meta arguments
         val subargs = subs.map(_.target)
         val opmeta = metaapplication(op, subargs)
         if (con.isEmpty) {
            application(opmeta, args)
         } else if (args.length == 1) {
            application(opmeta, binding(con, args.head))
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
      val (op, metaArgs, objArgs) = unapplication(t).getOrElse(return None)
      val not = getNotation(op).getOrElse(return None)
      val paths = (0 until objArgs.length).toList.map(i => Position((0 until i).toList.map(_ => 4)))
      val objArgPos = paths.reverse.map(p => p / 5)
      val opMetaPath = paths.last / 4
      val opMetaPos = if (metaArgs.isEmpty) List(opMetaPath)
         else (0 until metaArgs.length+1).toList.map(i => opMetaPath / (i+1))
      val arity = not.arity
      if (arity.canHandle(0,0,metaArgs.length+objArgs.length, false)) {
         // List(), List(4), ..., List(4, ..., 4)
         val tP = PragmaticTerm(op, Nil, Nil, metaArgs ::: objArgs, false, not, opMetaPos ::: objArgPos)
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
         } else None
      } else None
   }
}
