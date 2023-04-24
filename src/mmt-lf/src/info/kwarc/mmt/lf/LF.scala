package info.kwarc.mmt.lf

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects.Conversions._
import info.kwarc.mmt.api.objects._

/* Meta-variable names
   s, t: Terms
   A, B: Types
   K, L: kinds
   G: contexts
*/

/* Operators:
   v % A  : a context in which v : OMV has type A : Term
   G ++ G': appending G' to G
   G(name): look up type of OMV(name) in G
   v / s  : the substitution that substitutes v : OMV  with s : Term
   s ^ S  : application of the substitution S to s : Term
*/

case class LFError(msg: String) extends java.lang.Exception(msg)

object LF {
  val _base: DPath = Typed._base
  /** path of the theory that declares the symbols of LF */
  val _path: MPath = _base ? "LambdaPi"
  /** path of the theory LF */
  val theoryPath: MPath = _base ? "LF"

  def constant(name: String) = OMS(_path ? name)

  lazy val hoas = notations.HOAS(Apply.path, Lambda.path)
}

class LFSym(name: String) {
  val path: GlobalName = LF._path ? name
  val term = OMS(path)
}

/** provides apply/unapply methods for lambda abstraction
  *
  * for example, it permits constructing and pattern-matching terms as Lambda(variable-name, type, scope)
  *
  * unapply curries automatically
  */
object Lambda extends LFSym("lambda") {
  def apply(name: LocalName, tp: Term, body: Term) = OMBIND(this.term, OMV(name) % tp, body)
  def apply(con: Context, body: Term) = OMBIND(this.term, con, body)

  // like apply, but skips the abstraction for empty context
  def applyOrBody(con: Context, body: Term): Term = if (con.isEmpty) body else apply(con, body)

  def unapply(t: Term): Option[(LocalName, Term, Term)] = t match {
    case OMBIND(OMS(this.path), Context(VarDecl(n, None, Some(a), None, _), rest@_*), s) =>
      val newScope = if (rest.isEmpty)
        s
      else
        apply(Context(rest: _*), s)
      Some(n, a, newScope)
    case _ => None
  }

  /** [x:tp] x */
  def identity(tp: Term): Term = Lambda.apply(LocalName("x"), tp, OMV("x"))
}

/** provides apply/unapply methods for dependent function type formation
  *
  * the unapply method also matches a simple function type
  *
  * unapply curries automatically
  */
object Pi extends LFSym("Pi") {
  def apply(name: LocalName, tp: Term, body: Term) = OMBIND(this.term, OMV(name) % tp, body)

  def apply(con: Context, body: Term) = OMBIND(this.term, con, body)

  // like apply, but skips the abstraction for empty context
  def applyOrBody(con: Context, body: Term): Term = if (con.isEmpty) body else apply(con, body)

  def unapply(t: Term): Option[(LocalName, Term, Term)] = t match {
    case OMBIND(OMS(this.path), Context(VarDecl(n, None, Some(a), None, _), rest@_*), s) =>
      val newScope = if (rest.isEmpty)
        s
      else
        apply(Context(rest: _*), s)
      Some(n, a, newScope)
    case OMA(Arrow.term, args) if args.length >= 2 =>
      val name = OMV.anonymous
      if (args.length > 2)
        Some((name, args.head, OMA(Arrow.term, args.tail)))
      else
        Some((name, args.head, args(1)))
    case _ => None
  }
}

/** provides apply/unapply methods for simple function type formation
  * the unapply method does not match a dependent function type, even if the variable does not occur
  */
object Arrow extends LFSym("arrow") {
  def apply(t1: Term, t2: Term) = OMA(this.term, List(t1, t2))

  def apply(in: List[Term], out: Term): Term = if (in.isEmpty) out else OMA(this.term, in ::: List(out))

  def unapply(t: Term): Option[(Term, Term)] = t match {
    case OMA(this.term, hd :: tl) if tl.nonEmpty => Some((hd, apply(tl.init, tl.last)))
    case _ => None
  }
}

/** provides apply/unapply methods for application of a term to a single argument
  * the unapply method transparently handles associativity (currying) of application
  */
object Apply extends LFSym("apply") {
  def apply(f: Term, a: Term) = OMA(this.term, List(f, a))

  def applyCurried(f: Term, as: List[Term]): Term = as match {
    case Nil => f
    case hd::tl => applyCurried(Apply(f,hd), tl)
  }

  def unapply(t: Term): Option[(Term, Term)] = t match {
    case OMA(this.term, f :: a) =>
      if (a.length > 1) Some((OMA(this.term, f :: a.init), a.last))
      else if (a.length == 1) Some((f, a.head))
      else None
    case _ => None
  }
}

/** provides apply/unapply methods for application of a term to a list of arguments
  * the unapply method transparently handles associativity (currying) of application
  */
object ApplySpine {
  /** applies to a sequence of arguments (does not introduce currying) */
  def apply(f: Term, a: Term*): Term = OMA(Apply.term, f :: a.toList)

  def unapply(t: Term): Option[(Term, List[Term])] = t match {
    case OMA(Apply.term, f :: args) =>
      unapply(f) match {
        case None => Some((f, args))
        case Some((c, args0)) => Some((c, args0 ::: args))
      }
    case _ => None
  }
}

/**
  * Helper object with apply/unapply for matching and constructing LF function types.
  *
  * The methods
  * - automatically curry and uncurry.
  * - include the case n=0, in particular, unapply always matches!
  */
object FunType {
  def apply(in: List[(Option[LocalName], Term)], out: Term): Term = {
    in.foldRight(out)({
      case ((Some(x), t), sofar) => Pi(x, t, sofar)
      case ((None, t), sofar) => Arrow(t, sofar)
    })
  }

  def unapply(t: Term): Option[(List[(Option[LocalName], Term)], Term)] = t match {
    case Pi(name, tp, bd) =>
      val nm = name match {
        case OMV.anonymous => None
        case x => Some(x)
      }
      val (remainingArgs, ultimateScope) = unapply(bd).get //always returns non-None
      Some((nm, tp) :: remainingArgs, ultimateScope)
    case Arrow(t1, t2) =>
      val (remainingArgs, ultimateScope) = unapply(t2).get //always returns non-None
      Some((None, t1) :: remainingArgs, ultimateScope)
    case OMA(Arrow.term, List(a)) => Some((Nil, a))
    case t => Some(Nil, t)
  }

  /** only defined for Pi-like contexts */
  def apply(in: Context, t: Term): Term = apply(contextAsArgs(in), t)

  /** inverse of contextAsArgs but only for Pi-like contexts */
  def contextAsArgs(c: Context): List[(Option[LocalName],Term)] = c.getDeclarations.map {
    case VarDecl(n, None, Some(t), None, _) => if (n == OMV.anonymous) (None,t) else (Some(n),t)
  }

  def argsAsContext(args: List[(Option[LocalName], Term)]): Context = args.map {
    case (Some(n), t) => VarDecl(n, t)
    case (None, t) => VarDecl(OMV.anonymous, t)
  }
}

/** Like FunType but builds lambdas */
object FunTerm {
  def apply(in: List[(LocalName, Term)], out: Term): Term = {
    in.foldRight(out)({
      case ((x, t), sofar) => Lambda(x, t, sofar)
    })
  }

  def unapply(t: Term): Option[(List[(LocalName, Term)], Term)] = t match {
    case Lambda(name, tp, bd) =>
      val (remainingArgs, ultimateScope) = unapply(bd).get //always returns non-None
      Some((name, tp) :: remainingArgs, ultimateScope)
    case t => Some(Nil, t)
  }
}

/**
  * like ApplySpine, but covers the case n=0 akin to FunType
  *
  * note that ApplySpine(f, Nil) != ApplyGeneral(f, Nil)
  */
object ApplyGeneral {
  def apply(f: Term, args: List[Term]): Term = if (args.isEmpty) f else ApplySpine(f, args: _*)

  def unapply(t: Term): Option[(Term, List[Term])] = ApplySpine.unapply(t).orElse(Some((t, Nil)))
}

/**
  * auxiliary con/destructor for HOAS binders, e.g., forall [x:a] b
  */
object Binder {
  def apply(binder: GlobalName, x: LocalName, lfType: Term, body: Term): Term = {
    Apply(OMS(binder), Lambda(x, lfType, body))
  }

  def apply(binder: GlobalName, context: Context, body: Term): Term = {
    context.foldRight(body) { case (next, sofar) =>
      val VarDecl(name, None, Some(tp), None, _) = next
      apply(binder, name, tp, sofar)
    }
  }

  def unapply(t: Term): Option[(GlobalName, LocalName, Term, Term)] = t match {
    case Apply(OMS(binder), Lambda(x, tp, body)) => Some((binder, x, tp, body))
    case _ => None
  }
}

/*

/** The LF foundation. Implements type checking and equality */
class LFF extends Foundation {
   override val logPrefix = "lf"
   val foundTheory = LF.theoryPath
   def typing(tm : Option[Term], tp : Option[Term], G : Context = Context())(implicit fl : FoundationLookup) : Boolean = {
      log("typing\n" + tm.toString + "\n" + tp.toString)
      (tm, tp) match {
         case (Some(s), Some(a)) => check(s, a, G)
         case (Some(s), None) => infer(s, G) != Univ(2)
         case (None, Some(a)) => check(a, Univ(1), G) || check(a, Univ(2), G)
         case (None, None) => false
      }
   }
   def equality(tm1 : Term, tm2 : Term)(implicit fl : FoundationLookup) : Boolean = {
      log("equal\n" + tm1.toString + "\n" + tm2.toString)
      equal(tm1, tm2, Context())
   }
   def inference(tm: Term, context: Context)(implicit lup: Lookup) : Term = infer(tm, context)(new PlainLookup(lup))

/**
 * @param path the URI of a constant
 * @param lib  Lookup library
 * @return type of the constant
 * */
  def lookuptype(path : GlobalName)(implicit fl : FoundationLookup) : Term = fl.getType(path).getOrElse(throw LFError("no type exists for " + path))
  def lookupdef(path : GlobalName)(implicit fl : FoundationLookup) : Option[Term] = fl.getDefiniens(path)

   /**
    * check(s,T,G) iff G |- s : T : U for some U \in {type,kind}
    * checks whether a term s has type T
    */
   def check(s : Term, T : Term, G : Context)(implicit fl : FoundationLookup) : Boolean = {
      s match {
         case Univ(1) => T == Univ(2)
         case OMS(path) => equal(lookuptype(path), T, G)
         case OMV(name) =>  equal(T, G(name).asInstanceOf[VarDecl].tp.get, G) //TODO; why not equal(T, G(name), G)?; what does G(name) return?
         case Lambda(x, a, t) =>
            val G2 = G ++ OMV(x) % a
            reduce(T,G) match { //we reduce the type -> dependent type gets instantiated
               case Pi(y, av, by) =>
                  val bx = by ^ G.id ++ OMV(y)/OMV(x)
                  equal(a, av, G) && check(a, Univ(1), G) && check(t, bx, G2)
               case _ => false
            }
         case Pi(x, a, b) =>
            val G2 = G ++ OMV(x) % a
            (T == Univ(1) || T == Univ(2)) &&
            check(a, Univ(1), G) && check(b, T, G2)
         case Apply(f,arg) =>
            val funtype = infer(f, G)
            val argtype = infer(arg, G)
            //check(f, funtype, G) && check(arg, argtype, G) && {
            reduce(funtype, G) match {
               case Pi(x,a,b) =>
                  equal(argtype, a, G) && equal(T, b ^ (G.id ++ OMV(x)/arg), G)
               case _ => false
            }
            //}
         case _ => false
      }
   }


  /**
   * if G |- tm1 : A and G |- tm2 : A, then equal(tm1,tm2,G) iff G |- tm1 = tm2 : A
   */
   def equal (tm1 : Term, tm2 : Term, G : Context)(implicit fl : FoundationLookup) : Boolean = {
      (tm1, tm2) match {
           case (OMV(x), OMV(y)) => x == y
           case (OMS(c), OMS(d)) => if (c == d) true else {
              lookupdef(c) match {
                 case None => lookupdef(d) match {
                    case None => false
                    case Some(t) => equal(OMS(c), t, G)
                 }
                 case Some(t) => equal(OMS(d), t, G) //flipping the order so that if both c and d have definitions, d is expanded next
              }
           }
           case (Lambda(x1,a1,t1), Lambda(x2,a2,t2)) =>
              val x = if (x1 == x2) x1 else x1/""
              val G2 = G ++ OMV(x) % a1
              equal(a1, a2, G) && equal(t1^(OMV(x1)/OMV(x)), t2^(OMV(x2)/OMV(x)), G2)
           case (Pi(x1,a1,t1), Pi(x2,a2,t2)) =>
              val x = if (x1 == x2) x1 else x1/""
              val G2 = G ++ OMV(x) % a1
              equal(a1, a2, G) && equal(t1^(OMV(x1)/OMV(x)), t2^(OMV(x2)/OMV(x)), G2)
           case (Apply(f1,arg1), Apply(f2,arg2)) => {
              if (equal(f1, f2, G) && equal(arg1, arg2, G)) true else {
                 val tm1r = reduce(tm1, G)
                 val tm2r = reduce(tm2, G)
                 if (tm1r != tm1 || tm2r != tm2) {
                    equal(tm1r, tm2r, G)
                 } else false
              }
           }
           case _ => tm1 == tm2
      }
   }

 /**
  * if t is a constant or an application of a defined constant, it replaces the constant with its definition.
  * if t is a lambda expression application, it is beta-reduced
  * removes top-level redex, application of defined constant
  * if t well-formed, then |- reduce(t,G) = t
  * if |- t = Pi x:A. B for some A,B, then reduce(t) of the form Pi x:A.B (?)
  */
   def reduce(t : Term, G : Context)(implicit fl : FoundationLookup) : Term = t match {
    // t can also be an arbitrary application
         case Apply(Lambda(x,a,t), s) => if (check(s,a,G)) reduce(t ^ (G.id ++ OMV(x)/s), G) else throw LFError("ill-formed")
         case Apply(tm1, tm2) =>
            val tm1r = reduce(tm1, G)
            tm1r match {
               case Lambda(x,a,t) => reduce(Apply(tm1r, tm2), G)
               case _ => Apply(tm1r, tm2)
            }
         case ApplySpine(OMS(p), args) => lookupdef(p) match {
            case None => t
            case Some(d) => reduce(ApplySpine(d, args :_*), G)
         }
         case ApplySpine(_,_) => t
         case OMS(p) => lookupdef(p) match {
            case Some(d) => reduce(d, G)
            case None => t
         }
         case t => t
   }

 /**
  * if exists A such that G |- s : A, then G |- infer(s,G) = A
  */
   def infer(s : Term, G : Context)(implicit fl : FoundationLookup) : Term = {
      s match {
            case Univ(1) => Univ(2)
            case OMS(path) => lookuptype(path)
            case OMV(name) => G(name).tp match {
               case Some(t) => t
               case None => throw LFError("type of " + name + " not known")
            }
            case Lambda(name, tp, body) =>
               val G2 = G ++ OMV(name) % tp
               Pi(name,tp,infer(body, G2))
            case Pi(name, tp, body) =>
               val G2 = G ++ OMV(name) % tp
               infer(body, G2)
            case Apply(f, arg) =>
               // need to ensure that type of argument matches with type of functions
               val funtype = infer(f, G)
               val argtype = infer(arg, G)
               val r = reduce(funtype, G)
               r match {
                  case Pi(x,a,b) =>
                        if (equal(argtype, a, G))
                        b ^ G.id ++ OMV(x)/arg
                     else throw LFError("argument type mismatch: " + argtype + "   !=  " + a)
                  case _ => throw LFError("application of non-function")
               }
            case l: OMLIT => l.synType
            case _ => throw LFError("ill-formed")
       }
   }
}

*/
