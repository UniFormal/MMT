package info.kwarc.mmt.lf

import info.kwarc.mmt.api._
import uom._
import objects._

class NullaryLFConstantScala(val parent: MPath, val name: String) extends ConstantScala {
  def filter(args: List[Term]) = args.filterNot {a => a == term}
}

class UnaryLFConstantScala(val parent: MPath, val name: String) extends ConstantScala {
   def apply(arg: Term) = Apply(OMS(path), arg)
   def unapply(t: Term) = t match {
      case ApplySpine(OMS(this.path), List(a)) => Some(a)
      case _ => None
   }
}
class BinaryLFConstantScala(val parent: MPath, val name: String) extends ConstantScala {
   def apply(arg1: Term, arg2: Term) = ApplySpine(OMS(path), arg1, arg2)
   def assoc(neutral: Term, args: List[Term]) = {
     args match {
       case Nil => neutral
       case hd::tl => tl.fold(hd) {case (x,y) => apply(x,y)}
     }
   }
   def unapply(t: Term) = t match {
      case ApplySpine(OMS(this.path), List(a1, a2)) => Some((a1,a2))
      case _ => None
   }

   def associativeArguments(t: Term):List[Term] = unapply(t) match {
     case Some((x,y)) => associativeArguments(x):::associativeArguments(y)
     case None => List(t)
   }
}

class TernaryLFConstantScala(val parent: MPath, val name: String) extends ConstantScala {
   def apply(a1: Term, a2: Term, a3: Term) = ApplySpine(OMS(path), a1, a2, a3)
   def unapply(t: Term) = t match {
      case ApplySpine(OMS(this.path), List(a1, a2, a3)) => Some((a1,a2,a3))
      case _ => None
   }
}

class FouraryLFConstantScala(val parent: MPath, val name: String) extends ConstantScala {
   def apply(a1: Term, a2: Term, a3: Term, a4: Term) = ApplySpine(OMS(path), a1, a2, a3, a4)
   def unapply(t: Term) = t match {
      case ApplySpine(OMS(this.path), List(a1, a2, a3, a4)) => Some((a1,a2,a3,a4))
      case _ => None
   }
}

/**
 * auxiliary con/destructor for plain HOAS binders, e.g., forall [x:a] b with forall : (a -> prop) -> prop
 * 
 * See also [[TypedBinderScala]]
 */
class PlainBinderScala(val parent: MPath, val name: String) extends ConstantScala {
  def apply(x: LocalName, lfType: Term, body: Term): Term = {
    Apply(this.term, Lambda(x, lfType, body))
  }
  def apply(context: Context, body: Term): Term = {
    context.foldRight(body) {case (next, sofar) =>
      val VarDecl(name, None, Some(tp), None, _) = next
      apply(name, tp, sofar)
    }
  }
  def unapply(t: Term) : Option[(LocalName, Term, Term)] = t match {
     case Apply(this.term, Lambda(x, tp, body)) => Some((x, tp, body))
     case _ => None
  }
}

/**
 * auxiliary con/destructor for plain HOAS binders that take an extra argument for the variable type, e.g., forall S [x:tm S] b
  * with forall {s} (tm s -> prop) -> prop
 * 
 * See also [[PlainBinderScala]]
 * 
 * @param tm the operator used to lift the first argument to the LF-type level
 */
class TypedBinderScala(val parent: MPath, val name: String, tm: UnaryLFConstantScala) extends ConstantScala {Single =>
  def apply(x: LocalName, sort: Term, body: Term): Term = {
    Apply(this.term, Lambda(x, tm(sort), body))
  }
  def apply(binder: GlobalName, context: Context, body: Term): Term = {
    context.foldRight(body) {case (next, sofar) =>
      val VarDecl(name, None, Some(tm(sort)), None, _) = next
      apply(name, sort, sofar)
    }
  }
  def unapply(t: Term) : Option[(LocalName, Term, Term)] = t match {
     case Apply(this.term, Lambda(x, tm(sort), body)) => Some((x, sort, body))
     case _ => None
  }
  
  object multiple {
    def apply(bindings: List[(LocalName,Term)], body: Term) = {
      bindings.foldRight(body) {case ((x,s),sofar) =>
        Single(x,s,sofar)
      }
    }
    def unapply(t: Term): Option[(List[(LocalName,Term)],Term)] = t match {
      case Single(x,s,rest) => unapply(rest) match {
        case None =>
          Some((List((x,s)),rest))
        case Some((bindings,body)) =>
          Some(((x,s)::bindings, body))
      }
    }
  }
}

