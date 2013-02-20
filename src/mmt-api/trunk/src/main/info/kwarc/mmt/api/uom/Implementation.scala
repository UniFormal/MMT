package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api._

case class UOMError(msg: String) extends java.lang.Throwable

class Implementation(constantName : GlobalName, function : List[Term] => Term) extends BreadthRule(constantName) {
  def name = constantName
  val apply: Rewrite = (args : List[Term]) => {
     try {
        val res = function(args)
        res match {
          case OMA(OMS(this.constantName), newArgs) =>
            if (newArgs == args)
              NoChange
            else
              LocalChange(newArgs)
          case _ => GlobalChange(res)
        }
     } catch {
        case UOMError(_) => NoChange
     }
  }
}

object Flexary {
   /** convenience factory for functions of type obj* -> obj */
   def apply(name: GlobalName)(f: List[Term] => Term) = new Implementation(name, f)
}
object Unary {
   /** convenience factory for functions of type obj  -> obj */
   def apply(name: GlobalName)(f: Term    => Term): Implementation = Flexary(name) {args =>
      if (args.length != 1) throw UOMError("bad number of arguments")
      f(args.head)
   }
}
object Binary {
   /** convenience factory for functions of type obj x obj -> obj */
   def apply(name: GlobalName)(f: (Term,Term) => Term): Implementation = Flexary(name) {args =>
      if (args.length != 2) throw UOMError("bad number of arguments")
      f(args(0), args(1))
   }
}
object Ternary {
   /** convenience factory for functions of type obj x obj x obj -> obj */
   def apply(name: GlobalName)(f: (Term,Term,Term) => Term): Implementation = Flexary(name) {args =>
      if (args.length != 3) throw UOMError("bad number of arguments")
      f(args(0), args(1), args(3))
   }
}
object OneAndFlexary {
   /** convenience factory for functions of type obj x obj* -> obj */
   def apply(name: GlobalName)(f: (Term,List[Term]) => Term): Implementation = Flexary(name) {args =>
      if (args.isEmpty) throw UOMError("too few arguments")
      f(args.head, args.tail)
   }
}
object TwoAndFlexary {
   /** convenience factory for functions of type obj x obj x obj* -> obj */
   def apply(name: GlobalName)(f: (Term,Term,List[Term]) => Term): Implementation = Flexary(name) {args =>
      if (args.length <= 1) throw UOMError("too few arguments")
      f(args(0), args(1), args.drop(2))
   }
}

object Arguments {
   def unapplySeq(args: List[Term]) : Option[Seq[Term]] = Some(args)
}

