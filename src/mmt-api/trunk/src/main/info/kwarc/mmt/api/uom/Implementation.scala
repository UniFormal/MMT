package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api._

case class UOMError(msg: String) extends java.lang.Throwable

case class Unimplemented(msg: String) extends java.lang.Throwable

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
        case Unimplemented(_) => NoChange
     }
  }
}

object Implementation {
   /** convenience factory for functions of type obj*  -> obj */
   def S(name: GlobalName)(f: List[Term] => Term) = new Implementation(name, f)
   /** convenience factory for functions of type  -> obj */
   def constant(name: GlobalName)(f: () => Term) =
      AbbrevRule(name, f())
   /** convenience factory for functions of type obj  -> obj */
   def A(name: GlobalName)(f: Term    => Term): Implementation = S(name) {args =>
      if (args.length != 1) throw UOMError("bad number of arguments")
      f(args.head)
   }
   /** convenience factory for functions of type obj x obj -> obj */
   def AA(name: GlobalName)(f: (Term,Term) => Term): Implementation = S(name) {args =>
      if (args.length != 2) throw UOMError("bad number of arguments")
      f(args(0), args(1))
   }
   /** convenience factory for functions of type obj x obj x obj -> obj */
   def AAA(name: GlobalName)(f: (Term,Term,Term) => Term): Implementation = S(name) {args =>
      if (args.length != 3) throw UOMError("bad number of arguments")
      f(args(0), args(1), args(2))
   }
   
   /** convenience factory for functions of type obj x obj x obj -> obj */
   def AAAA(name: GlobalName)(f: (Term,Term,Term,Term) => Term): Implementation = S(name) {args =>
      if (args.length != 4) throw UOMError("bad number of arguments")
      f(args(0), args(1), args(2), args(3))
   }
   
   /** convenience factory for functions of type obj x obj x obj -> obj */
   def AAAAA(name: GlobalName)(f: (Term,Term,Term,Term,Term) => Term): Implementation = S(name) {args =>
      if (args.length != 5) throw UOMError("bad number of arguments")
      f(args(0), args(1), args(2), args(3), args(4))
   }
   /** convenience factory for functions of type obj x obj* -> obj */
   def AS(name: GlobalName)(f: (Term,List[Term]) => Term): Implementation = S(name) {args =>
      if (args.isEmpty) throw UOMError("too few arguments")
      f(args.head, args.tail)
   }
   /** convenience factory for functions of type obj x obj x obj* -> obj */
   def AAS(name: GlobalName)(f: (Term,Term,List[Term]) => Term): Implementation = S(name) {args =>
      if (args.length <= 1) throw UOMError("too few arguments")
      f(args(0), args(1), args.drop(2))
   }
}
