package info.kwarc.mmt.uom

import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api._

case class UOMError(msg: String) extends java.lang.Throwable

class Implementation(constantName : GlobalName, function : ((Term*) => Term) ) {
  def name = constantName
  def f = function
  def apply(args : Term*) : Term = { f(args : _*)}
}

object Implementation {
   def apply(name: GlobalName)(f: (Term*) => Term) = {
      new Implementation(name, f)
   }
   // we can use this one later; currently there are no arities
   def apply(name: GlobalName, n: Int)(f: (Term*) => Term) = {
      def frestricted(args: Term*) = if (args.length == n) f(args: _*) else throw UOMError("bad number of arguments")
      new Implementation(name, frestricted)
   }
}

object Arguments {
   def unapplySeq(args: Seq[Term]) : Option[Seq[Term]] = Some(args)
}

object UnitConvImplems {
  val base = DPath(new utils.xml.URI("http://cds.omdoc.org/foundations/lf/lf.omdoc"))

  def plus_implem(l : Term*) : Term = {
    def arithm(l : Term*) : BigInt = {
      l.toList match {
        case Nil => 0
        case OMI(i) :: tl => i + arithm(tl :_*)
        case _ => throw new Exception("plus_implem works only with integers")
      }
    }
    
    return OMI(arithm(l:_*))
  }
  val plus = new Implementation (
    base ? "domain" ? "+",
    plus_implem
  )

  
  def mult_implem(l : Term*) : Term = {
    def arithm(l : Term*) : BigInt = {
      l.toList match {
        case Nil => 1
        case OMI(i) :: tl => i * arithm(tl :_*)
        case _ => throw new Exception("plus_implem works only with integers")
      }
    }
    
    return OMI(arithm(l:_*))
  }
  val mult = new Implementation (
    base ? "domain" ? "*",
    mult_implem
  )
}
