package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api._

case class UOMError(msg: String) extends java.lang.Throwable

class Implementation(constantName : GlobalName, function : ((Term*) => Term) ) extends BreadthRule(constantName) {
  def name = constantName
  val apply: Rewrite = (args : List[Term]) => {
     try {
        val res = function(args : _*)
        res match {
          case OMA(OMS(constantName), args) => LocalChange(args)
          case _ => GlobalChange(res)
        }
     } catch {
        case UOMError(_) => NoChange
     }
  }
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
   def unapplySeq(args: List[Term]) : Option[Seq[Term]] = Some(args)
}

