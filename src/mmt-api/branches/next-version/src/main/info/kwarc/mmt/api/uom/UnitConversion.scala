package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import objects._

object UnitConvImplems {
  val base = DPath(utils.URI("http", "cds.omdoc.org") / "foundations" / "lf" / "lf.omdoc")

  def plus_implem(l : Term*) : Term = {
    def arithm(l : Term*) : Double = {
      l.toList match {
        case Nil => 0
        case OMF(i) :: tl => i + arithm(tl :_*)
        case _ => throw new Exception("plus_implem works only with OMF")
      }
    }
    
    return OMF(arithm(l:_*))
  }
  val plus = new Implementation (
    base ? "domain" ? "+",
    plus_implem
  )

  
  def mult_implem(l : Term*) : Term = {
    def arithm(l : Term*) : Double = {
      l.toList match {
        case Nil => 1
        case OMF(i) :: tl => i * arithm(tl :_*)
        case _ => throw new Exception("plus_implem works only with OMF")
      }
    }
    
    return OMF(arithm(l:_*))
  }
  val mult = new Implementation (
    base ? "domain" ? "*",
    mult_implem
  )

  def minus_implem(l : Term*) : Term = {
    l.toList match {
      case OMF(i) :: OMF(j) :: Nil => return OMF(i-j)
      case _ => throw new Exception("minus_implem works only with 2 OMFs")
    }
  }
  val minus = new Implementation (
    base ? "domain" ? "-",
    minus_implem
  )
  
  def divide_implem(l : Term*) : Term = {
    l.toList match {
      case OMF(i) :: OMF(j) :: Nil => {
        if (j == 0)
          throw new Exception("division by zero")
        return OMF(i/j)
      }
      case _ => throw new Exception("divide_implem works only with 2 OMFs")
    }
  }
  val divide = new Implementation (
    base ? "domain" ? "%2F",
    divide_implem
  )

}
