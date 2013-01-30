package info.kwarc.mmt.api.uom

abstract class OMType {
   type universe
   def is(a: universe): Boolean
   /** assumes is(a) && is(b) */
   def equal(a: universe, b: universe): Boolean
}

object Nat extends OMType {
   type universe = BigInt
   def is(a: universe): Boolean = a >= 0
   def equal(a: universe, b: universe): Boolean = (a == b)   
}

object Rat extends OMType {
   type universe = (BigInt,BigInt)
   def is(a: universe): Boolean = a match {
      case (x,y) => y != 0
   }
   def equal(a: universe, b: universe): Boolean = {
      a._1 * b._2 == a._2 * b._1
   }   
}

object Plus {
   def apply(x: Nat.universe*): Nat.universe = {
      x.foldLeft(0){case (a,b) => a + b}
   }
}
