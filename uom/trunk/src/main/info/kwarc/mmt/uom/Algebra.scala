/*package main.info.kwarc.mmt.uom

case class Factor(base: Term, power: Int) {
   def invert = Factor(base, - power)
}

class Factorize(alg: Algebra) {
   def explode(t : Term) : List[Factor] = t match {
      case OMA(alg.comp, args)           => args.flatMap(explode)
      case OMA(alg.unaryinv, List(t))    => explode(t).reverseMap(invert)
      case OMA(alg.binaryinv, List(s,t)) => explode(s) ::: explode(t).reverseMap(invert)
      case OMID(alg.unit)                => Nil
      case OMA(alg.power, List(b, p)) if alg.commutative =>
         simplify(p) match {
            case OMI(n) => explode(b).map(Factor(_,n))
            case _ => List(Factor(t, 1))
         }
      case t =>
        val r = simplify(t)
        if (r == t) List(Factor(t, 1)) else explode(r)
   }
   def implode(factors : List[Factor]) : Term =
      if (alg.commutative) alg.unit
}

class Algebra(val comp: GlobalName, unaryinv: GlobalName, binaryinv: GlobalName, unit: GlobalName, power: GlobalName)

abstract class Rule {
   val head : GlobalName
   def apply(t: Term) : Term
}

class Semigroup(val head: GlobalName) extends Rule {
   def apply(t: Term) = t match {
      case OMA(OMID(head), args) => 
         val newargs = args flatMap {
            case OMA(OMID(head), as) => as
            case t => List(t)
         }
         OMA(OMID(head), newargs)
      case t => t
   }
}

class Simplifier {
   def rules = new HashMap[GlobalName, List[GlobalName]]  
   def apply(t: Term) : Term = {
      rules.get(t.head) match {
         case Some(r) => r(t)
         case None => 
      }
   }
}

class Monoid

class Group(mon : Monoid, inv: GlobalName, bininv) {
   def apply(arg: Term) = arg match {
      case OMA(OMID(inv), List(t)) => t
      case OMA(OMID(mon.comp), args) => OMA(OMID(mon.comp), args.reverseMap(OMID(inv), _))
      case OMID(mon.unit) => OMID(mon.unit)
      case t => OMA(OMID(inv), List(t))
   }
}


*/