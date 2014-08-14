package info.kwarc.mmt.api.objects

import info.kwarc.mmt.api._
import uom._

abstract class RealizedOperator(op: GlobalName) extends BreadthRule(op) {
   val argTypes: List[RealizedType]
   val retType : RealizedType
   private def applicable(args: List[Term]): Boolean = {
      if (args.length != argTypes.length) throw ParseError("")
      (args zip argTypes).forall {
         case (l: OMLIT, lt) => l.rt == lt
         case _ => false
      }
   }
   def apply(args: List[Term]): OMLIT
   
   val apply: Rewrite = (args: List[Term]) => {
         if (applicable(args))
            GlobalChange(apply(args))
         else
            NoChange
   }
}


/** 
 *  counterpart to a [[RealizedOperator]] for the partial inverse
 */
abstract class InverseOperator(val head: GlobalName) extends UOMRule {
   /** takes the result of applying 'head' and returns the list of arguments */
   def unapply(l: OMLIT): Option[List[OMLIT]]
}

