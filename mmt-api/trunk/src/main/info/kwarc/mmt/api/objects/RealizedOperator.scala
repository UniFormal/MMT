package info.kwarc.mmt.api.objects

import info.kwarc.mmt.api._
import uom._

abstract class RealizedOperator(val op: GlobalName) extends BreadthRule(op) {
   val argTypes: List[RealizedType]
   val retType : RealizedType
   def applicable(args: List[Term]): Boolean = {
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

object RealizedOperator {
   def apply(op:GlobalName, rType: RealizedType)(comp: rType.univ): AbbrevRule = new AbbrevRule(op, rType(comp))
   
   def apply(op:GlobalName, argType1: RealizedType, rType: RealizedType)(comp: argType1.univ => rType.univ): RealizedOperator =
      new RealizedOperator(op) {
         val argTypes = List(argType1)
         val retType = rType
         def apply(args: List[Term]): OMLIT = args(0) match {
            case argType1(x) => rType(comp(x))
            case _ => throw ImplementationError("illegal arguments")
         }
      }
   def apply(op:GlobalName, argType1: RealizedType, argType2: RealizedType, rType: RealizedType)
            (comp: (argType1.univ, argType2.univ) => rType.univ): RealizedOperator =
      new RealizedOperator(op) {
         val argTypes = List(argType1, argType2)
         val retType = rType
         def apply(args: List[Term]): OMLIT = (args(0), args(1)) match {
            case (argType1(x), argType2(y)) => rType(comp(x,y))
            case _ => throw ImplementationError("illegal arguments")
         }
      }
   def apply(op:GlobalName, t1: RealizedType, t2: RealizedType, t3: RealizedType, r: RealizedType)
            (comp: (t1.univ, t2.univ, t3.univ)
                   => r.univ): RealizedOperator =
      new RealizedOperator(op) {
         val argTypes = List(t1, t2, t3)
         val retType = r
         def apply(as: List[Term]): OMLIT = (as(0), as(1), as(2)) match {
            case (t1(x1), t2(x2), t3(x3)) =>
               r(comp(x1,x2, x3))
            case _ => throw ImplementationError("illegal arguments")
         }
      }
   def apply(op:GlobalName, t1: RealizedType, t2: RealizedType, t3: RealizedType, t4: RealizedType, r: RealizedType)
            (comp: (t1.univ, t2.univ, t3.univ, t4.univ)
                   => r.univ): RealizedOperator =
      new RealizedOperator(op) {
         val argTypes = List(t1, t2, t3, t4)
         val retType = r
         def apply(as: List[Term]): OMLIT = (as(0), as(1), as(2), as(3)) match {
            case (t1(x1), t2(x2), t3(x3), t4(x4)) =>
               r(comp(x1,x2, x3, x4))
            case _ => throw ImplementationError("illegal arguments")
         }
      }
   def apply(op:GlobalName, t1: RealizedType, t2: RealizedType, t3: RealizedType, t4: RealizedType, t5: RealizedType, r: RealizedType)
            (comp: (t1.univ, t2.univ, t3.univ, t4.univ, t5.univ)
                   => r.univ): RealizedOperator =
      new RealizedOperator(op) {
         val argTypes = List(t1, t2, t3, t4, t5)
         val retType = r
         def apply(as: List[Term]): OMLIT = (as(0), as(1), as(2), as(3), as(4)) match {
            case (t1(x1), t2(x2), t3(x3), t4(x4), t5(x5)) =>
               r(comp(x1,x2, x3, x4, x5))
            case _ => throw ImplementationError("illegal arguments")
         }
      }
   def apply(op:GlobalName, t1: RealizedType, t2: RealizedType, t3: RealizedType, t4: RealizedType, t5: RealizedType, t6: RealizedType,
             r: RealizedType)
            (comp: (t1.univ, t2.univ, t3.univ, t4.univ, t5.univ, t6.univ)
                   => r.univ): RealizedOperator =
      new RealizedOperator(op) {
         val argTypes = List(t1, t2, t3, t4, t5, t6)
         val retType = r
         def apply(as: List[Term]): OMLIT = (as(0), as(1), as(2), as(3), as(4), as(5)) match {
            case (t1(x1), t2(x2), t3(x3), t4(x4), t5(x5), t6(x6)) =>
               r(comp(x1,x2, x3, x4, x5, x6))
            case _ => throw ImplementationError("illegal arguments")
         }
      }
   def apply(op:GlobalName, t1: RealizedType, t2: RealizedType, t3: RealizedType, t4: RealizedType, t5: RealizedType, t6: RealizedType,
             t7: RealizedType, r: RealizedType)
            (comp: (t1.univ, t2.univ, t3.univ, t4.univ, t5.univ, t6.univ, t7.univ)
                   => r.univ): RealizedOperator =
      new RealizedOperator(op) {
         val argTypes = List(t1, t2, t3, t4, t5, t6, t7)
         val retType = r
         def apply(as: List[Term]): OMLIT = (as(0), as(1), as(2), as(3), as(4), as(5), as(6)) match {
            case (t1(x1), t2(x2), t3(x3), t4(x4), t5(x5), t6(x6), t7(x7)) =>
               r(comp(x1,x2, x3, x4, x5, x6, x7))
            case _ => throw ImplementationError("illegal arguments")
         }
      }
   def apply(op:GlobalName, t1: RealizedType, t2: RealizedType, t3: RealizedType, t4: RealizedType, t5: RealizedType, t6: RealizedType,
             t7: RealizedType, t8: RealizedType, r: RealizedType)
            (comp: (t1.univ, t2.univ, t3.univ, t4.univ, t5.univ, t6.univ, t7.univ, t8.univ)
                   => r.univ): RealizedOperator =
      new RealizedOperator(op) {
         val argTypes = List(t1, t2, t3, t4, t5, t6, t7, t8)
         val retType = r
         def apply(as: List[Term]): OMLIT = (as(0), as(1), as(2), as(3), as(4), as(5), as(6), as(7)) match {
            case (t1(x1), t2(x2), t3(x3), t4(x4), t5(x5), t6(x6), t7(x7), t8(x8)) =>
               r(comp(x1,x2, x3, x4, x5, x6, x7, x8))
            case _ => throw ImplementationError("illegal arguments")
         }
      }
   def apply(op:GlobalName, t1: RealizedType, t2: RealizedType, t3: RealizedType, t4: RealizedType, t5: RealizedType, t6: RealizedType,
             t7: RealizedType, t8: RealizedType, t9: RealizedType, r: RealizedType)
            (comp: (t1.univ, t2.univ, t3.univ, t4.univ, t5.univ, t6.univ, t7.univ, t8.univ, t9.univ)
                   => r.univ): RealizedOperator =
      new RealizedOperator(op) {
         val argTypes = List(t1, t2, t3, t4, t5, t6, t7, t8, t9)
         val retType = r
         def apply(as: List[Term]): OMLIT = (as(0), as(1), as(2), as(3), as(4), as(5), as(6), as(7), as(8)) match {
            case (t1(x1), t2(x2), t3(x3), t4(x4), t5(x5), t6(x6), t7(x7), t8(x8), t9(x9)) =>
               r(comp(x1,x2, x3, x4, x5, x6, x7, x8, x9))
            case _ => throw ImplementationError("illegal arguments")
         }
      }
   def apply(op:GlobalName, t1: RealizedType, t2: RealizedType, t3: RealizedType, t4: RealizedType, t5: RealizedType, t6: RealizedType,
             t7: RealizedType, t8: RealizedType, t9: RealizedType, t10: RealizedType, r: RealizedType)
            (comp: (t1.univ, t2.univ, t3.univ, t4.univ, t5.univ, t6.univ, t7.univ, t8.univ, t9.univ, t10.univ)
                   => r.univ): RealizedOperator =
      new RealizedOperator(op) {
         val argTypes = List(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)
         val retType = r
         def apply(as: List[Term]): OMLIT = (as(0), as(1), as(2), as(3), as(4), as(5), as(6), as(7), as(8), as(9)) match {
            case (t1(x1), t2(x2), t3(x3), t4(x4), t5(x5), t6(x6), t7(x7), t8(x8), t9(x9), t10(x10)) =>
               r(comp(x1,x2, x3, x4, x5, x6, x7, x8, x9, x10))
            case _ => throw ImplementationError("illegal arguments")
         }
      }
}

