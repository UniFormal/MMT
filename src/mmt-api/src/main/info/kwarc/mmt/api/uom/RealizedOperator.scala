package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import objects._
import parser._
import checking._

/** helper class to store the type of a [[RealizedOperator]]s */
case class SynOpType(args: List[Term], ret: Term) {
  def arity = args.length
  def =>:(arg: Term) = SynOpType(arg::args, ret)
}

/** helper class to store the type of a [[SemanticOperator]]s */
case class SemOpType(args: List[SemanticType], ret: SemanticType) {
  def arity = args.length
  /** enables the notation a =>: b =>: c for function types */
  def =>:(arg: SemanticType) = SemOpType(arg::args, ret)
}

/** A RealizedOperator couples a syntactic function (a Constant) with a semantic function (a Scala function) */
case class RealizedOperator(synOp: GlobalName, synTp: SynOpType, semOp: SemanticOperator, semTp: SemOpType) extends BreadthRule(synOp) {
   if (synTp.arity != semTp.arity)
     throw ImplementationError("illtyped realization")
   if (!semOp.getTypes.contains(semTp))
     throw ImplementationError("illtyped realization")

   val arity = synTp.arity
   
   private val argTypes: List[RealizedType] = (synTp.args zip semTp.args) map {case (syn,sem) => RealizedType(syn,sem)} 
   private val retType : RealizedType = RealizedType(synTp.ret, semTp.ret)
   
   private def applicable(args: List[Term]): Boolean = {
      if (args.length != argTypes.length) throw ParseError("")
      (args zip argTypes).forall {
         //case (l: OMLIT, lt) => l.rt == lt
         case (l,lt) if lt.unapply(l).isDefined => true
         case _ => false
      }
   }
   
   def apply(args: List[Term]): OMLIT = {
      if (args.length != arity)
        throw ImplementationError("illegal argument number")
      val argsV = (argTypes zip args) map {case (aT, a) =>
        aT.unapply(a).getOrElse {
          throw ImplementationError("illegal argument type")
        }
      }
      retType(semOp(argsV))
   }
   
   val apply: Rewrite = (args: List[Term]) => {
     if (applicable(args))
        GlobalChange(apply(args))
     else
        NoChange
   }
   
   /** the solution rule if the semantic operator is invertible */
   //TODO pragmatic applications
   def getSolutionRule: Option[SolutionRule] = semOp match {
     case so: Invertible =>
       val sr = new SolutionRule(synOp) {
         def applicable(t: Term) = t match {
           case OMA(OMS(`synOp`), args) =>
             val nonlits = args.zipWithIndex.filterNot(_._1.isInstanceOf[OMLIT])
             nonlits match {
               case (_,i)::Nil => Some(i)
               case _ => None
             }
           case _ => None
         }
         def apply(j: Equality): Option[(Equality,String)] = {
           None //TODO
         }
       }
       Some(sr)
     case _ => None
   }
}

object RealizedOperator {
  /** checks if semOp is declared to have a type that realizes synTp */
  def check(lookup: Term => RealizedType)(semOp: SemanticOperator, synTp: SynOpType): Option[SemOpType] = {
     val retS::argsS = (synTp.ret::synTp.args) map {a => lookup(a).semType} 
     semOp.getTypes.find {st => st.args == argsS && st.ret == retS}
  }
}

/** 
 *  counterpart to a [[RealizedOperator]] for the partial inverse
 */
abstract class InverseOperator(val head: GlobalName) extends UOMRule {
   /** takes the result of applying 'head' and returns the list of arguments */
   def unapply(l: OMLIT): Option[List[OMLIT]]
}