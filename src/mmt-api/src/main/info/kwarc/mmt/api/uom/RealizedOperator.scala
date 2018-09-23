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

/** A RealizedOperator couples a syntactic constant (a Constant) with a semantic function (a Scala function) */
class RealizedValue(synVal: GlobalName, synTp: Term, semVal: SemanticValue) extends AbbrevRule(synVal, RealizedValue.make(synTp, semVal))

object RealizedValue {
  /** builds the literal from a semantic value */
  def make(tp: Term, semVal: SemanticValue) = {
    val rt = new RealizedType(tp, semVal.tp)
    rt.of(semVal.value)
  }
}
/** A RealizedOperator couples a syntactic function (a Constant) with a semantic function (a Scala function) */
case class RealizedOperator(synOp: GlobalName, synTp: SynOpType, semOp: SemanticOperator, semTp: SemOpType) extends BreadthRule(synOp) {
  /** basic type-checking */
  override def init {
     semOp.init
     if (synTp.arity != semTp.arity)
       throw ImplementationError("syntactic and semantic arity are not equal")
     if (!semOp.getTypes.contains(semTp))
       throw ImplementationError("the semantic operator does not have the semantic type")
  }

   val arity = synTp.arity

   private val argTypes: List[RealizedType] = (synTp.args zip semTp.args) map {case (syn,sem) => RealizedType(syn,sem)}
   private val retType : RealizedType = RealizedType(synTp.ret, semTp.ret)

   private def applicable(args: List[Term]): Boolean = {
      if (args.length != argTypes.length) return false
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
      retType of semOp(argsV)
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
       val sr = new ValueSolutionRule(synOp) {
         def applicable(t: Term) = t match {
           case OMA(OMS(`synOp`), args) if args.length == arity =>
             val nonlits = args.zipWithIndex.filterNot(_._1.isInstanceOf[OMLIT])
             nonlits match {
               case (_,i)::Nil => Some(i)
               case _ => None
             }
           case _ => None
         }
         def apply(j: Equality): Option[(Equality,String)] = {
           val res = j.tm2 match {
             case retType(v) => v
             case _ => return None
           }
           val OMA(_, args) = j.tm1
           val ukArgs = args.zipWithIndex map {
             case (l: OMLIT, p) => KnownArg(l.value, p)
             case (_, p) => new UnknownArg(semTp.args(p), p)
           }
           so.invert(ukArgs, res) flatMap {b =>
             if (b) {
               val eqs = ukArgs.flatMap {
                 case u: UnknownArg =>
                   val p = u.pos
                   u.getSolution match {
                     case Some(uS) =>
                       val eq = Equality(j.stack, args(p), argTypes(p) of uS, Some(synTp.args(p)))
                       List(eq)
                     case None =>
                       // the inversion rule thinks it found a solution but it was discarded because it was invalid (because the expected type sharper)
                       // we can signal failure here as well
                       None
                   }
                 case _ => Nil
               }
               Some((eqs.head, "inverting operator " + synOp))
             } else
               // actually, we can signal failure, but the rule does not have access to the solver
               None
           }
         }
       }
       Some(sr)
     case _ => None
   }

   override def providedRules = super.providedRules ::: getSolutionRule.toList
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
