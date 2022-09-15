package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import objects._
import parser._
import checking._

/** helper class to store the type of a [[RealizedOperator]]s
 *  @param under HOAS application operators
 *  @param args argument types
 *  @param ret return type
 */
case class SynOpType(under: List[GlobalName], args: List[Term], ret: Term) {
  def arity = args.length
  def =>:(arg: Term) = SynOpType(under, arg::args, ret)
}

/** helper class to store the type of a [[SemanticOperator]]s */
case class SemOpType(args: List[SemanticType], ret: SemanticType) {
  def arity = args.length
  /** enables the notation a =>: b =>: c for function types */
  def =>:(arg: SemanticType) = SemOpType(arg::args, ret)
  
  def subtype(into: SemOpType): Boolean = {
    if (args.length != into.args.length) return false
    ((into.args zip args) forall {case (a,i) => i subtype a}) && (ret subtype into.ret)
  }
}

/** A RealizedOperator couples a syntactic constant (a Constant) with a semantic function (a Scala function) */
class RealizedValue(synVal: GlobalName, synTp: Term, semVal: SemanticValue) extends AbbrevRule(synVal, RealizedValue.make(synTp, semVal))

object RealizedValue {
  /** builds the literal from a semantic value */
  def make(tp: Term, semVal: SemanticValue) = {
    val rt = RealizedType(tp, semVal.tp)
    rt.of(semVal.value)
  }
}
/** A RealizedOperator couples a syntactic function (a Constant) with a semantic function (a Scala function) */
case class RealizedOperator(synOp: GlobalName, synTp: SynOpType, semOp: SemanticOperator, semTp: SemOpType) extends SimplificationRule(synOp) {
  /** basic type-checking */
  override def init: Unit = {
     semOp.init
     if (synTp.arity != semTp.arity)
       throw ImplementationError("syntactic and semantic arity are not equal")
     if (! (semOp.getTypes exists {st => semTp subtype st}))
       throw ImplementationError("the semantic operator does not have the semantic type")
  }

   val arity = synTp.arity
   private val under = synTp.under
   private val underL = under.length

   private val argTypes: List[RealizedType] = (synTp.args zip semTp.args) map {case (syn,sem) => RealizedType(syn,sem)}
   private val retType : RealizedType = RealizedType(synTp.ret, semTp.ret)

   private val App = new notations.OMAUnder(under)
   
   def apply(context: Context, t: Term): Simplifiability = {
      t match {
        case App(OMS(`synOp`),args) if args.length == arity =>
          val argsV = (args zip argTypes).zipWithIndex map {case ((a,aT),i) =>
             aT.unapply(a).getOrElse {
               return RecurseOnly(List(underL+i))
             }
          }
          val tS = retType of semOp(argsV)
          Simplify(tS)
        case _ =>
          Simplifiability.NoRecurse
      }
   }

   /** the inversion rule if the semantic operator is invertible */
   private def getInversionRule: Option[InverseOperator] = semOp match {
     case so: Invertible =>
       val io = new InverseOperator(synOp) {
         def unapply(l: OMLIT): Option[List[OMLIT]] = {
            val v = l match {
              case retType(v) => v
              case _ => return None
            }
            val uargs = semTp.args.zipWithIndex map {case (t,i) => new UnknownArg(t,i)}
            val r = so.invert(uargs, v) 
            if (r contains true) {
              // both lists have length arity
              val args = (argTypes zip uargs) map {case (atp,ua) =>
                // .get defined because so.invert succeeded
                // .of defined because only valid solutions are filled in
                atp.of(ua.getSolution.get)
              }
              Some(args)
            } else None
         }
       }
       Some(io)
     case _ => None
   }
   
   /** the solution rule if the semantic operator is invertible */
   private def getSolutionRule: Option[SolutionRule] = semOp match {
     case so: Invertible =>
       val sr = new ValueSolutionRule(synOp) {
         def applicable(t: Term) = t match {
           case OMA(f,args) =>
             val comps = f::args
             if (comps.startsWith(synTp.under.map(OMS(_)) ::: List(OMS(synOp)))) {
               val numUnderArgs = synTp.under.length
               val effectiveArgs = args.drop(numUnderArgs)
               if (effectiveArgs.length == arity) {
                 val nonlits = effectiveArgs.zipWithIndex.filterNot(_._1.isInstanceOf[OMLIT])
                 nonlits match {
                   case (_,i)::Nil => Some(numUnderArgs+i)
                   case _ => None
                 }
               } else None
             } else None
           case _ => None
         }
         def apply(j: Equality): Option[(Equality,String)] = {
           val res = j.tm2 match {
             case retType(v) => v
             case _ => return None
           }
           val OMA(_, allArgs) = j.tm1
           val args = allArgs.drop(synTp.under.length)
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

   override def providedRules = super.providedRules ::: getSolutionRule.toList ::: getInversionRule.toList
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
 *  This is also possible if head is injective. See [[lf.LFRealizationInScala]] for the non-injective case.
 */
abstract class InverseOperator(val head: GlobalName) extends SimplifierRule {
   /** takes the result of applying 'head' and returns the list of arguments */
   def unapply(l: OMLIT): Option[List[OMLIT]]
}
