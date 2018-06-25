package info.kwarc.mmt.lf.EquivalenceRelations

import info.kwarc.mmt.api._
import objects._
import symbols._
import notations._
import checking._
import modules._

import info.kwarc.mmt.lf._
//import scala.collection.parallel.ParIterableLike.Copy

sealed abstract class InductiveDecl {
  def name: LocalName
  def args: List[(Option[LocalName], Term)]
  def ret: Term
  def tp : Term = {FunType(args, ret)}
  def toTerm = FunType(args,ret)
}
case class TypeLevel(name: LocalName, args: List[(Option[LocalName], Term)]) extends InductiveDecl {
  def ret = Univ(1)
}
case class TermLevel(name: LocalName, args: List[(Option[LocalName], Term)], ret: Term) extends InductiveDecl
case class StatementLevel(name: LocalName, args: List[(Option[LocalName], Term)]) extends InductiveDecl {
  def ret = Univ(1)
}

class InductiveTypes extends StructuralFeature("EquivalenceRelation") with ParametricTheoryLike {
  def isJudgment(tp: Term): Boolean = tp match {
      case FunType(_, ApplySpine(OMS(s),_)) =>
         //this can throw errors if the implicit graph is not fully loaded
         try {
            controller.globalLookup.getConstant(s).rl.contains("Judgment")
         } catch {case e: Error =>
            false
         }
      case _ => false
   }
  //def getHeaderNotation = List(LabelArg(1, LabelInfo.none))

  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment) {
    //TODO: check for inhabitability
  }

  def elaborate(parent: DeclaredModule, dd: DerivedDeclaration) = {
    val args = dd.tpC.get match {
      case Some(OMA(_, args)) => args
    }
    val (tpArg:OMV, relName:LocalName, relArg:OMV, relTp:OMV) = args match {
        case x::List(y) => x match {
          case Univ(1) => y match {
            case OML(relName, Some(relTp), Some(relArg), _, _) => (x,relName, relArg, relTp)
          }
        }
      case _ => throw LocalError("bad Arguments")
    }
    val loc= LocalName("")
    val Lambda(_, Lambda(_, a, b), prop) = relTp
    if (a != b)
      throw LocalError("conflicting argument types: The second argument is not of type A -> A -> Bool")
    if (a != tpArg)
      throw LocalError("conflicting argument types: The second argument is not a relation on the type A specified by the first argument")
    val relDecl = VarDecl(LocalName(parent.name.toString()+relArg.toMPath.last), None, Some(tpArg), Some(relArg), None).toDeclaration(parent.toTerm)
    
    val True : Term = LFEquality(relArg, relArg)
    val List(c, d, e, f, g, h):List[OMV] = List(1 to 6) map {n => OMV(parent.path+"quantifiedVar"+n.toString())}
    val List(i, j, k, l, m, n) = List(c, d, e, f, g, h) map {tm => OML(tm.name, Some(tpArg), None, None, None)}
    val dedPath : LocalName = LocalName("http://docs.omdoc.org/urtheories/primitive_types/bool.mmt#335.14.2:387.14.54")
    val ded = OMV(dedPath)
    val transBody = LFEquality(True, Arrow(OMA(relArg, List(i, j)), Arrow(OMA(relArg, List(j, k)), OMA(relArg, List(i, k)))))
    val trans= Pi(i.name, i.tp.get, Pi(j.name, j.tp.get, Pi(k.name, k.tp.get, OMA(ded, List(transBody)))))
    val symmBody = LFEquality(True, Arrow(OMA(relArg, List(l, m)), OMA(relArg, List(m, l))))
    val symm = Pi(l.name, l.tp.get, Pi(m.name, m.tp.get, OMA(ded, List(symmBody))))
    val reflBody = LFEquality(True, OMA(relArg, List(n, n)))
    val refl = Pi(n.name, l.tp.get, OMA(ded, List(reflBody)))
    val predTrans = Lambda(LocalName(parent.name+"transitive_predicate"), relTp, trans)
    val predRefl = Lambda(loc, relTp, refl)
    val predSymm = Lambda(loc, relTp, symm)
    
    
    val applyPred = {pred:Term => OMBIND(ded, Context.empty,OMBIND(pred, Context.empty, relArg))}
    val axiomToDecl = {ax:Term => VarDecl(LocalName(parent.name.toString()+ax.toString()), None, Some(Univ(1)),  Some(ax), None).toDeclaration(parent.toTerm)}
    val predToDecl = {pred:Term => axiomToDecl(applyPred(pred))}
    val elabDecls @ _::transDecl::reflDecl::List(symmDecl) = relDecl::(predTrans::predRefl::List(predSymm) map predToDecl)
    new Elaboration {
      def domain = elabDecls map {d => d.name}
      def getO(n: LocalName) = {
        elabDecls.find(_.name == n)
      }
    }
  }
}

object InductiveRule extends StructuralFeatureRule(classOf[InductiveTypes], "EquivalenceRelation")
