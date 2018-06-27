package info.kwarc.mmt.lf.EquivalenceRelations

import info.kwarc.mmt.api._
import objects._
import symbols._
import notations._
import checking._
import modules._

import info.kwarc.mmt.lf._
import info.kwarc.mmt.lf.induction.InductiveTypes

//import scala.collection.parallel.ParIterableLike.Copy

class EquivalenceRelation extends StructuralFeature("EquivalenceRelation") with ParametricTheoryLike {
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
    val Lambda(_, Lambda(_, a, b), prop) = relTp
    if (a != b)
      throw LocalError("conflicting argument types: The second argument is not of type A -> A -> Bool")
    if (a != tpArg)
      throw LocalError("conflicting argument types: The second argument is not a relation on the type A specified by the first argument")
    val relDecl = VarDecl(LocalName(parent.name.toString()+relArg.toMPath.last), None, Some(tpArg), Some(relArg), None).toDeclaration(parent.toTerm)
    
    val True : Term = LFEquality(relArg, relArg)
    val con = Context(dd.modulePath)
    def newVar() : OMV = {
      val tm : Term = Context.pickFresh(Context(dd.modulePath), LocalName("x")) match {case (nm:LocalName, s:Substitution) => s.apply(nm).get}
      tm match {
        case x:OMV => x
        case _ => throw ImplementationError("Unexpected result: Context.pickFresh doesn't generate an OMV")
      }
    }
    Context.pickFresh(con, LocalName("x")) match {case (name:LocalName, s:Substitution) => s.apply(name).get}
    val List(c, d, e, f, g, h):List[OMV] = List(1 to 6) map {n => newVar}
    val List(i, j, k, l, m, n) = List(c, d, e, f, g, h) map {tm:OMV => OML(tm.name, Some(tpArg), None, None, None)}
    val dedPath : LocalName = LocalName("http://docs.omdoc.org/urtheories/primitive_types/bool.mmt#335.14.2:387.14.54")
    val ded = OMV(dedPath)
    val DED = {x:Term => OMA(ded, List(x))}
    def ApplRel(x:Term,y:Term) : Term = DED(OMA(relArg, List(l, m)))
    val transBody = LFEquality(True, Arrow(ApplRel(i, j), Arrow(ApplRel(j, k), ApplRel(i, k))))
    val trans= Pi(i.name, i.tp.get, Pi(j.name, j.tp.get, Pi(k.name, k.tp.get, DED(transBody))))
    val symmBody = LFEquality(True, Arrow(ApplRel(l, m), ApplRel(m, l)))
    val symm = Pi(l.name, l.tp.get, Pi(m.name, m.tp.get, DED(symmBody)))
    val reflBody = LFEquality(True, ApplRel(n, n))
    val refl = Pi(n.name, l.tp.get, DED(reflBody))
    val predTrans = Lambda(LocalName(parent.name+"transitivity_predicate"), relTp, trans)
    val predRefl = Lambda(LocalName(parent.name+"reflexivity_predicate"), relTp, refl)
    val predSymm = Lambda(LocalName(parent.name+"symmetry_predicate"), relTp, symm)
    
    val applyPred = {pred:Term => OMBIND(ded, Context.empty,OMBIND(pred, Context.empty, relArg))}
    val axiomToDecl = {ax:Term => VarDecl(LocalName(parent.name.toString()+ax.toString()), None, Some(Univ(1)),  Some(ax), None).toDeclaration(parent.toTerm)}
    val predToDecl = {pred:Term => axiomToDecl(applyPred(pred))}
    val elabDecls @ List(_, transDecl, reflDecl, symmDecl) = relDecl::(List(predTrans, predRefl, predSymm) map predToDecl)
    new Elaboration {
      def domain = elabDecls map {d => d.name}
      def getO(n: LocalName) = {
        elabDecls.find(_.name == n)
      }
    }
  }
}

object InductiveRule extends StructuralFeatureRule(classOf[InductiveTypes], "EquivalenceRelation")
