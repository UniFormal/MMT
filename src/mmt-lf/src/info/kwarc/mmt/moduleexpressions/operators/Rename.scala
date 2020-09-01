package info.kwarc.mmt.moduleexpressions.operators

import info.kwarc.mmt.api.LocalName
import info.kwarc.mmt.api.checking.{CheckingCallback, ComputationRule, History}
import info.kwarc.mmt.api.objects.{AnonymousDiagram, AnonymousMorphism, DiagramArrow, DiagramNode, OML, Stack, Term}
import info.kwarc.mmt.api.uom.{BinaryConstantScala, FlexaryConstantScala, RecurseOnly, Simplifiability, Simplify}
import info.kwarc.mmt.api.utils.SkipThis

object Rename extends FlexaryConstantScala(Combinators._path, "rename") {
  /** the label of the renamed theory */
  val nodeLabel = LocalName("pres")
  /** the label of the renaming morphism (from old to renamed) */
  val arrowLabel = LocalName("rename")

  def pairToTerm(on: (LocalName, LocalName)) = OML(on._1, None, Some(OML(on._2)))

  def pairsToMorph(on: List[(LocalName, LocalName)]) = new AnonymousMorphism(on map pairToTerm)
}

object Rename1 extends BinaryConstantScala(Combinators._path, "rename1")

object ComputeRename extends ComputationRule(Rename.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = {
    val Rename(diag, rens@_*) = tm
    val ad = Common.asAnonymousDiagram(solver, diag).getOrElse {
      return RecurseOnly(List(1))
    }
    // perform the renaming
    val oldNew: List[(LocalName, Term)] = rens.toList.mapOrSkip {
      case Rename1(OML(old, None, None, _, _), nw) => (old, nw)
      case r =>
        solver.error("not a renaming " + r)
        throw SkipThis
    }
    val dN = ad.getDistNode.getOrElse {
      solver.error("distinguished node not found")
      return Simplifiability.NoRecurse
    }
    val atR = dN.theory.rename(oldNew: _*)
    val dNR = DiagramNode(Rename.nodeLabel, atR)
    val renM = new AnonymousMorphism(oldNew map { case (o, n) => OML(o, None, Some(n)) })
    val renA = DiagramArrow(Rename.arrowLabel, dN.label, dNR.label, renM, true)
    val adP = Common.prefixLabels(ad, Rename.arrowLabel)
    val result = new AnonymousDiagram(adP.nodes ::: List(dNR), adP.arrows ::: List(renA), Some(Rename.nodeLabel))
    // remove all invalidated realizations, i.e., all that realized a theory one of whose symbols was renamed
    // TODO more generally, we could keep track of the renaming necessary for this realization, but then realizations cannot be implicit anymore
    /*val removeReals = thyAnon.decls.flatMap {
      case oml @ RealizeOML(p, _) =>
        solver.lookup.getO(p) match {
          case Some(dt: Theory) =>
            if (oldNew.exists {case (old,nw) => dt.declares(old)})
              List(oml)
            else
              Nil
          case _ => Nil
        }
      case _ => Nil
    }
    thyAnon.decls = thyAnon.decls diff removeReals */
    Simplify(result.toTerm)
  }
}
