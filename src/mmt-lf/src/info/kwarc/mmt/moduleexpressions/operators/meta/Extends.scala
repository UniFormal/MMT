package info.kwarc.mmt.moduleexpressions.operators.meta

import info.kwarc.mmt.api.LocalName
import info.kwarc.mmt.api.checking.{CheckingCallback, ComputationRule, History}
import info.kwarc.mmt.api.objects.OMSReplacer
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.uom.{Simplify, FlexaryConstantScala, Simplifiability, RecurseOnly}
import info.kwarc.mmt.moduleexpressions.operators.{Combinators, Common}

object Extends extends FlexaryConstantScala(Combinators._path, "extends") {
  /** the label of the distinguished node after extension */
  val nodeLabel = LocalName("pres")
  /** the label of the distinguished arrow after extension (from old to extended theory) */
  val arrowLabel = LocalName("extend")
}

// TODO all rules must preserve and reflect typing errors
// declaration merging must happen somewhere

object ComputeExtends extends ComputationRule(Extends.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = {
    val Extends(diag, wth@_*) = tm
    val ad = Common.asAnonymousDiagram(solver, diag).getOrElse {
      return RecurseOnly(List(1))
    }
    // dN : distinguished node of the input diagram
    val dN = ad.getDistNode.getOrElse {
      solver.error("distinguished node not found")
      return Simplifiability.NoRecurse
    }
    val old_decls = dN.theory.decls
    val old_names = old_decls.map(_.name)
    // Getting the new declarations as List[OML]
    val ext_decls = wth match {
      case OMLList(ds) =>
        // references to symbols in the extended theory were parsed as OMS's, need to be changed into OML's
        val trav = OMSReplacer { p =>
          if (old_names contains p.name) Some(OML(p.name)) else None
        }

        def tr(t: Term) = trav(t, stack.context)

        ds map { o =>
          val oT = OML(o.name, o.tp map tr, o.df map tr)
          oT
        }
      case _ => return RecurseOnly(List(2))
    }
    // creating the new AnonymousDiagram
    val new_decls = old_decls ::: ext_decls
    val new_dN = DiagramNode(Extends.nodeLabel, new AnonymousTheory(dN.theory.mt, new_decls))
    val extM = new AnonymousMorphism(Nil)
    val extA = DiagramArrow(Extends.arrowLabel, dN.label, new_dN.label, extM, true)
    val adP = Common.prefixLabels(ad, Extends.arrowLabel)
    val result = new AnonymousDiagram(adP.nodes ::: List(new_dN), adP.arrows ::: List(extA), Some(Extends.nodeLabel))
    Simplify(result.toTerm)
  }
}
