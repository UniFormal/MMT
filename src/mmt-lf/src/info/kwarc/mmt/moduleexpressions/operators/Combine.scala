/**
  * Combine, Pushout, Mixin and related diagram operators.
  */

package info.kwarc.mmt.moduleexpressions.operators

import info.kwarc.mmt.api.LocalName
import info.kwarc.mmt.api.checking.{CheckingCallback, ComputationRule, History}
import info.kwarc.mmt.api.objects.OMLReplacer
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.uom._

/**
  * apply/unapply methods for terms of the form
  * Combine(diagram1, List(Rename1(old1,new1),...), diagram2, List(Rename1(old2,new2),...))
  */

/* Have a common Pushout that contains methods common between Combine and Mixin */

trait Pushout extends ConstantScala {
  val parent = Combinators._path

  def apply(d1: Term, r1: List[Term], d2: Term, r2: List[Term], over: Term) = {
    path(d1 :: r1 ::: List(d2) ::: r2 ::: List(over))
  }

  def unapply(t: Term): Option[(Term, List[Term], Term, List[Term], Term)] = t match {
    case OMA(OMS(this.path), args) =>
      var left = args
      val d1 = left.headOption.getOrElse(return None)
      left = left.tail
      val r1 = left.takeWhile(t => Rename1.unapply(t).isDefined)
      left = left.drop(r1.length)
      val d2 = left.headOption.getOrElse(return None)
      left = left.tail
      val r2 = left.takeWhile(t => Rename1.unapply(t).isDefined)
      left = left.drop(r2.length)
      val over = left.headOption.getOrElse(return None)
      left = left.tail
      if (left.nonEmpty) return None
      Some((d1, r1, d2, r2, over))
    case _ => None
  }
}

object PushoutUtils {

  case class BranchInfo(anondiag: AnonymousDiagram, distNode: DiagramNode,
                        distTo: List[DiagramArrow], renames: List[(LocalName, Term)]) {
    def extend(label: LocalName, po: DiagramNode) = {
      //val morph = new AnonymousMorphism(po.theory.decls.diff(distNode.theory.decls))
      val maps = renames.map { case (o, n) => OML(o, None, Some(n)) }
      DiagramArrow(label, distNode.label, po.label, new AnonymousMorphism(maps), false)
    }
  }

  def collectBranchInfo(solver: CheckingCallback, d: Term, rename: List[Term])(implicit stack: Stack, history: History): Option[BranchInfo] = {
    val ren = Common.asSubstitution(rename)
    val ad = Common.asAnonymousDiagram(solver, d).getOrElse(return None)
    val distNode = ad.getDistNode.getOrElse(return None)
    val distTo = ad.getDistArrowsTo(distNode.label)
    Some(PushoutUtils.BranchInfo(ad, distNode, distTo, ren))
  }
}

object Combine extends Pushout {
  val name = "combine"
  val nodeLabel = LocalName("pres")
  val arrowLabel1 = LocalName("extend1")
  val arrowLabel2 = LocalName("extend2")
  val arrowLabel = LocalName("extend")

}

object ComputeCombine extends ComputationRule(Combine.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = {
    /** ************** Input pieces *****************/
    val Combine(d1, r1, d2, r2, over) = tm
    val List(ad1, ad2, ad_over) = List(d1, d2, over).map(d => Common.asAnonymousDiagram(solver, d).get) // TODO: Handle errors here

    /** ************** Calculate the views from the source to the two nodes (after renaming) ****************/
    val List(renames1, renames2): List[List[OML]] = List(r1, r2).map { r => Common.asSubstitution(r).map { case (o, n) => OML(o, None, Some(n)) } }
    val List(in_view1, in_view2): List[List[OML]] = List(ad1, ad2).map { d => d.viewOf(ad_over.getDistNode.get, d.getDistNode.get) } // TODO: Handle errors here
    val view1: List[OML] = (AnonymousMorphism(in_view1) compose AnonymousMorphism(renames1)).decls
    val view2: List[OML] = (AnonymousMorphism(in_view2) compose AnonymousMorphism(renames2)).decls

    /** ************** Check The Guard *******************/
    /* - Now, view1 and view2 have the list of assignments from the source to the targets (after applying the renames)
     * - We compare them to make sure the names matches
     * - Order does not matter, so we convert the list to a set.
     *  */
    // if (view1.toSet != view2.toSet) throw (new GeneralError("Wrong renames"))

    /** ************** Define the new theory ***************/
    // apply the rename
    def view_as_sub(v: List[OML]) = v.map { case OML(o, _, Some(n), _, _) => (o, n) }

    val List(renThry1, renThry2): List[AnonymousTheory] = List(ad1, ad2).map(b => b.getDistNode.get.theory.rename(view_as_sub(view1): _*))

    /* Calculate the pushout as distinct union
     * TODO: Find a better way to choose the meta-theory
     * */
    val new_decls: List[OML] = (renThry1.decls concat renThry2.decls).distinct
    val new_theory: AnonymousTheory = new AnonymousTheory(ad1.getDistNode.get.theory.mt, new_decls)

    /** **************** Build the new diagram *******************/
    val dist_node: DiagramNode = DiagramNode(Combine.nodeLabel, new_theory)
    // TODO: The assignments in the arrow need to be the identity
    val impl_arrow = DiagramArrow(Combine.arrowLabel, ad_over.distNode.get, dist_node.label, new AnonymousMorphism(Nil), true)

    /* This is used in case theories are not built in a tiny-theories way. In this case, we need to generate names for the arrows. */
    val jointDiag: AnonymousDiagram = (Common.prefixLabels(ad1, LocalName("left")) ++ Common.prefixLabels(ad2, LocalName("right"))).get
    // the [[Option]] above should always be defined due to prefixing which
    // prevents name clashes

    val List(map1, map2) = List(view1, view2).map { v => Common.asSubstitution(v).map { case (o, n) => OML(o, None, Some(n)) } }

    val arrow1 = DiagramArrow(Combine.arrowLabel1, ad1.distNode.get, dist_node.label, new AnonymousMorphism(map1), false)
    val arrow2 = DiagramArrow(Combine.arrowLabel2, ad2.distNode.get, dist_node.label, new AnonymousMorphism(map2), false)

    val result_diag = new AnonymousDiagram(jointDiag.nodes ::: List(dist_node), jointDiag.arrows ::: List(arrow1, arrow2, impl_arrow), Some(dist_node.label))

    Simplify(result_diag.toTerm)
  }
}

object Mixin extends Pushout {
  val name = "mixin"
  val nodeLabel = LocalName("pres")
  val arrowLabel1 = LocalName("extend1")
  val arrowLabel2 = LocalName("view")
  val arrowLabel = LocalName("extend")
}

/**
  * Translate(m,T) and Expand(m,T) form a pushout along an inclusion as follows:
  *
  * m : A -> B
  * inclusion from A to T
  * Expand(m,T): T -> Translate(m,T)
  * inclusion from B to Translate(m,T)
  */

object ComputeMixin extends ComputationRule(Mixin.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = {
    /** ************** Input pieces *****************/
    val Mixin(d1, r1, d2, r2, over) = tm
    // TODO @Yasmine: I think it should be RecurseOnly(List(1,2,3))
    val List(ad1, ad2, ad_over) = List(d1, d2, over).map(d => Common.asAnonymousDiagram(solver, d).getOrElse(return RecurseOnly(List(1)))) // TODO: Handle errors here

    /** ************** Calculate the views from the source to the two nodes (after renaming) ****************/
    /*
    val List(renames1,renames2) : List[List[OML]] = List(r1,r2).map{r => Common.asSubstitution(r).map{case (o,n) => OML(o,None,Some(n))}}
    val List(in_view1,in_view2) : List[List[OML]] = List(ad1,ad2).map{d => d.viewOf(ad_over.getDistNode.get, d.getDistNode.get)} // TODO: Handle errors here
    val view1: List[OML] = ad1.compose(in_view1,renames1)
    val view2: List[OML] = ad2.compose(in_view2,renames2)
    */

    val List(ren1, ren2) = List(r1, r2).map { r => Common.asSubstitution(r) }
    // val anonMor = Common.asAnonymousMorphism(solver,ad2.getDistArrow.getOrElse(return Recurse).from)

    /* From the first diagram we get the inclusion arrow, apply ren1 to it */
    val d1_dN = ad1.getDistNode.getOrElse(return Recurse)
    val d1_dN_renamed = d1_dN.theory.rename(ren1: _*)
    val d2_dN = ad2.getDistNode.getOrElse(return Recurse)
    val d2_dN_renamed = d2_dN.theory.rename(ren2: _*)

    /* Get the view from the second diagram */
    val view: DiagramArrow = ad2.getDistArrow.getOrElse(return Recurse)
    val view_renamed: AnonymousMorphism = new AnonymousMorphism(Common.applySubstitution(view.morphism.decls, ren2))
    val view_from: DiagramNode = ad2.getNode(view.from).getOrElse(return Recurse)

    /** TODO: THis part needs to be CHANGED */
    val mor = view.morphism.decls.map { oml => (LocalName(oml.name), oml.df.getOrElse(return Recurse)) }
    val morAsSub = view.morphism.decls.flatMap { oml => oml.df.toList.map { d => Sub(oml.name, d) } }
    val translator = OMLReplacer(morAsSub)

    val new_decls: List[OML] = Common.applySubstitution(d1_dN_renamed.decls, mor)
    val pushout = new AnonymousTheory(d1_dN.theory.mt, new_decls)

    val node = DiagramNode(Mixin.nodeLabel, pushout)
    // TODO: The morphisms needs more thinking
    val arrow1 = DiagramArrow(Mixin.arrowLabel1, d1_dN.label, node.label, view.morphism, false)
    val arrow2 = DiagramArrow(Mixin.arrowLabel2, d2_dN.label, node.label, new AnonymousMorphism(Nil), false)
    val dA = DiagramArrow(Mixin.arrowLabel, view.from, node.label, view.morphism, true)
    val result = new AnonymousDiagram(ad1.nodes ::: ad2.nodes ::: List(node), ad1.arrows ::: ad2.arrows ::: List(arrow1, arrow2, dA), Some(node.label))
    Simplify(result.toTerm)
  }
}

// TODO better name
/** see [[Mixin]] */
object Expand extends BinaryConstantScala(Combinators._path, "expand")

// TODO does not work yet
object ComputeExpand extends ComputationRule(Expand.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) = {
    val Expand(mor, thy) = tm
    thy match {
      case AnonymousTheoryCombinator(at) =>
        val res = new AnonymousTheory(at.mt, Nil) //TODO this should be an AnonymousMorphism; same as AnonymousTheory but no dependency
        // add include of mor
        at.decls.foreach { case OML(n, t, d, _, _) =>
          // skip all includes of theories that are already include in domain of mor
          val ass = OML(n, None, Some(OML(n, None, None)))
          res.add(ass)
        }
        Simplify(res.toTerm)
      case _ => Recurse
    }
  }
}