package info.kwarc.mmt.moduleexpressions

import info.kwarc.mmt.api.{uom, _}
import checking._
import info.kwarc.mmt.api.frontend.{Controller, ExtensionManager, Report}
import info.kwarc.mmt.api.libraries.{Library, ThinGeneratedCategory}
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.symbols._
import objects._
import utils._
import uom._
import info.kwarc.mmt.lf._
import info.kwarc.mmt.moduleexpressions.Combine.path
import info.kwarc.mmt.moduleexpressions.PushoutUtils.BranchInfo

import scala.collection.mutable.HashSet

object Combinators {
  val _path = ModExp._base ? "Combinators"
}

object Common {
  /** apply/unapply functions so that ExistingName(p) is the label of a module with URI p */
  object ExistingName {
    def apply(p: MPath) = LocalName(p)
    def unapply(l: LocalName) = l.steps match {
      case List(ComplexStep(p)) => Some(p)
      case _ => None
    }
  }

  /** turns a declared theory into an anonymous one by dropping all global qualifiers (only defined if names are still unique afterwards) */
  def anonymize(solver: CheckingCallback, namedTheory: Theory)(implicit stack: Stack, history: History): AnonymousTheory = {
    // collect included theories
    val includes = namedTheory.getIncludesWithoutMeta.flatMap {i => solver.lookup.getO(i) match {
      case Some(dt: Theory) =>
        List(dt)
      case Some(se) =>
        solver.error("ignoring include of " + se.path)
        Nil
      case None =>
        Nil
    }}
    // code for translating OMS's to OML references
    var names: List[GlobalName] = Nil
    val trav = OMSReplacer {p =>
      if (names contains p) Some(OML(p.name)) else None
    }
    def translate(tm: Term) = trav(tm, stack.context)
    // turn all constants into OML's
    val omls = (includes:::List(namedTheory)).flatMap {th =>
      th.getDeclarationsElaborated.flatMap {
        case c: Constant =>
          val cT = OML(c.name,  c.tp map translate, c.df map translate, c.not)
          if (names.exists(p => p.name == c.name))
            solver.error("theory has duplicate name: " + c.name)
          names ::= c.path
          List(cT)
        case _ => Nil
      }
    }
    //val real = RealizeOML(namedTheory.path, None) // the theorem that the anonymous theory realizes namedTheory
    new AnonymousTheory(namedTheory.meta, omls)
  }

  /** provides the base case of the function that elaborates a theory expression (in the form of an [[AnonymousTheory]]) */
  def asAnonymousTheory(solver: CheckingCallback, thy: Term)(implicit stack: Stack, history: History): Option[AnonymousTheory] = {
    thy match {
      // named theories
      case OMMOD(p) =>
        solver.lookup.getO(p) match {
          case Some(th: Theory) =>
            lazy val default = anonymize(solver, th)
            th.dfC.normalize(d => solver.simplify(d)) // make sure a normalization value is cached
            val at = th.dfC.normalized match {
              case Some(df) =>
                df match {
                  case AnonymousTheoryCombinator(at) => at
                  case _ => default
                }
              case None => default
            }
            Some(at)
          case Some(dm:DerivedModule) if dm.feature == DiagramDefinition.feature =>
            dm.dfC.normalized flatMap {
              case AnonymousDiagramCombinator(ad) =>
                ad.getDistNode map { n => n.theory }
              case _ => None
            }
          case Some(_) =>
            solver.error("not a theory: " + p)
            None
          case None =>
            solver.error("unknown name: " + p)
            None
        }
      // explicit anonymous theories
      case AnonymousTheoryCombinator(at) => Some(at)
      case _ => None
    }
  }

  /** like asAnonymousTheory but for morphisms */
  def asAnonymousMorphism(solver: CheckingCallback, fromTerm: Term, from: AnonymousTheory,
                          toTerm: Term, to: AnonymousTheory, mor: Term)(implicit stack: Stack, history: History): Option[AnonymousMorphism] = {
     mor match{
       case OMMOD(p) =>
         solver.lookup.getO(p) match {
           case Some(m : View) =>
             m.dfC.normalize(d => solver.simplify(d)) // make sure a normalization value is cached
             val at = m.dfC.normalized match {
               case Some(df) =>
                 df match {
                   case AnonymousMorphismCombinator(at) => at
                     // TODO: handle stuff here !!
                 }
             }
             Some(at)
         }
       case AnonymousMorphismCombinator(at) => Some(at) // explicit anonymous theories
       case _ => None
     }
  }

  /** provides the base case of the function that elaborates a diagram expression (in the form of an [[AnonymousDiagram]]) */
  def asAnonymousDiagram(solver: CheckingCallback, diag: Term)(implicit stack: Stack, history: History): Option[AnonymousDiagram] = {
    diag match {
      // named diagrams
      case OMMOD(p) =>
        solver.lookup.getO(p) match {
          case Some(dm: DerivedModule) if dm.feature == DiagramDefinition.feature =>
            dm.dfC.normalized flatMap {
              case AnonymousDiagramCombinator(ad) => Some(ad)
              case _ => None
            }
          case Some(thy: Theory) =>
            // the theory as a one-node diagram
            val anonThy = anonymize(solver, thy)
            val label = ExistingName(thy.path)
            val anonThyN = DiagramNode(label, anonThy)
            val ad = new AnonymousDiagram(List(anonThyN), Nil, Some(label))
            Some(ad)
          case Some(vw: View) =>
            // the view as a one-edge diagram
            val from = asAnonymousTheory(solver, vw.from).getOrElse(return None)
            val to   = asAnonymousTheory(solver, vw.to).getOrElse(return None)
            val mor  = asAnonymousMorphism(solver, vw.from, from, vw.to, to, vw.toTerm).getOrElse(return None)
            val label = ExistingName(vw.path)
            // TODO this only makes sense if domain and codomain are named theories; otherwise, we should maybe copy the whole diagram
            val fromL = LocalName(vw.from.toMPath)
            val toL   = LocalName(vw.to.toMPath)
            val fromN = DiagramNode(fromL, from)
            val toN   = DiagramNode(toL, to)
            val arrow = DiagramArrow(label, fromL, toL, mor, vw.isImplicit)
            val ad = new AnonymousDiagram(List(fromN,toN), List(arrow), Some(toL))
            Some(ad)
          case _ => return None
        }
      // explicit anonymous diagrams
      case AnonymousDiagramCombinator(ad) => Some(ad)
      case _ => None
    }
  }

  def prefixLabels(ad: AnonymousDiagram, prefix: LocalName) = {
    def f(l: LocalName) = {
      l match {
        case ExistingName(_) => l
        case _ => prefix / l
      }
    }
    ad.relabel(f)
  }

  /* Applying a substitution function to on OML */
  def applySubstitution (decls : List[OML], renames : List[(LocalName,Term)]): List[OML] =
    decls.map(
      d => d match {
        case OML(label,tp,df,nt,feature) =>
          val rens = renames.filter(r => if(r._1.equals(label)) true else false)
          if(rens.isEmpty) d
          else (new OML(rens.last._2.asInstanceOf[OML].name,tp,df,nt,feature))
      })

  def asSubstitution(r : List[Term]) : List[(LocalName,Term)] = r.map {
    case Rename1(OML(old,None,None,_,_), nw) => (old,nw)
    case _ => return Nil
  }
}

/** operator for the empty theory of a given meta-theory */
object Empty extends UnaryConstantScala(Combinators._path, "empty") {
  val label = LocalName("empty")
}

/** Empty(p) ---> p{} */
object ComputeEmpty extends ComputationRule(Extends.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = {
    val mt = tm match {
      case Empty(OMMOD(p)) => Some(p)
      case OMS(Empty.path) => None
      case _ => return Recurse
    }
    val thy = new AnonymousTheory(mt, Nil)
    val dn = DiagramNode(Empty.label, thy)
    val ad = new AnonymousDiagram(List(dn), Nil, Some(dn.label))
    Simplify(ad.toTerm)
  }
}

/* The rules below compute the results of theory combinators.
 * Each rule is applicable if the arguments have been computed already.
 *
 * The rules also throw typing errors if they encounter any.
 * Open question: Should they be required to find all errors? Maybe only all structural errors?
 */

object Extends extends FlexaryConstantScala(Combinators._path, "extends"){
  /** the label of the distinguished node after extension */
  val nodeLabel = LocalName("pres")
  /** the label of the distinguished arrow after extension (from old to extended theory) */
  val arrowLabel = LocalName("extend")
}

// TODO all rules must preserve and reflect typing errors
// declaration merging must happen somewhere

object ComputeExtends extends ComputationRule(Extends.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = {
    val Extends(diag,wth@_*) = tm
    val ad = Common.asAnonymousDiagram(solver, diag).getOrElse {return RecurseOnly(List(1))}
    // Getting the new declarations as List[OML]
    val extD = wth match {
      case OMLList(extDecl) => extDecl
      case _ => return RecurseOnly(List(2))
    }
    // dN : distinguished node of the input diagram
    val dN = ad.getDistNode.getOrElse {
      solver.error("distinguished node not found")
      return Simplifiability.NoRecurse
    }
    // creating the new AnonymousDiagram
    val new_decls = dN.theory.decls ::: extD
    val new_dN = DiagramNode(Extends.nodeLabel, new AnonymousTheory(dN.theory.mt,new_decls))
    val extM = new AnonymousMorphism(Nil)
    val extA = DiagramArrow(Extends.arrowLabel, dN.label, new_dN.label, extM, true)
    val adP = Common.prefixLabels(ad, Extends.arrowLabel)
    val result = new AnonymousDiagram(adP.nodes ::: List(new_dN), adP.arrows ::: List(extA), Some(Extends.nodeLabel))
    Simplify(result.toTerm)
  }
}

object Rename extends FlexaryConstantScala(Combinators._path, "rename") {
  /** the label of the renamed theory */
  val nodeLabel = LocalName("pres")
  /** the label of the renaming morphism (from old to renamed) */
  val arrowLabel = LocalName("rename")

  def pairToTerm(on: (LocalName,LocalName)) = OML(on._1, None, Some(OML(on._2)))
  def pairsToMorph(on: List[(LocalName,LocalName)]) = new AnonymousMorphism(on map pairToTerm)
}

object Rename1 extends BinaryConstantScala(Combinators._path, "rename1")

object ComputeRename extends ComputationRule(Rename.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = {
    val Rename(diag,rens@_*) = tm
    val ad = Common.asAnonymousDiagram(solver, diag).getOrElse {return RecurseOnly(List(1))}
    // perform the renaming
    val oldNew: List[(LocalName,Term)] = rens.toList.mapOrSkip {
      case Rename1(OML(old,None,None,_,_), nw) => (old,nw)
      case r =>
        solver.error("not a renaming " + r)
        throw SkipThis
    }
    val dN = ad.getDistNode.getOrElse {
      solver.error("distinguished node not found")
      return Simplifiability.NoRecurse
    }
    val atR = dN.theory.rename(oldNew:_*)
    val dNR = DiagramNode(Rename.nodeLabel, atR)
    val renM = new AnonymousMorphism(oldNew map {case (o,n) => OML(o, None, Some(n))})
    val renA = DiagramArrow(Rename.arrowLabel, dN.label, dNR.label, renM, true)
    val adP = Common.prefixLabels(ad, Rename.arrowLabel)
    val result = new AnonymousDiagram(adP.nodes ::: List(dNR),adP.arrows ::: List(renA), Some(Rename.nodeLabel))
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

/**
 * apply/unapply methods for terms of the form
 * Combine(diagram1, List(Rename1(old1,new1),...), diagram2, List(Rename1(old2,new2),...))
 */

/* Have a common Pushout that contains methods common between Combine and Mixin */

trait Pushout extends ConstantScala {
  val parent = Combinators._path

  def apply(d1: Term, r1: List[Term], d2: Term, r2: List[Term], over : Term) = {
    path(d1 :: r1 ::: List(d2) ::: r2 ::: List(over))
  }

  def unapply(t: Term): Option[(Term,List[Term],Term,List[Term],Term)] = t match {
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
      Some((d1,r1,d2,r2,over))
    case _ => None
  }
}

object PushoutUtils {
  case class BranchInfo(anondiag: AnonymousDiagram, distNode: DiagramNode,
                        distTo: List[DiagramArrow], renames: List[(LocalName,Term)]) {
    def extend(label: LocalName, po: DiagramNode) = {
      //val morph = new AnonymousMorphism(po.theory.decls.diff(distNode.theory.decls))
      val maps = renames.map {case (o,n) => OML(o, None, Some(n))}
      DiagramArrow(label, distNode.label, po.label, new AnonymousMorphism(maps), false)
    }
  }
  def collectBranchInfo(solver: CheckingCallback,d: Term,rename : List[Term])(implicit stack: Stack, history: History): Option[BranchInfo] = {
    val ren = Common.asSubstitution(rename)
    val ad = Common.asAnonymousDiagram(solver, d).getOrElse(return None)
    val distNode = ad.getDistNode.getOrElse(return None)
    val distTo = ad.getDistArrowsTo(distNode.label)
    Some(PushoutUtils.BranchInfo(ad,distNode,distTo,ren))
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
    /**************** Input pieces *****************/
    val Combine(d1, r1, d2, r2, over) = tm
    val List(ad1, ad2, ad_over) = List(d1, d2, over).map(d => Common.asAnonymousDiagram(solver, d).get) // TODO: Handle errors here

    /**************** Calculate the views from the source to the two nodes (after renaming) ****************/
    val List(renames1,renames2) : List[List[OML]] = List(r1,r2).map{r => Common.asSubstitution(r).map{case (o,n) => OML(o,None,Some(n))}}
    val List(in_view1,in_view2) : List[List[OML]] = List(ad1,ad2).map{d => d.viewOf(ad_over.getDistNode.get, d.getDistNode.get)} // TODO: Handle errors here
    val view1: List[OML] = ad1.compose(in_view1,renames1)
    val view2: List[OML] = ad2.compose(in_view2,renames2)

    /**************** Check The Guard *******************/
    /* - Now, view1 and view2 have the list of assignments from the source to the targets (after applying the renames)
     * - We compare them to make sure the names matches
     * - Order does not matter, so we convert the list to a set.
     *  */
    if (view1.toSet != view2.toSet) throw (new GeneralError("Wrong renames"))

    /**************** Define the new theory ***************/
    // apply the rename
    def view_as_sub(v : List[OML]) = v.map{case OML(o,_,Some(n),_,_) => (o,n)}
    val List(renThry1,renThry2) : List[AnonymousTheory] = List(ad1,ad2).map(b => b.getDistNode.get.theory.rename(view_as_sub(view1):_*))

    /* Calculate the pushout as distinct union
     * TODO: Find a better way to choose the meta-theory
     * */
    val new_decls : List[OML] = (renThry1.decls union renThry2.decls).distinct
    val new_theory : AnonymousTheory = new AnonymousTheory(ad1.getDistNode.get.theory.mt,new_decls)

    /****************** Build the new diagram *******************/
    val dist_node: DiagramNode = DiagramNode(Combine.nodeLabel, new_theory)
    // TODO: The assignments in the arrow need to be the identity
    val impl_arrow = DiagramArrow(Combine.arrowLabel, ad_over.distNode.get, dist_node.label, new AnonymousMorphism(Nil), true)

    /* This is used in case theories are not built in a tiny-theories way. In this case, we need to generate names for the arrows. */
    val jointDiag: AnonymousDiagram = (Common.prefixLabels(ad1, LocalName("left")) union Common.prefixLabels(ad2, LocalName("right")))

    val List(map1,map2) = List(view1,view2).map{v => Common.asSubstitution(v).map { case (o, n) => OML(o, None, Some(n))}}

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

/*
object ComputeMixin extends ComputationRule(Mixin.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = {
    /* Both combine and translate takes two diagrams and two renames. The difference is in the content of the second diagram */
    val Mixin(d1, r1, d2, r2) = tm
    val List(ad1,ad2) = List(d1,d2).map{d => Common.asAnonymousDiagram(solver, d).getOrElse{return RecurseOnly(List(1))}}
    val List(ren1,ren2) = List(r1,r2).map{r => Common.asSubstitution(r)}
    // val anonMor = Common.asAnonymousMorphism(solver,ad2.getDistArrow.getOrElse(return Recurse).from)

    /* From the first diagram we get the inclusion arrow, apply ren1 to it */
    val d1_dN = ad1.getDistNode.getOrElse(return Recurse)
    val d1_dN_renamed = d1_dN.theory.rename(ren1:_*)
    val d2_dN = ad2.getDistNode.getOrElse(return Recurse)
    val d2_dN_renamed = d2_dN.theory.rename(ren2:_*)

    /* Get the view from the second diagram */
    val view : DiagramArrow = ad2.getDistArrow.getOrElse(return Recurse)
    val view_renamed :  AnonymousMorphism = new AnonymousMorphism(Common.applySubstitution(view.morphism.decls,ren2))
    val view_from : DiagramNode = ad2.getNode(view.from).getOrElse(return Recurse)

    /** TODO: THis part needs to be CHANGED */
    val mor = view.morphism.decls.map {oml => (LocalName(oml.name),oml.df.getOrElse(return Recurse))}
    val morAsSub = view.morphism.decls.flatMap{oml => oml.df.toList.map {d => Sub(oml.name, d)}}
    val translator = OMLReplacer(morAsSub)

    val new_decls : List[OML] = Common.applySubstitution(d1_dN_renamed.decls,mor)
    val pushout = new AnonymousTheory(d1_dN.theory.mt,new_decls)

    val node = DiagramNode(Mixin.nodeLabel,pushout)
    // TODO: The morphisms needs more thinking
    val arrow1 = DiagramArrow(Mixin.arrowLabel1,d1_dN.label,node.label,view.morphism,false)
    val arrow2 = DiagramArrow(Mixin.arrowLabel2,d2_dN.label,node.label,new AnonymousMorphism(Nil),false)
    val dA = DiagramArrow(Mixin.arrowLabel,view.from,node.label,view.morphism,true)
    val result = new AnonymousDiagram(ad1.nodes:::ad2.nodes:::List(node),ad1.arrows:::ad2.arrows:::List(arrow1,arrow2,dA),Some(node.label))
    Simplify(result.toTerm)
  }
}
*/
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
          at.decls.foreach {case OML(n,t,d,_,_) =>
            // skip all includes of theories that are already include in domain of mor
            val ass = OML(n,None,Some(OML(n,None,None)))
            res.add(ass)
          }
          Simplify(res.toTerm)
        case _ => Recurse
      }
   }
}
