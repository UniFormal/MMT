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
    val fromPath = fromTerm match {
      case OMMOD(p) => p
      case _ => return None
    }
    val morAnon = new AnonymousMorphism(Nil)
    // replaces toTerm-OMS's in mor with to-OMLs
    val trav = OMSReplacer {p =>
      if (to.isDeclared(p.name)) Some(OML(p.name)) else None
    }
    from.decls.foreach {oml =>
      solver.lookup.getO(mor, ComplexStep(fromPath)/oml.name) foreach {
        case c: Constant => c.df foreach {df =>
          val dfT = trav(df, Context.empty)
          morAnon add OML(oml.name, None, Some(dfT))
        }
        case se =>
          solver.error("unknown assignment " + se.path)
      }
    }
    Some(morAnon)
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
  /* Applying a rename function to on OML */
  def applyRenameFunc (decls : List[OML], renames : List[(LocalName,LocalName)]): List[OML] =
    decls.map(
      d => d match {
        case OML(label,tp,df,nt,feature) =>
          val rens = renames.filter(r => if(r._1.equals(label)) true else false)
          if(rens.isEmpty) d
          else (new OML(rens.last._2,tp,df,nt,feature))
      })

  def asRenameFunc(r : List[Term]) : List[(LocalName,LocalName)] = r.map {
    case Rename1(OML(old,None,None,_,_), OML(nw,None,None,_,_)) => (old,nw)
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
    val result = new AnonymousDiagram(ad.nodes ::: List(new_dN), ad.arrows ::: List(extA), Some(Extends.nodeLabel))
    Simplify(result.toTerm)
  }
}

object Rename extends FlexaryConstantScala(Combinators._path, "rename") {
  /** the label of the renamed theory */
  val nodeLabel = LocalName("pres")
  /** the label of the renaming morphism (from old to renamed) */
  val arrowLabel = LocalName("rename")
}

object Rename1 extends BinaryConstantScala(Combinators._path, "rename1")

object ComputeRename extends ComputationRule(Rename.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = {
    val Rename(diag,rens@_*) = tm
    val ad = Common.asAnonymousDiagram(solver, diag).getOrElse {return RecurseOnly(List(1))}
    // perform the renaming
    val oldNew: List[(LocalName,LocalName)] = rens.toList.mapOrSkip {
      case Rename1(OML(old,None,None,_,_), OML(nw,None,None,_,_)) => (old,nw)
      case r =>
        solver.error("not a renaming " + r)
        throw SkipThis
    }
    val dN = ad.getDistNode.getOrElse {
      solver.error("distinguished node not found")
      return Simplifiability.NoRecurse
    }
    val atR = dN.theory.copy
    atR.rename(oldNew:_*)
    val dNR = DiagramNode(Rename.nodeLabel, atR)
    val renM = new AnonymousMorphism(oldNew map {case (o,n) => OML(o, None, Some(OML(o)))})
    val renA = DiagramArrow(Rename.arrowLabel, dN.label, dNR.label, renM, true)
    val result = new AnonymousDiagram(ad.nodes ::: List(dNR),ad.arrows ::: List(renA), Some(Rename.nodeLabel))
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

/* Have a common ComputePushout that contains methods common between Combine and Translate(Mixin) */

trait Pushout extends ConstantScala {
  val parent = Combinators._path

  def apply(d1: Term, r1: List[Term], d2: Term, r2: List[Term]) = {
    path(d1 :: r1 ::: List(d2) ::: r2)
  }

  def unapply(t: Term): Option[(Term,List[Term],Term,List[Term])] = t match {
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
      if (left.nonEmpty) return None
      Some((d1,r1,d2,r2))
    case _ => None
  }
}

object PushoutUtils {
  case class BranchInfo(anondiag: AnonymousDiagram, dom: LocalName, distNode: DiagramNode,
                        distTo: List[DiagramArrow], renames: List[(LocalName,LocalName)]) {
    def extend(po: DiagramNode) = {
      val morph = new AnonymousMorphism(po.theory.decls.diff(distNode.theory.decls))
      DiagramArrow(Combine.arrowLabel1, distNode.label, Combine.nodeLabel, morph, false)
    }
  }
  def collectBranchInfo(solver: CheckingCallback,d: Term,rename : List[Term])(implicit stack: Stack, history: History): Option[BranchInfo] = {
    val ren = Common.asRenameFunc(rename)
    val ad = Common.asAnonymousDiagram(solver, d).getOrElse(return None)
    val dom = ad.getDistArrow.getOrElse(return None).from
    val distNode = ad.getDistNode.getOrElse(return None)
    val distTo = ad.getDistArrowsTo(distNode.label)
    Some(PushoutUtils.BranchInfo(ad,dom,distNode,distTo,ren))
  }
}

object Combine extends Pushout {
  val name = "combine"

  val nodeLabel = LocalName("pres")
  val arrowLabel1 = LocalName("extend1")
  val arrowLabel2 = LocalName("extend2")
  val arrowLabel = LocalName("diag")

}

object ComputeCombine extends ComputationRule(Combine.path) {
  
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = {
    val Combine(d1,r1,d2,r2) = tm

    val b1 : BranchInfo = PushoutUtils.collectBranchInfo(solver,d1,r1).getOrElse(return Recurse)
    val b2 : BranchInfo = PushoutUtils.collectBranchInfo(solver,d2,r2).getOrElse(return Recurse)
    
    /* Finding the common node (\Gamma) */
    val nearestCommonSource : LocalName = (b1.distTo.map(_.from) intersect b2.distTo.map(_.from)).headOption.getOrElse(return Recurse)
    val source : DiagramNode = nearestCommonSource match {
      case Common.ExistingName(s) => b1.anondiag.getNode(nearestCommonSource).get
      case _ => return Recurse
    }

    /* Getting the declarations after applying rename */
    val node1Decls : List[OML] = Common.applyRenameFunc(b1.distNode.theory.decls,b1.renames)
    val node2Decls : List[OML] = Common.applyRenameFunc(b2.distNode.theory.decls,b2.renames)
    val commonDecls : List[OML] = node1Decls.intersect(node2Decls)

    /* TODO: Check the guard */
    val result_node_decls : List[OML] = (node1Decls ::: node2Decls).distinct

    /* TODO: How to choose the meta-theory? We need to have a constraint that one of them contains the other? */
    val result_node = DiagramNode(Combine.nodeLabel, new AnonymousTheory(b1.distNode.theory.mt,result_node_decls))

    val arrow1 = DiagramArrow(Combine.arrowLabel,b1.distNode.label,result_node.label,new AnonymousMorphism(List.empty),false)
    val arrow2 = DiagramArrow(Combine.arrowLabel2,b2.distNode.label,result_node.label,new AnonymousMorphism(List.empty),false)
    val diag = DiagramArrow(Combine.arrowLabel,source.label,result_node.label,new AnonymousMorphism(List.empty),true)

    /* Next Problem: How to retrieve the arrows? */
    // so we have a source, and two distinguished nodes.. I need to define the extension relation between them
    // def getImplicit(from: Term, to: Term) : Option[Term]
    // val morphism = (solver.lookup.getImplicit(source.toTerm,ad1.getDistNode.get.toTerm))

    val arrows = (b1.anondiag.arrows ::: b2.anondiag.arrows).distinct
    val nodes = (b1.anondiag.nodes ::: b2.anondiag.nodes).distinct
    /* val List(dom1,dom2) = List(ad1,ad2) map {d =>
      val fromT = d.getDistArrow.getOrElse(return Recurse).from
      fromT match {
        case Common.ExistingName(p) => p
        case _ => return Recurse
      }
    }*/

    val ad = new AnonymousDiagram(nodes ::: List(result_node), arrows:::List(b1.extend(result_node),b2.extend(result_node),diag), Some(result_node.label))
    Simplify(ad.toTerm)
  }
}

object Translate extends Pushout {
  val name = "translate"

  val nodeLabel = LocalName("pres")
  val arrowLabel1 = LocalName("extend")
  val arrowLabel2 = LocalName("view")
  val arrowLabel = LocalName("diag")
}
/**
 * Translate(m,T) and Expand(m,T) form a pushout along an inclusion as follows:
 *
 * m : A -> B
 * inclusion from A to T
 * Expand(m,T): T -> Translate(m,T)
 * inclusion from B to Translate(m,T)
 */


object ComputeTranslate extends ComputationRule(Translate.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = {
    /* Both combine and translate takes two diagrams and two renames. The difference is in the content of the second diagram */
    val Translate(d1, r1, d2, r2) = tm
    val List(ad1,ad2) = List(d1,d2).map{d => Common.asAnonymousDiagram(solver, d).getOrElse {return RecurseOnly(List(1))}}
    val List(ren1,ren2) = List(r1,r2).map{r => Common.asRenameFunc(r)}



    /* From the first diagram we get the inclusion arrow, apply ren1 to it */
    val targetNodeDecls : List[OML] = Common.applyRenameFunc(ad1.getDistNode.getOrElse(return Recurse).theory.decls,ren1)
    val codAnon = new AnonymousTheory(ad1.getDistNode.getOrElse(return Recurse).theory.mt,targetNodeDecls)
    val domAnon = ad1.getNode(ad1.getDistArrow.getOrElse(return Recurse).from).getOrElse(return Recurse).theory
    // val sourceNodeDecls : List[OML] = Common.applyRenameFunc(ad1.getNode(ad1.getDistArrow.getOrElse(return Recurse).from).getOrElse(return Recurse).theory.decls,ren1)

    /* The input mor as DiagramArrow */
    val arrow2 : DiagramArrow = ad2.getDistArrow.getOrElse(return Recurse)
    /* From the second diagram, we get the view, then apply the rename function on it */
    val morphDecls : List[OML] = Common.applyRenameFunc(arrow2.morphism.decls,ren2)
    val morAnon = new AnonymousMorphism(morphDecls)



    /*   val dom = Morph.domain(mor)(solver.lookup).getOrElse{return Recurse}
    val cod = Morph.codomain(mor)(solver.lookup).getOrElse{return Recurse}
    val List(thyAnon,domAnon,codAnon) = List(???,dom,cod).map {t => Common.asAnonymousTheory(solver, t).getOrElse(return Recurse)} */

    // Common.asAnonymousMorphism(solver, dom, domAnon, cod, codAnon,AnonymousMorphismCombinator(morphDecls)).getOrElse(return Recurse)
    // translate all declarations of thy that are not from dom via mor and add them to cod
    val morAsSub = morAnon.decls.flatMap {oml => oml.df.toList.map {d => Sub(oml.name, d)}}
    val translator = OMLReplacer(morAsSub)
    val pushout = codAnon
    morAnon.decls.foreach {oml =>
        if (! domAnon.isDeclared(oml.name)) {
          if (codAnon.isDeclared(oml.name)) {
            solver.error("pushout not defined because of name clash: " + oml.name)
            return Recurse
          }
          val omlT = translator(oml, stack.context).asInstanceOf[OML]
          pushout.add(omlT)
        }
    }
    Simplify(pushout.toTerm)
  }
}

// TODO better name
/** see [[Translate]] */
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
