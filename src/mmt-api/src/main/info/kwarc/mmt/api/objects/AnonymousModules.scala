package info.kwarc.mmt.api.objects

import info.kwarc.mmt.api._
import notations._
import utils._

/** auxiliary class for storing lists of declarations statefully without giving it a global name
 *
 * anonymous modules are object that can be converted into these helper classes using the objects [[AnonymousTheory]] and [[AnonymousMorphism]]
 */
trait AnonymousBody extends MutableElementContainer[OML] with DefaultLookup[OML] with DefaultMutability[OML] {
  var decls: List[OML]
  def getDeclarations = decls
  def setDeclarations(ds: List[OML]) {
    decls = ds
  }
  def toTerm: Term
  override def toString = getDeclarations.mkString("{", ", ", "}")
}

/** a theory given by meta-theory and body */
class AnonymousTheory(val mt: Option[MPath], var decls: List[OML]) extends AnonymousBody {
  def rename(oldNew: (LocalName,LocalName)*) = {
    val sub: Substitution = oldNew.toList map {case (old,nw) => Sub(old, OML(nw))}
    val trav = symbols.OMLReplacer(sub)
    val newDecls = decls map {oml =>
       // TODO this renaming is too aggressive if there is OML shadowing or if OMLs are used for other purposes
       val omlR = trav(oml, Context.empty).asInstanceOf[OML] // this traverser always maps OMLs to OMLs
       listmap(oldNew, oml.name) match {
         case Some(nw) => omlR.copy(name = nw)
         case None => omlR
       }
    }
    decls = newDecls
  }
  def copy = new AnonymousTheory(mt, decls)
  def toTerm = AnonymousTheoryCombinator(mt, decls)
  override def toString = {
    val m = mt.map(_.toString).getOrElse("")
    m + super.toString
  }
}

/** bridges between [[AnonymousTheory]] and [[Term]] */
object AnonymousTheoryCombinator {
  val path = ModExp.anonymoustheory

  def apply(mt: Option[MPath], decls: List[OML]) = OMA(OMS(path), mt.map(OMMOD(_)).toList:::decls)
  def unapply(t: Term): Option[AnonymousTheory] = {
    val at = t match {
      case OMA(OMS(this.path), OMMOD(mt)::OMLList(omls)) =>
        new AnonymousTheory(Some(mt), omls)
      case OMA(OMS(this.path), OMLList(omls)) =>
        new AnonymousTheory(None, omls)
      case _ => return None
    }
    Some(at)
  }
}

/** a morphism given by domain, codomain, and body */
class AnonymousMorphism(var decls: List[OML]) extends AnonymousBody {
  def toTerm = AnonymousMorphismCombinator(decls)
  def copy = new AnonymousMorphism(decls)
}

/** bridges between [[AnonymousMorphism]] and [[Term]] */
object AnonymousMorphismCombinator {
  val path = ModExp.anonymousmorphism

  def apply(decls: List[OML]) = OMA(OMS(path), decls)
  def unapply(t: Term): Option[AnonymousMorphism] = t match {
    case OMA(OMS(this.path), OMLList(omls)) =>
      val am = new AnonymousMorphism(omls)
      Some(am)
    case _ => None
  }
}

/** used in [[AnonymousDiagram]] */
sealed abstract class DiagramElement {
  def label: LocalName
  def toTerm: Term
}
/** a node in an [[AnonymousDiagram]]
 *  @param label the label of this node
 *  @param theory the theory attached to this node (the same theory may be attached to multiple nodes)
 */
case class DiagramNode(label: LocalName, theory: AnonymousTheory) extends DiagramElement {
  def toTerm = OML(label, Some(TheoryType(Nil)), Some(theory.toTerm))
  override def toString = s"$label:THEY=$theory"
}
/** an arrow in an [[AnonymousDiagram]]
 *  @param label the label of this arrow
 *  @param from the label of the domain node in the same diagram
 *  @param to the label of the codomain node in the same diagram
 *  @param morphism the morphism attached to this arrow (the same theory may be attached to multiple nodes)
 */
case class DiagramArrow(label: LocalName, from: LocalName, to: LocalName, morphism: AnonymousMorphism) extends DiagramElement {
  def toTerm = OML(label, Some(MorphType(OML(from), OML(to))), Some(morphism.toTerm))
  override def toString = s"$label:$from->$to=$morphism"
}
/** a diagram in the category of theories and morphisms
 *  @param nodes the nodes
 *  @param arrows the arrows
 *  @param distNode the label of a distinguished node to be used if this diagram is used like a theory
 *  @param distArrow the label of a distinguished arrow to be used if this diagram is used like a morphism
 *  invariant: codomain of distArrow is distNode
 */
class AnonymousDiagram(val nodes: List[DiagramNode], val arrows: List[DiagramArrow], val distNode: Option[LocalName], val distArrow: Option[LocalName]) {
  def getDistNode: Option[DiagramNode] = nodes.find(distNode contains _.label)
  def getDistArrow: Option[DiagramArrow] = arrows.find(distArrow contains _.label)
  def getElements = nodes:::arrows
  def toTerm = AnonymousDiagramCombinator(nodes, arrows, distNode, distArrow)
  override def toString = nodes.mkString(", ") + "; " + arrows.mkString(", ") + "; " + (distNode.toList ::: distArrow.toList).mkString(", ") 
}

/** bridges between [[AnonymousDiagram]] and [[Term]] */
object AnonymousDiagramCombinator {
  val path = ModExp.anonymousdiagram

  def apply(nodes: List[DiagramNode], arrows: List[DiagramArrow], distNode: Option[LocalName], distArrow: Option[LocalName]) = {
    val nodesT = nodes map {n => n.toTerm}
    val arrowsT = arrows map {a => a.toTerm}
    val dN = distNode.toList.map(n => OML(n))
    val dA = distArrow.toList.map(n => OML(n))
    OMA(OMS(this.path), dN ::: dA ::: nodesT ::: arrowsT)
  }
  def unapply(t: Term): Option[AnonymousDiagram] = t match {
    case OMA(OMS(this.path), args) =>
      var nodes: List[DiagramNode] = Nil
      var arrows: List[DiagramArrow] = Nil
      var (elems, dN, dA) = args match {
        case OML(dN, None, None, _ , _) :: OML(dA, None, None, _ , _) :: rest => (rest, Some(dN), Some(dA))
        case OML(dN, None, None, _ , _) :: rest => (rest, Some(dN), None)
        case rest => (rest, None, None)
      }
      elems foreach {
        case OML(name, Some(tp), Some(df), _, _) => (tp, df) match {
          case (TheoryType(Context.empty), AnonymousTheoryCombinator(at)) =>
            nodes ::= DiagramNode(name, at)
          case (MorphType(OML(f, None, None, _, _), OML(t, None, None, _, _)), AnonymousMorphismCombinator(am)) =>
            arrows ::= DiagramArrow(name, f, t, am)
          case _ => return None
        }
        case _ => return None
      }
      val ad = new AnonymousDiagram(nodes.reverse, arrows.reverse, dN, dA)
      Some(ad)
    case _ => None
  }
  
}

object OMLList {
  // awkward casting here, but this way the list is not copied; thus, converting back and forth between Term and AnonymousTheory is cheap
  def unapply(ts: List[Term]): Option[List[OML]] = {
    if (ts.forall(_.isInstanceOf[OML]))
      Some(ts.asInstanceOf[List[OML]])
    else
      None
  }
}

class DerivedOMLFeature(val feature: String) {
   val path = Path.parseS("http://cds.omdoc.org/mmt?mmt?StructuralFeature", NamespaceMap.empty)
   def maketerm(feat : String, tp : Term) =
      OMA(OMS(path), List(OML(LocalName(feat)),tp))

   def apply(name: LocalName, tp: Term, df: Option[Term] = None, nt: Option[TextNotation] = None) =
      OML(name, Some(tp), df, nt, Some(feature))
}

object DerivedOMLFeature {
   def apply(name: LocalName, feat: String, tp: Term, df: Option[Term] = None) = OML(name, Some(tp), df, None, Some(feat))
   def unapply(o:OML): Option[(LocalName, String, Term, Option[Term])] = o match {
      case OML(n, Some(tp), df,_, Some(f)) => Some((n,f,tp,df))
      case _ => None
   }

  /** for mixing into subclasses of the companion class */
  trait Named {self: DerivedOMLFeature =>
     def unapply(o : OML): Option[(LocalName, Term, Option[Term])] = {
        if (o.featureOpt contains feature) {
           o match {
              case OML(n, Some(tp), df, None, _) => Some((n,tp,df))
              case _ => throw ImplementationError("unsupported properties of derived declaration")
           }
        } else
           None
     }
  }
  /** for mixing into subclasses of the companion class */
  trait Unnamed {self: DerivedOMLFeature =>
    def apply(p: MPath, df: Option[Term]): OML = apply(LocalName(p), OMMOD(p), df)
    def unapply(o : OML): Option[(MPath, Option[Term])] = {
        if (o.featureOpt contains feature) {
           o match {
              case OML(LocalName(ComplexStep(p)::Nil), Some(OMMOD(q)), df, _, _) if p == q => Some((p, df))
              case _ => throw ImplementationError("unsupported properties of derived declaration")
           }
        } else
           None
     }
  }
}

object IncludeOML extends DerivedOMLFeature("include") with DerivedOMLFeature.Unnamed {
   def apply(p: MPath, args: List[Term]): OML = apply(LocalName(p), OMPMOD(p, args))
}

object StructureOML extends DerivedOMLFeature("structure") with DerivedOMLFeature.Named

/**
 * a realization declaration is like a structure/include except it *postulates* the existence of a theory morphism
 * if partial, the morphism maps declarations to declarations of the same local name in the containing theory
 */
object RealizeOML extends DerivedOMLFeature("realize") with DerivedOMLFeature.Unnamed
