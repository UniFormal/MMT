package info.kwarc.mmt.api.objects

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.notations._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.utils._


/** auxiliary class for storing lists of declarations statefully without giving it a global name
  *
  * anonymous modules are object that can be converted into these helper classes using the objects [[AnonymousTheory]] and [[AnonymousMorphism]]
  */
trait AnonymousBody extends ElementContainer[OML] with DefaultLookup[OML] with ShortURIPrinter{
  val decls: List[OML]
  def getDeclarations = decls
  def toSubstitution : Substitution = decls.map{case OML(name,_,df,_,_) => Sub(name,df.get)}
  def toTerm: Term
  def toStr(implicit shortURIs: Boolean) = getDeclarations.map(_.toStr).mkString("{", ", ", "}")
}

/** a theory given by meta-theory and body */
case class AnonymousTheory(mt: Option[MPath], val decls: List[OML]) extends AnonymousBody {
  def rename(oldNew: (LocalName,Term)*) = {
    val sub: Substitution = oldNew.toList map {case (old,nw) => Sub(old, nw)}
    val trav = objects.OMLReplacer(sub)
    val newDecls = decls map {oml =>
      // TODO this renaming is too aggressive if there is OML shadowing or if OMLs are used for other purposes
      val omlR : OML = trav(oml, Context.empty).asInstanceOf[OML] // this traverser always maps OMLs to OMLs
      listmap(oldNew, oml.name) match {
        case Some(nw) => omlR.copy(name = nw.asInstanceOf[OML].name)
        case None => omlR
      }
    }
    AnonymousTheory(mt, newDecls)
  }
  def add(d: OML) = new AnonymousTheory(mt, decls ::: List(d))
  def union(that: AnonymousTheory) = {
    val ds = (this.decls ::: that.decls).distinct
    if (this.mt != that.mt) throw GeneralError("different meta-theories")
    new AnonymousTheory(mt, ds)
  }
  def toTerm = AnonymousTheoryCombinator(mt, decls)
  override def toString = {
    val m = mt.map(_.toString).getOrElse("")
    m + super.toString
  }
}

/** bridges between [[AnonymousTheory]] and [[Term]] */
// TODO decide if AnonymousTheory should subclass Term directly
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
case class AnonymousMorphism(val decls: List[OML]) extends AnonymousBody {
  def toTerm = AnonymousMorphismCombinator(decls)
  def add(d: OML) = new AnonymousMorphism(decls ::: List(d))

  /** creates a traverser for morphism application (more efficient if multiple applications are needed) */
  def applier = {
    val subs = decls map {o => Sub(o.name, o.df.get)}
    OMLReplacer(subs)
  }

  /** applies this morphism to a term */
  def apply(t: Term): Term = applier(t, Context.empty)

  /** diagram-order composition: this ; that */
  def compose(that: AnonymousMorphism) = {
    val applyThat = that.applier
    val declsMapped = decls map {o =>
       val dfMapped = o.df map {d => applyThat(d, Context.empty)}
       OML(o.name, None, dfMapped)
     }
    AnonymousMorphism(declsMapped)
  }
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
sealed abstract class DiagramElement extends ShortURIPrinter {
  def label: LocalName
  def toTerm: Term
  def getBody: AnonymousBody
}
/** a node in an [[AnonymousDiagram]]
  *  @param label the label of this node
  *  @param theory the theory attached to this node (the same theory may be attached to multiple nodes)
  */
case class DiagramNode(label: LocalName, theory: AnonymousTheory) extends DiagramElement {
  override def getBody: AnonymousBody = theory
  override def toTerm: Term = OML(label, Some(TheoryType(Nil)), Some(theory.toTerm))
  override def toStr(implicit shortURIs: Boolean) = s"$label:THY=${theory.toStr}"
  def canEqual(a: Any): Boolean = a.isInstanceOf[DiagramNode]
  override def equals(that: Any): Boolean =
    that match {
      case that: DiagramNode => (that.label == this.label) && (that.theory == this.theory)
      case _ => false
    }
  override def hashCode: Int = { label.hashCode() + theory.hashCode() }
}
/** an arrow in an [[AnonymousDiagram]]
  *  @param label the label of this arrow
  *  @param from the label of the domain node in the same diagram
  *  @param to the label of the codomain node in the same diagram
  *  @param morphism the morphism attached to this arrow (the same theory may be attached to multiple nodes)
  */
case class DiagramArrow(label: LocalName, from: LocalName, to: LocalName, morphism: AnonymousMorphism, isImplicit: Boolean) extends DiagramElement {
  override def getBody: AnonymousBody = morphism
  override def toTerm: Term = {
    val f = if (isImplicit) Some("implicit") else None
    OML(label, Some(MorphType(OML(from), OML(to))), Some(morphism.toTerm), None, f)
  }
  override def toStr(implicit shortURIs: Boolean): String = {
    val a = if (isImplicit) "-i->" else "--->"
    s"$label:$from$a$to=${morphism.toStr}"
  }
}
/** a diagram in the category of theories and morphisms
  *  @param nodes the nodes
  *  @param arrows the arrows
  *  @param distNode the label of a distinguished node to be used if this diagram is used like a theory
  *
  * Invariants:
  *   - invariant: codomain of distArrow is distNode
  *   - among all [[DiagramNode diagram nodes]] and [[DiagramArrow diagram arrows]] the labels are unique
  *     in other words: ''getElements.map(_.label)'' is a sequence without duplicates
  */
case class AnonymousDiagram(nodes: List[DiagramNode], arrows: List[DiagramArrow], distNode: Option[LocalName]) {
  def getNode(label: LocalName): Option[DiagramNode] = nodes.find(_.label == label)
  def getArrow(label: LocalName): Option[DiagramArrow] = arrows.find(_.label == label)
  def getArrowWithNodes(label: LocalName): Option[(DiagramNode,DiagramNode,DiagramArrow)] = {
    val a = getArrow(label).getOrElse(return None)
    val f = getNode(a.from).getOrElse(return None)
    val t = getNode(a.to).getOrElse(return None)
    Some((f,t,a))
  }
  def getDistNode: Option[DiagramNode] = distNode flatMap getNode
  def getDistArrow: Option[DiagramArrow] = distNode flatMap {n => arrows.find(a => a.isImplicit && a.to == n)}
  /**
   * returns all distinguished arrows that can be composed to form a morphism into the node label
   * starting from that node 
   */
  def getDistArrowsTo(label: LocalName): List[DiagramArrow] = {
    arrows.find(a => a.to == label && a.isImplicit) match {
      case None => Nil
      case Some(a) => a :: getDistArrowsTo(a.from)
    }
  }
  def getAllArrowsTo(label : LocalName) : List[DiagramArrow] = {
    arrows.filter(a => a.to == label)
  }
  /* Finding the path between two nodes using their labels */
  def path(sourceLabel : LocalName, targetLabel : LocalName) : List[DiagramArrow] ={
    arrows.find(a => a.to == targetLabel) match {
      case None => Nil
      case Some(a) =>
        a :: (if (a.label == sourceLabel) Nil else path(sourceLabel,a.label))
    }
  }

  /* A function to compose two substitutions
   * The function assume that assign1 has no duplicate assignments for the same symbol.
   */

  /* Finding the views
   * - each view is a list of assignments of terms to constants TODO: we consider now only constants to constants
   * - find the path (list of views) from the source to the target
   * - calculate the flat list of assignments in these views (order matters for cases like [a |-> b, b |-> c])
   * An assignment name = definition is represented as an OML(name, type, definition, notationOpt, featureOpt)
   * */
  def viewOf(source : DiagramNode, target : DiagramNode): List[OML] = {
    val arrows: List[DiagramArrow] = path(source.label, target.label)
    val assignments: List[OML] = arrows.flatMap(a => a.morphism.decls)
    assignments
  }

  def getDistArrowWithNodes: Option[(DiagramNode,DiagramNode,DiagramArrow)] = getDistArrow flatMap {a => getArrowWithNodes(a.label)}
  def getElements: List[DiagramElement] = nodes ::: arrows

  def getElement(label: LocalName): Option[DiagramElement] = getNode(label) match {
    case Some(node) =>
      Some(node)
    case None => getArrow(label) match {
      case Some(arrow) =>
        Some(arrow)
      case None =>
        None
    }
  }

  /** replaces every label l with r(l) */
  def relabel(r: LocalName => LocalName) : AnonymousDiagram = {
    val nodesR = nodes.map(n => n.copy(label = r(n.label)))
    val arrowsR = arrows.map(a => a.copy(label = r(a.label), from = r(a.from), to = r(a.to)))
    AnonymousDiagram(nodesR, arrowsR, distNode map r)
  }

  def toTerm: Term = AnonymousDiagramCombinator(nodes, arrows, distNode)
  override def toString: String = nodes.mkString(", ") + "; " + arrows.mkString(", ") + "; " + distNode.toList.mkString("")

  /**
    * Transform the anonymous diagram into a list of in-memory [[Module modules]].
    *
    * Note that the modules do *not* get registered to the [[info.kwarc.mmt.api.frontend.Controller]] or any other
    * entity. They are solely in-memory representations.
    *
    * @param labeller A function determining the fully qualified path to the generated modules given the "anonymous" names of the diagram elements in the anonymous diagram.
    * @return List of in-memory modules, not yet registered anywhere.
    */
  def toModules(labeller: LocalName => MPath): List[Module] = {
    getElements.map {
      case DiagramNode(name, anonymousTheory) =>
        val newName = labeller(name)

        Theory(
          newName.doc,
          newName.name,
          anonymousTheory.mt,
          df = TermContainer.asParsed(anonymousTheory.toTerm)
        )

      case DiagramArrow(name, from, to, anonMor, isImplicit) =>
        val newName = labeller(name)

        View(
          newName.doc,
          newName.name,
          OMMOD(labeller(from)),
          OMMOD(labeller(to)),
          TermContainer.asParsed(anonMor.toTerm),
          isImplicit
        )
    }
  }

  /**
    * Compute the union with another diagram.
    *
    * @return A name clash occurs if there is an element in ''otherDiagram'' with a label l such that in the current diagram there is a non-equal element with the same label.
    *         If such a name clash occurs, [[None]] is returned.
    *         Otherwise, the union is returned.
    *
    */
  def ++(otherDiagram: AnonymousDiagram): Option[AnonymousDiagram] = {
    val otherElements: Map[LocalName, DiagramElement] =
      otherDiagram.getElements.groupBy(_.label).view.mapValues(_.head).toMap

    for ((otherName, otherElement) <- otherElements) {
      getElement(otherName) match {
        case Some(ourElement) if ourElement != otherElement =>
          return None // name clash

        case _ => // continue
      }
    }

    Some(AnonymousDiagram(
      // compute the unions of nodes and arrows, respectively
      (nodes ::: otherDiagram.nodes).groupBy(_.label).map(_._2.head).toList,
      (arrows ::: otherDiagram.arrows).groupBy(_.label).map(_._2.head).toList,
      None
    ))
  }

  /**
    * Get all transitive-reflexive diagram element dependencies for `element` categorizied into
    * [[DiagramNode]] dependencies and [[DiagramArrow]] dependencies.
    */
  private def getDependenciesFor(element: DiagramElement): (Set[DiagramNode], Set[DiagramArrow]) = {
    val deps = BreadthFirstSearch.collectBounded(
      all = getElements.toSet,
      initial = Seq(element),
      explorer = (elem: DiagramElement, _: Set[DiagramElement], remainingElems: Set[DiagramElement]) => {
        // If (T in deps) is included in (S in remainingElements), get T into inclusionDeps
        // TODO: Dependency tracking for anonymous modules does not account for defined views currently!
        val inclusionDeps = remainingElems.filter(_.getBody.getDeclarations.exists {
          case IncludeOML(OML(label, _, _, _, _), _) if label == elem.label => true
          case _ => false
        })

        // If (T in deps) occurs as domain or codomain of an arrow in remainingElements, get T into arrowDeps
        val arrowDeps = remainingElems.filter {
          case DiagramArrow(_, from, to, _, _) if from == elem.label || to == elem.label => true
          case _ => false
        }

        inclusionDeps ++ arrowDeps
      }
    )

    (deps.collect { case x: DiagramNode => x }, deps.collect { case x: DiagramArrow => x })
  }

  /**
    * Compute the dependencies-aware difference ''this\labels''.
    */
  def --(labels: Seq[LocalName]): AnonymousDiagram = {
    val (nodeDepsLists, arrowDepsLists) = labels.flatMap(getElement).map(getDependenciesFor).unzip

    // flatten the sets to one set, respectively
    val nodeDeps = nodeDepsLists.reduceLeftOption((x, y) => x ++ y).getOrElse(Set())
    val arrowDeps = arrowDepsLists.reduceLeftOption((x, y) => x ++ y).getOrElse(Set())

    AnonymousDiagram(
      nodes = nodes.toSet.diff(nodeDeps).toList,
      arrows = arrows.toSet.diff(arrowDeps).toList,
      distNode = distNode.filterNot(distNodeLabel => {
        nodeDeps.exists(_.label == distNodeLabel)
      })
    )
  }

  def -(otherDiagram: AnonymousDiagram): AnonymousDiagram = {
    val actualLabelsToRemove = otherDiagram.getElements.mapOrSkip(otherElement =>
      getElement(otherElement.label) match {
        case Some(ourElement) if ourElement == otherElement =>
          otherElement.label

        case _ => throw SkipThis
      }
    )

    this -- actualLabelsToRemove
  }

  def -(label: LocalName): AnonymousDiagram = {
    this -- List(label)
  }
}

object AnonymousDiagram {
  def union(diagrams: AnonymousDiagram*): Option[AnonymousDiagram] = {
    val firstDiag :: remainingDiags = diagrams.toList

    remainingDiags.foldLeft[Option[AnonymousDiagram]](Some(firstDiag))((optionDiag, diag) => optionDiag match {
      case None => None
      case Some(diagPrime) => diagPrime ++ diag
    })
  }
}

/** bridges between [[AnonymousDiagram]] and [[Term]] */
object AnonymousDiagramCombinator {
  val path = ModExp.anonymousdiagram

  def apply(nodes: List[DiagramNode], arrows: List[DiagramArrow], distNode: Option[LocalName]) = {
    val nodesT = nodes map {n => n.toTerm}
    val arrowsT = arrows map {a => a.toTerm}
    val dN = distNode.toList.map(n => OML(n))
    OMA(OMS(this.path), dN ::: nodesT ::: arrowsT)
  }
  def unapply(t: Term): Option[AnonymousDiagram] = t match {
    case OMA(OMS(this.path), args) =>
      var nodes: List[DiagramNode] = Nil
      var arrows: List[DiagramArrow] = Nil
      var (elems, dN) = args match {
        case OML(dN, None, None, _ , _) :: rest => (rest, Some(dN))
        case rest => (rest, None)
      }
      elems foreach {
        case OML(name, Some(tp), Some(df), _, feature) => (tp, df) match {
          case (TheoryType(Context.empty), AnonymousTheoryCombinator(at)) =>
            nodes ::= DiagramNode(name, at)
          case (MorphType(OML(f, None, None, _, _), OML(t, None, None, _, _)), AnonymousMorphismCombinator(am)) =>
            val impl = feature contains "implicit"
            arrows ::= DiagramArrow(name, f, t, am, impl)
          case _ => return None
        }
        case _ => return None
      }
      val ad = AnonymousDiagram(nodes.reverse, arrows.reverse, dN)
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
    def apply(p: MPath, args: List[Term], df: Option[Term]): OML = apply(LocalName(p), OMPMOD(p,args), df)
    def apply(n: LocalName, df: Option[Term]): OML = apply(n, OML(n), df)
    def unapply(o : OML): Option[(Term, Option[Term])] = {
      if (o.featureOpt contains feature) {
        o match {
          case OML(_, Some(tp), df, _, _) => Some((tp, df))
          case _ => throw ImplementationError("unsupported properties of derived declaration")
        }
      } else
        None
    }
  }
}

/**
  * An inclusion of a [[Theory]] or [[AnonymousTheory]] as a [[OML]] to be used as a declaration in an [[AnonymousBody]].
  *
  * @example
  *          '''
  *          includeOml match {
  *            case IncludeOML(OMPMOD(theory, args), df) => /* inclusion of a (possibly parametric) theory */
  *            case IncludeOML(anonTheoryName, df) => /* inclusion of an anonymous theory in the same diagram */
  *          }
  *          '''
  *
  */
object IncludeOML extends DerivedOMLFeature(IncludeVarDecl.feature) with DerivedOMLFeature.Unnamed

object StructureOML extends DerivedOMLFeature("structure") with DerivedOMLFeature.Named

/**
  * a realization declaration is like a structure/include except it *postulates* the existence of a theory morphism
  * if partial, the morphism maps declarations to declarations of the same local name in the containing theory
  */
object RealizeOML extends DerivedOMLFeature("realize") with DerivedOMLFeature.Unnamed