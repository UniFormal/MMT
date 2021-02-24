package info.kwarc.mmt.api.ontology
import info.kwarc.mmt.api._
import utils._
import info.kwarc.mmt.api.moc._
import info.kwarc.mmt.api.objects.{OMS, Term}
import info.kwarc.mmt.api.ontology.rdf.ULO.{ObjectProperty, ULOElem, meta_theory}
import info.kwarc.mmt.api.ontology.rdf.{ULO, ULOStatement}
import org.eclipse.rdf4j.model.IRI
import org.eclipse.rdf4j.model.util.Values.iri


/**
 * An object of type Unary represents a unary predicate on MMT paths in the MMT ontology.
 * The semantics of these objects is given by their name
 */
sealed abstract class Unary(override val toString : String, val toULO : Option[ULO.Class] = None) {
   /** yields the corresponding relational item that classifies p */
   def apply(p : Path) = Individual(p, this)
}
case object IsDocument extends Unary("document",Some(ULO.document))
case object IsTheory extends Unary("theory",Some(ULO.theory))
case object IsView extends Unary("view",Some(ULO.view))
case object IsStructure extends Unary("structure",Some(ULO.structure))
case object IsConstant extends Unary("constant",Some(ULO.constant))
case object IsUntypedConstant extends Unary("untypedconstant")
case object IsDataConstructor extends Unary("dataconstructor",Some(ULO.function))
case object IsRule extends Unary("rule",Some(ULO.rule_constant))
case object IsJudgementConstructor extends Unary("judgementconstructor",Some(ULO.judgment_constructor))
case object IsDatatypeConstructor extends Unary("datatypeconstructor",Some(ULO.constructor))
case object IsHighUniverse extends Unary("highuniverse",Some(ULO.high_universe))
case object IsPattern extends Unary("pattern",Some(ULO.pattern))
case object IsInstance extends Unary("instance",Some(ULO.instance))
case object IsDerivedDeclaration extends Unary("deriveddeclaration",Some(ULO.derived_declaration))
case object IsConAss extends Unary("conass")
case object IsStrAss extends Unary("strass")
case object IsNotation extends Unary("notation")

/** Extractor extensions should use instances of this class to extend the ontology for unary relations */
case class CustomUnary(name : String, val namespace : DPath) extends Unary(name) {
   override val toULO: Option[ULO.Class] = Some(new ULO.Class(name) {
      override def toIri: IRI = iri(ULO.URLEscape.apply(namespace).toString + "#" + ULO.URLEscape.URLEscaping(name))
   })
}


object Unary {
   def parse(t : Term) : Unary = t match {
      case OMS(QMTUnaries.IsDocument) => IsDocument
      case OMS(QMTUnaries.IsTheory) => IsTheory
      case OMS(QMTUnaries.IsView) => IsView
      case OMS(QMTUnaries.IsStructure) => IsStructure
      case OMS(QMTUnaries.IsConstant) => IsConstant
      case OMS(QMTUnaries.IsUntypedConstant) => IsUntypedConstant
      case OMS(QMTUnaries.IsDatatypeConstructor) => IsDatatypeConstructor
      case OMS(QMTUnaries.IsDataConstructor) => IsDataConstructor
      case OMS(QMTUnaries.IsJudgementConstructor) => IsJudgementConstructor
      case OMS(QMTUnaries.IsRule) => IsRule
      case OMS(QMTUnaries.IsHighUniverse) => IsHighUniverse
      case OMS(QMTUnaries.IsInstance) => IsInstance
      case OMS(QMTUnaries.IsDerivedDeclaration) => IsDerivedDeclaration
      case OMS(QMTUnaries.IsConAss) => IsConAss
      case OMS(QMTUnaries.IsStrAss) => IsStrAss
      case OMS(QMTUnaries.IsNotation) => IsNotation
      // TODO: Custom unary
   }
}

/**
 * An object of type Binary represents a binary predicate between MMT paths in the MMT ontology.
 * The semantics of these objects is given by their name
 */
sealed abstract class Binary(val desc : String, val backwardsDesc: String, val toULO : Option[ObjectProperty] = None) {
   /** yields the corresponding relational item that classifies p */
   def apply(subj : Path, obj : Path) = Relation(this, subj, obj)
   /** syntactic sugar for queries: ToSubject(this) */
   def unary_- = ToSubject(this)
   /** syntactic sugar for queries: ToObject(this) */
   def unary_+ = ToObject(this)
}

object Binary {
  implicit def toRelation(b: Binary) = +b

   def parse(t : Term) : Binary = t match {
      case OMS(QMTBinaries.DependsOn) => DependsOn
      case OMS(QMTBinaries.HasMeta) => HasMeta
      case OMS(QMTBinaries.Includes) => Includes
      case OMS(QMTBinaries.HasDomain) => HasDomain
      case OMS(QMTBinaries.HasCodomain) => HasCodomain
      case OMS(QMTBinaries.IsInstanceOf) => IsInstanceOf
      case OMS(QMTBinaries.RefersTo) => RefersTo
      case OMS(QMTBinaries.Declares) => Declares
      case OMS(QMTBinaries.IsAliasFor) => IsAliasFor
      case OMS(QMTBinaries.IsAlignedWith) => IsAlignedWith
      case OMS(QMTBinaries.HasViewFrom) => HasViewFrom
      case OMS(QMTBinaries.IsImplicitly) => IsImplicitly

      // TODO: Custom binary
   }
}

// module - module, component - component
case object DependsOn extends Binary("depends on", "depended on by",Some(ULO.depends_on))
// theory - theory
case object HasMeta extends Binary("has meta-theory", "is meta-theory of",Some(ULO.has_metatheory))
case object Includes extends Binary("includes", "included by",Some(ULO.includes))
//link - theory, style - any
case object HasDomain extends Binary("has domain", "is domain of",Some(ULO.domain))
case object HasCodomain extends Binary("has codomain", "is codomain of",Some(ULO.codomain))
//theory - theory, mediated by implicit views and structures (excluding includes)
case object IsImplicitly extends Binary("implicitly realizes", "is implicitly realized by",Some(ULO.realizes))
//structure - structure, mediated by views
case object HasViewFrom extends Binary("has view from", "has view as",Some(ULO.has_view_from))
// constant - constant (not used yet)
case object IsInstanceOf extends Binary("is instance of", "instantiates",Some(ULO.instance_of))
//path - path
case object RefersTo extends Binary("refers to", "is refered to by",Some(ULO.crossrefs))
//parent - child (many-many relation because a declaration may be referenced in other documents)
case object Declares extends Binary("contains declaration of", "is declared in",Some(ULO.declares))
// symbol - symbol, module - module
case object IsAliasFor extends Binary("is alias for", "has alias",Some(ULO.same_as))
// symbol - symbol
case object IsAlignedWith extends Binary("is aligned with", "is aligned with",Some(ULO.aligned_with))

/** Extractor extensions should use instances of this class to extend the ontology for binary relations */
case class CustomBinary(name : String, override val desc : String, override val backwardsDesc : String, val namespace : DPath) extends Binary(desc, backwardsDesc) {
  override def toString = name
   override val toULO: Option[ULO.ObjectProperty] = Some(new ObjectProperty(name) {
      override def toIri: IRI = iri(ULO.URLEscape(namespace).toString + "#" + ULO.URLEscape.URLEscaping(name))
   })
}

/** A RelationalElement is any element that is used in the relational representation of MMT content.
 * These include the unary and binary predicates occurring in an MMT ABox.
 * They do not correspond to XML elements in an OMDoc element and thus do not extend StructuralElement.
 */
abstract class RelationalElement {
   /** the URL from which this item originated, currently not implemented */
   //val parent : Path = null //TODO origin of relational items
   /** the MMTURI of the "about" item in the RDF sense */
   val path : Path
   /** XML representation */
   def toNode : scala.xml.Node
   /** text representation */
   def toPath : String

   def toULO : Option[ULOStatement]
}

/**
 * An object of type Individual represents a unary predicate in the ABox.
 */
case class Individual(path : Path, tp : Unary) extends RelationalElement {
   def toNode = <individual path={path.toPath} predicate={tp.toString}/>
   def toPath = tp.toString + " " + path.toPath

   override def toULO: Option[ULOStatement] = tp.toULO match {
      case Some(ulo) => Some(ulo(path))
      case _ =>
       None
   }
}

/**
 * An object of type Relation represents a binary predicate in the ABox.
 * The path of a Relation is always the knowledge item reading which produced the Relation.
 */
case class Relation(dep : Binary, subj : Path, obj : Path) extends RelationalElement {
   val path = subj
   def toNode = <relation subject={subj.toPath} predicate={dep.toString} object={obj.toPath}/>
   override def toString = subj.toString + " " + dep.toString + " " + obj.toString
   def toPath = dep.toString + " " + subj.toPath + " " + obj.toPath

   override def toULO: Option[ULOStatement] = dep.toULO match {
      case Some(ulo) => Some(ulo(subj,obj))
      case _ => None
   }
}
