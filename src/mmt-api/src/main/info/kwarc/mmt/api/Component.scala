package info.kwarc.mmt.api

import objects._

/** A component of a declaration, e.g., the type of a [[Constant]] (akin to XML attributes) */
case class DeclarationComponent(key: ComponentKey, value: ComponentContainer) extends NamedElement {
  val name = LocalName(key.toString) //TODO integrate ComponentKey as LocalName
}

/** A ComponentContainer holds the data keyed by a DeclarationComponent */
trait ComponentContainer {
   /** updates this container using the pieces of another one
    *  @return true if a change took place
    */
   def update(nw: ComponentContainer): Boolean
   /** empties this container, afterwards the component is absent */
   def delete: Unit
   /** true if (some dimension of) this component is present */
   def isDefined: Boolean
}

trait AbstractObjectContainer extends ComponentContainer {
   def get: Option[Obj]
}

trait AbstractTermContainer extends AbstractObjectContainer {
   def get: Option[Term]
}

/** a dummy container for a stateless term */ 
class FinalTermContainer(t: Term) extends AbstractTermContainer {
   def update(nw: ComponentContainer) = nw match {
      case nw: FinalTermContainer => nw.get != Some(t)
      case _ => throw ImplementationError("cannot update final term container")
   }
   def delete {}
   def isDefined = true
   def get = Some(t) 
}

/** A ComponentKey identifies a [[DeclarationComponent]]. */
abstract class ComponentKey(s : String) {
   override def toString = s
   def apply(cont: ComponentContainer) = DeclarationComponent(this, cont)
}

object ComponentKey {
  def parse(s : String) : ComponentKey = s match {
    case "type" => TypeComponent
    case "definition" => DefComponent
    case "domain" => DomComponent
    case "codomain" => CodComponent
    case "params" => ParamsComponent
    case "pattern-body" => PatternBodyComponent
    case "metadata" => MetaDataComponent
    case s if s.startsWith("ext-") => OtherComponent(s)
    case s => throw ParseError("Invalid name of declaration component: " + s)
  }
}

/** components that are MMT objects */
abstract class ObjComponentKey(s: String) extends ComponentKey(s)

/** components that are MMT terms */
abstract class TermComponentKey(s: String) extends ObjComponentKey(s)

/** type of a [[symbols.Constant]] */
case object TypeComponent extends TermComponentKey("type")
/** definiens of [[symbols.Constant]], DefinedTheory, DefinedView, DefinedStructure */
case object DefComponent  extends TermComponentKey("definition")
/** domain of a [[modules.Link]], meta-theory of a theory */
@deprecated("replace with TypeComponent")
case object DomComponent  extends TermComponentKey("domain")
/** codomain of a [[modules.Link]] */
@deprecated("replace with TypeComponent")
case object CodComponent  extends TermComponentKey("codomain")

/** parameters */
@deprecated("replace with TypeComponent")
case object ParamsComponent extends ObjComponentKey("params")

/** custom component, e.g., in a [[DerivedDeclaration]] */
case class OtherComponent(s: String) extends ComponentKey(s)

/** components that are notations */
abstract class NotationComponentKey(s: String) extends ComponentKey(s)
/** (text-based) parsing notation of a symbol */
case object ParsingNotationComponent extends NotationComponentKey("parsing-notation")
/** (two-dimensional, MathML-like) presentation notation of a symbol */
case object PresentationNotationComponent extends NotationComponentKey("presentation-notation")
/** (narrative) verbalization notation of a symbol */
case object VerbalizationNotationComponent extends NotationComponentKey("verbalization-notation")

object NotationComponent {
   def unapply(d: DeclarationComponent) = d match {
      case DeclarationComponent(k: NotationComponentKey, v: notations.NotationContainer) => Some((k,v))
      case _ => None
   }
}

// the following components are used only by change management
case object PatternBodyComponent extends ComponentKey("pattern-body")
case object MetaDataComponent extends ComponentKey("metadata")

object TermComponent {
   private val components = List(TypeComponent,DefComponent,DomComponent,CodComponent)
   def parse(s: String) = components find {c => c.toString == s} getOrElse {throw ParseError("illegal component: " + s)}
}