package info.kwarc.mmt.api

/** A component of a declaration, e.g., the type of a [[Constant]] (akin to XML attributes) */
case class DeclarationComponent(key: ComponentKey, value: ComponentContainer)

/** A ComponentContainer holds the data keyed by a DeclarationComponent */
trait ComponentContainer {
   /** updates this container using the pieces of another one */
   def update(nw: ComponentContainer)
   /** empties this container, afterwards the component is absent */
   def delete: Unit
   /** true if (some dimension of) this component is present */
   def isDefined: Boolean
}

trait AbstractTermContainer extends ComponentContainer {
   def get: Option[objects.Term]
}

/** a dummy container for a stateless term */ 
class FinalTermContainer(t: objects.Term) extends AbstractTermContainer {
   def update(nw: ComponentContainer) {}
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

/** type of a [[symbols.Constant]] */
case object TypeComponent extends ComponentKey("type")
/** definitiens of [[symbols.Constant]], DefinedTheory, DefinedView, DefinedStructure */
case object DefComponent  extends ComponentKey("definition")
/** domain of a [[modules.Link]], meta-theory of a theory */
case object DomComponent  extends ComponentKey("domain")
/** codomain of a [[modules.Link]] */
case object CodComponent  extends ComponentKey("codomain")

/** custom component, e.g., in a [[DerivedDeclaration]] */
case class OtherComponent(s: String) extends ComponentKey(s)

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
case object ParamsComponent extends  ComponentKey("params")
case object PatternBodyComponent extends ComponentKey("pattern-body")
case object MetaDataComponent extends ComponentKey("metadata")

object TermComponent {
   private val components = List(TypeComponent,DefComponent,DomComponent,CodComponent)
   def parse(s: String) = components find {c => c.toString == s} getOrElse {throw ParseError("illegal component: " + s)}
}