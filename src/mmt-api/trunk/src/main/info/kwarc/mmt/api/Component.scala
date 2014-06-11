package info.kwarc.mmt.api

/** A ComponentContainer holds the data keyed by a DeclarationComponent */
trait ComponentContainer {
   /** updates this container using the pieces of another one */
   def update(nw: ComponentContainer)
   /** empties this container, afterwards the component is absent */
   def delete
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

/** A DeclarationComponent identifies a component of a Declaration. It is used in CPath's */
abstract class DeclarationComponent(s : String) {
   override def toString = s
}

object DeclarationComponent {
  def parse(s : String) : DeclarationComponent = s match {
    case "type" => TypeComponent
    case "definition" => DefComponent
    case "domain" => DomComponent
    case "codomain" => CodComponent
    case "params" => ParamsComponent
    case "pattern-body" => PatternBodyComponent
    case "metadata" => MetaDataComponent
    case s => throw ImplementationError("Invalid Declaration Component name : " + s)
  }
}

/** type of a [[symbols.Constant]] */
case object TypeComponent extends DeclarationComponent("type")
/** definitiens of [[symbols.Constant]], DefinedTheory, DefinedView, DefinedStructure */
case object DefComponent  extends DeclarationComponent("definition")
/** domain of a [[modules.Link]], meta-theory of a theory */
case object DomComponent  extends DeclarationComponent("domain")
/** codomain of a [[modules.Link]] */
case object CodComponent  extends DeclarationComponent("codomain")

abstract class NotationComponent(s: String) extends DeclarationComponent(s)
/** (text-based) parsing notation of a symbol */
case object ParsingNotationComponent extends NotationComponent("parsing-notation")
/** (two-dimensional, MathML-like) presentation notation of a symbol */
case object PresentationNotationComponent extends NotationComponent("presentation-notation")
/** (narrative) verbalization notation of a symbol */
case object VerbalizationNotationComponent extends NotationComponent("verbalization-notation")

// the following components are used only by change management
case object ParamsComponent extends DeclarationComponent("params")
case object PatternBodyComponent extends DeclarationComponent("pattern-body")
case object MetaDataComponent extends DeclarationComponent("metadata")

object TermComponent {
   private val components = List(TypeComponent,DefComponent,DomComponent,CodComponent)
   def parse(s: String) = components find {c => c.toString == s} getOrElse {throw ParseError("illegal component: " + s)}
}