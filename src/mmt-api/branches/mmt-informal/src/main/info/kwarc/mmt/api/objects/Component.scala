package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._

/** A DeclarationComponent identifies a component of a Declaration. It is used in CPath's */
abstract class DeclarationComponent(s : String) {
   override def toString = s
}

object DeclarationComponent {
  def apply(s : String) : DeclarationComponent = s match {
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

/** type of a Constant */
case object TypeComponent extends DeclarationComponent("type")
/** definitiens of Constant, DefinedTheory, DefinedView, DefinedStructure; target of an Assignment */
case object DefComponent  extends DeclarationComponent("definition")
/** domain of a Link, meta-theory of a theory */
case object DomComponent  extends DeclarationComponent("domain")
/** codomain of a link */
case object CodComponent  extends DeclarationComponent("codomain")

// the following components are used only by change management
case object ParamsComponent extends DeclarationComponent("params")
case object PatternBodyComponent extends DeclarationComponent("pattern-body")
case object MetaDataComponent extends DeclarationComponent("metadata")

object TermComponent {
   private val components = List(TypeComponent,DefComponent,DomComponent,CodComponent)
   def parse(s: String) = components find {c => c.toString == s} getOrElse {throw ParseError("illegal component: " + s)}
}