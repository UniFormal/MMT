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

trait AbstractTermContainer extends ComponentContainer {
   def get: Option[objects.Term]
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

/** container for an MPath */
class MPathContainer(path: Option[MPath]) extends AbstractTermContainer {
   /** internally stored as a term to allow for attaching metadata */
   private var term: Option[Term]= path map {p => OMMOD(p)}
   def update(nw: ComponentContainer) = nw match {
      case nw: MPathContainer =>
         val changed = term != nw.term
         term = nw.term
         changed
      case _ => throw ImplementationError("expected atomic component")
   }
   def delete {term = None}
   def isDefined = term.isDefined
   def get = term
   def getPath = term map {case OMMOD(p) => p}
}

/** container for mutable contexts */ 
class ContextContainer extends ComponentContainer {
   private var context: Option[Context] = None
   def update(nw: ComponentContainer) = nw match {
      case nw: ContextContainer =>
        val changed = nw.get != context
        context = nw.context
        changed
      case _ => throw ImplementationError("expected context")
   }
   def isDefined = context.isDefined
   def get = context
   def set(c: Context) {
     context = Some(c)
   }
   def delete {context = None}
}

object ContextContainer {
  def apply(c: Context) = {
    val cc = new ContextContainer
    cc.context = Some(c)
    cc
  }
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

/** type of a [[symbols.Constant]] */
case object TypeComponent extends ObjComponentKey("type")
/** definiens of [[symbols.Constant]], DefinedTheory, DefinedView, DefinedStructure */
case object DefComponent  extends ObjComponentKey("definition")
/** domain of a [[modules.Link]], meta-theory of a theory */
case object DomComponent  extends ObjComponentKey("domain")
/** codomain of a [[modules.Link]] */
case object CodComponent  extends ObjComponentKey("codomain")

/** parameters */
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