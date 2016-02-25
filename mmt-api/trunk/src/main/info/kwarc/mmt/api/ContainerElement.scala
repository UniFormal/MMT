package info.kwarc.mmt.api

trait NamedElement {
   /** the name relative to the parent */
   def name: LocalName
}

trait ElementContainer[+S <: NamedElement] {
   def domain: Iterable[LocalName]
   def getDeclarations: List[S]

   def getO(name: LocalName): Option[S]
   def getMostSpecific(name: LocalName): Option[(S,LocalName)]

   def isDeclared(name : LocalName) = getO(name).isDefined
   
   /** same as get(LocalName(name)) */ 
   def getO(name : String) : Option[S] = getO(LocalName(name))
   def get(name: LocalName): S = getO(name) getOrElse {
     throw ImplementationError("get called without checking that name exists")
   }
}

trait NamedElementContainer[+S <: NamedElement] extends ElementContainer[S] with NamedElement

trait DefaultLookup[S <: NamedElement] {self: ElementContainer[S] =>
  def domain = getDeclarations.map(_.name)
  def getO(name: LocalName) = getDeclarations.reverse.find(_.name == name)
  def getMostSpecific(name: LocalName): Option[(S,LocalName)] = {
    getDeclarations.reverse.foreach {i =>
      name.dropPrefix(i.name).foreach {suffix =>
        return Some((i, suffix))
      }
    }
    None
  }
}

trait MutableElementContainer[S <: NamedElement] extends ElementContainer[S] {
  def add(s: S, after: Option[LocalName] = None): Unit
  def update(s: S): Unit
  def delete(name: LocalName): Option[S]
  def reorder(name: LocalName): Unit
}

class ListContainer[S <: NamedElement](items: List[S]) extends ElementContainer[S] with DefaultLookup[S] {
  def getDeclarations = items
}

/**
 * This is now a Container[O] rather than I
 * internals are done via encapsulation
 * this allows the inherited method .getDeclarations to work as expected during lookup
 * so that Containers and ElaboratableElements behave the same 
 * (i.e. look by default in the elaboration)
 */
trait ElaboratableElement[I <: NamedElement, O <: StructuralElement] extends NamedElementContainer[O] {
   def components : List[I] 
   //just awful 
   private val outer = this
   def internalView = new ListContainer[I](components) with NamedElementContainer[I] {
     val name = outer.name
   }
   def elaborate: ElementContainer[O]
}

/**
 * Rework of StructuralFeature
 * check and elaborate taking just a Context is to weak I think
 * generally they need access to the library to look up dependant theories (e.g. for realms) 
 */
abstract class ElaboratorExtension[I <: NamedElement, O <: StructuralElement](val key : String) extends frontend.FormatBasedExtension {
   def isApplicable(s: String) = s == key
   
   def check(context: objects.Context, internal: NamedElementContainer[I])(implicit env: checking.CheckingEnvironment): Unit

   def elaborate(context: objects.Context, internal: NamedElementContainer[I]): ElementContainer[O]
//   def modules(d: DerivedDeclaration): List[Module]
}

/**
 * Rework of DerivedDeclaration
 * elaborator is now taken as a constructor argument
 * this has the positive side-effect that elaboration can be local and library doesn't need a controller anymore
 * the (big) negative is that parsing/construction fails if the right extension isn't loaded
 * Considering lookup is such a central feature of MMT a missing elaborator would break things anyway though
 * Extends ElaboratableElement (as it should) and trivially implements the elaborate method based on its elaborator
 */
abstract class ElaboratingDeclaration[I <: NamedElement, O <: StructuralElement](elaborator : ElaboratorExtension[I,O]) extends ElaboratableElement[I,O] {
  def elaborate : ElementContainer[O] = elaborator.elaborate(objects.Context.empty, internalView)
  def getDeclarations: List[O] = elaborate.getDeclarations
  
}

/**
 * Something like DeclarationGroups would extend this
 * Theories and Views also if they were elaboratable elements to begin with
 */
trait RecursivelyElaboratableElement[S <: StructuralElement] extends StructuralElement with ElaboratableElement[S,S] {
  def elaborate = {
    val ds = getDeclarations.flatMap {
      case ee: ElaboratableElement[S,S] =>
        ee.elaborate.getDeclarations
      case e => List(e)
    }
    new ListContainer(ds)
  }
}

/**
 * Attempt to rework some part of Library code without disturbing existing code
 * get, getDeclarationInTerm stay the same (mostly)
  
 */
abstract class GenericLookup {
  //generic lookup method where containers lazyly resolve names inside 
  //e.g. getInTheory method from library would become .get or .getElab method of DeclaredTheory Class
  def getDeclarationInElement(elem : NamedElement, name : LocalName) : NamedElement = name match {
    case LocalName.empty => elem
    case _ => elem match {
      case cont : ElementContainer[_] =>
        cont.getMostSpecific(name) match {
          case Some((newElem, newName)) => getDeclarationInElement(newElem, newName)
          case None => throw new GetError("container cannot find name" + name + " inside")
        }
      case _ => throw new GetError("loooking for name inside non-container element")      
    }    
  }
}
