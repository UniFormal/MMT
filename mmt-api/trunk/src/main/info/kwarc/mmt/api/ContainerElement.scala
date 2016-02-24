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

trait ElaboratableElement[I <: NamedElement, O <: StructuralElement] extends ElementContainer[I] {
   def elaborate: ElementContainer[O]
}

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
