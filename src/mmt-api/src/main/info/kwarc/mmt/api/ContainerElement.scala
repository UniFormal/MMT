package info.kwarc.mmt.api

import objects._

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
  def getO(name: LocalName) = getDeclarations.view.reverse.find(_.name == name)
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

trait ContainerElement[S <: StructuralElement] extends StructuralElement with MutableElementContainer[S]

class ListContainer[S <: NamedElement](items: List[S]) extends ElementContainer[S] with DefaultLookup[S] {
  def getDeclarations = items
}

trait ElaboratableElement[I <: NamedElement, O <: StructuralElement] extends NamedElement with ElementContainer[I] {
  def feature : String // id of the default elaborator
  def getElaboration(context: Context, elab : ElaboratorExtension[I,O]) : List[O] = elab.elaborate(context, this).getDeclarations
}

abstract class ElaboratorExtension[I <: NamedElement, O <: StructuralElement](val key : String) extends frontend.FormatBasedExtension {
   def isApplicable(s: String) = s == key
   def check(context: objects.Context, elem: ElaboratableElement[I,O])(implicit env: checking.CheckingEnvironment): Unit
   def elaborate(context: objects.Context, elem: ElaboratableElement[I,O]): ElementContainer[O]
//   def modules(d: DerivedDeclaration): List[Module]
}

/*
trait RecursivelyElaboratableElement[S <: StructuralElement] extends StructuralElement with ElaboratableElement[S,S] {
  def getElaboration(context: Context, elab : ElaboratorExtension[S,S]) = {
    val ds = components.flatMap {
      case ee: ElaboratableElement[S,S] =>
        ee.getElaboration(context, elab)
      case e => List(e)
    }
    ds
  }
}
*/