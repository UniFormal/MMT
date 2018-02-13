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

/** see add methods of [[MutableElementContainer]], [[Controller]], [[Library]] */
sealed abstract class AddPosition
case object AtBegin extends AddPosition
case object AtEnd extends AddPosition
case class After(name: LocalName) extends AddPosition
case class Before(name: LocalName) extends AddPosition

/** helper class for creating a sequence of [[ApddPosition]]'s where each is right after the previous one */
class RepeatedAdd(private var next: AddPosition) {
  def getNextFor(d: NamedElement) = {
    val n = next
    next = After(d.name)
    n
  }
}

trait MutableElementContainer[S <: NamedElement] extends ElementContainer[S] {
  def add(s: S, at: AddPosition = AtEnd): Unit
  def update(s: S): Unit
  def delete(name: LocalName): Option[S]
  /** replaces an element with the renamed version and renames all references */
  //def rename(oldNews: List[(LocalNameLocalName)]): Unit // TODO: implement in Document and Body
  /** moves the named declaration to the beginning, thus calling reorder in the new order reorders the container */
  def reorder(name: LocalName): Unit
}

/** straightforward update methods for classes that store their elements in a list-like variable */
trait DefaultMutability[S <: NamedElement] extends {self: MutableElementContainer[S] =>
   /** must be provided by the implementing class to update the declarations */
   protected def setDeclarations(items: List[S]): Unit

   def add(i: S, at: AddPosition = AtEnd) {
     val items = getDeclarations
     // default: insert at end
     def defaultPos = items.length
     val pos = at match {
        case After(a) => items.indexWhere(_.name == a) match {
           case -1 => defaultPos // maybe issue warning that a not found
           case i => i+1
        }
        case Before(a) => items.indexWhere(_.name == a) match {
           case -1 => defaultPos
           case i => i
        }
        case AtBegin => 0
        case AtEnd => items.length
     }
     val (bef,aft) = items.splitAt(pos) // items(pos) == aft.head
     setDeclarations(bef ::: i :: aft)
  }
  /** updates or adds a child */
  def update(s: S) {
     val items = getDeclarations
     val i = items.indexWhere(_.name == s.name)
     if (i != -1)
       setDeclarations(items.take(i) ::: s :: items.drop(i+1))
     else
       add(s)
  }
  /** deletes a child */
  def delete(n: LocalName) = {
     val d = getO(n)
     val items = getDeclarations
     setDeclarations(items.filterNot(_.name == n))
     d
  }
  /** moves ln to the end */
  def reorder(ln: LocalName) {
     val items = getDeclarations
     items.find(_.name == ln) match {
        case Some(i) =>
          delete(ln)
          setDeclarations(items ::: List(i))
        case None => throw ImplementationError("element does not exist")
     }
  }
}

trait ContainerElement[S <: StructuralElement] extends StructuralElement with MutableElementContainer[S] {
   /** the list of declarations in the order of addition, excludes generated declarations */
   def getPrimitiveDeclarations = getDeclarations.filterNot(_.isGenerated)
   /** the list of declarations using elaborated declarations where possible */
   def getDeclarationsElaborated = getDeclarations.filterNot(uom.ElaboratedElement.isProperly)
}

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
