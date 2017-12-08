package info.kwarc.mmt.api.utils

import scala.collection.mutable
import scala.reflect.ClassTag


/**
  * Abstract class to be used as a class for a companion object to be tracked by a [[AccessibleCompanionCollection]]
  * @param ct Implicitly generated [[ClassTag]] for this [[AccessibleCompanion]]
  * @tparam T  class that this is a Companion Object for
\  */
abstract class AccessibleCompanion[+T](implicit ct: ClassTag[T]) {
  /** A boolean indicating if this action should be registered automatically */
  protected[utils] val autoRegister : Boolean = true

  /** get a runtime representation of the class of a companion object */
  val companionClass : Class[_] = ct.runtimeClass
}

/** Represents the companion of a singleton */
trait SingletonAccessibleCompanion[+T <: Singleton] extends AccessibleCompanion[T] {
  /** the sole instance of this Companion */
  lazy val companionInstance : T = companionClass.getField("MODULE$").get(companionClass).asInstanceOf[T]
}

/**
  * Represents a collection of [[AccessibleCompanion]] objects
  * @tparam CompanionType the conrete type of [[AccessibleCompanion]]s this collection is for
  * @tparam T type of class to store companions for
  */
abstract class AccessibleCompanionCollection[T, CompanionType <: AccessibleCompanion[T]] {

  /** internal representation of the list of accessible companions */
  private val companions: mutable.ListBuffer[CompanionType] = new mutable.ListBuffer

  /** registers an [[AccessibleCompanion]] with this collection */
  protected def register(companion: CompanionType): Unit = {
    if(companion.autoRegister){
      this.companions += companion
    }
  }

  /** public (read-only) list of companions */
  lazy val all: List[CompanionType] = companions.toList

  /** finds an [[AccessibleCompanion]] belonging to a given class */
  def find(cls : Class[_]) : CompanionType = all.find(_.companionClass == cls).get
  /** finds an [[AccessibleCompanion]] belonging to the given class of an instance */
  def find(instance: T) : CompanionType = find(instance.getClass)
  /** finds all [[AccessibleCompanion]] matching a predicate */
  def filter(p: CompanionType => Boolean) : List[CompanionType] = all.filter(p)
}