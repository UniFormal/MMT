package info.kwarc.mmt.api.execution

import info.kwarc.mmt.api._
import modules._
import symbols._
import objects._
import symbols._

class InstanceOfTheory(env: RuntimeEnvironment, val theory: Theory) extends
      Theory(theory.parent, theory.name / env.nextInstance.toString, theory.meta, Theory.noParams, Theory.noBase) {

  // add a realization of the instantiated theory
  add(Include(toTerm, theory.path, Nil, None, true))

  val id = name.last.toString
  override def toString = s"instance $id of ${theory.path}"

  def get(p: GlobalName) : Option[Term] = {
    val name = p.toLocalName
    getO(name) match {
      case Some(c: Constant) =>
        c.dfC.analyzed
      case Some(_) =>
        throw GetError(path ? name,"not a constant")
      case None =>
        throw GetError(path ? name,"constant does not exist")
    }
  }

  def set(p: GlobalName, t: Term) {
      val name = p.toLocalName
      getO(name) match {
        case Some(c: Constant) =>
          c.dfC.analyzed = t
        case Some(e) =>
          throw AddError(e, "assignment to a non-constant")
        case None =>
          val c = ConstantAssignment(toTerm, name, Nil, Some(t))
          add(c)
      }
   }
}

/** the semantic of all instances, which allows embedding run-time instances into the term language
  *
  * this is the type of all instances, any more fine-granular typing must be done with separate typing rules
  */
object InstanceType extends uom.Atomic[InstanceOfTheory] {
  def asString = "instance"
  val cls = classOf[InstanceOfTheory]
  def fromString(s: String) = throw ImplementationError("runtime instances do not have string representation")
}