package info.kwarc.mmt.api

import objects._
import utils.URI
import scala.collection.mutable.ListMap


/** a trait to be mixed into declarations or objects
 * it provides a stateful key-value map for storing arbitrary information
 *
 * This trait introduces state into the stateless Obj case classes and should be used cautiously.
 * For example, x match {case x => x} preserves all client properties while
 * x match {case OMS(p) => OMS(p)} deletes all client properties.
 * Software components should only access properties they put themselves or
 * that are explicitly guaranteed by other components.
 *
 * While a bad idea in most situations, client properties are very useful occasionally.
 * Example applications:
 *   - UOM uses a property to remember whether a Term has been simplified already
 *   - UOM uses a property to remember what kind of change a simplification has produced
 */
trait ClientProperties {
   lazy val clientProperty = new ListMap[URI, Any]
}

/** convenience to create get and put methods for objects with ClientProperties
 *  @param property the client property for which to generate get and put methods
 */
class ClientProperty[-C <: ClientProperties,A](val property: URI) {
   /** put the client property */
   def put(t: C, a:A): Unit = {
      t.clientProperty(property) = a
   }
   /** get the client property if defined */
   def get(t: C): Option[A] = t.clientProperty.get(property) match {
      case Some(a: A @unchecked) =>  // :A is unchecked but true if put was used to set the property
         Some(a)
      case None => None
      case Some(_) =>
         throw ImplementationError("client property has bad type") // impossible if put was used
   }
   def erase(t: C): Unit = {
      t.clientProperty -= property
   }
}

/**
 * apply/unapply methods that encapsulate functionality for attaching a Boolean clientProperty to a Term
 */
class BooleanClientProperty[-C <: ClientProperties](p: URI) extends ClientProperty[C,Boolean](p){
   def apply(t: C) : t.type = {
     put(t, true)
     t
   }
   def unapply(t: C): Option[t.type] =
      get(t) match {
          case Some(true) => Some(t)
          case _ => None
      }
   def set(t: C): Unit = {
     put(t, true)
   }
   def unset(t: C): Unit = {
     put(t, false)
   }
   def is(t: C) = get(t) contains true
}


class TermProperty[A](p: URI) extends ClientProperty[Obj, A](p) {
  def eraseDeep(o: Obj): Unit = {
    erase(o)
    o.subobjects foreach {case (_,so) => eraseDeep(so)}
  }
}
class BooleanTermProperty(p: URI) extends BooleanClientProperty[Obj](p)

object TermProperty {
   /** removes all term properties recursively (not a Traverser to avoid copying the Term) */
   def eraseAll(t: Term): Unit = {
      t.clientProperty.clear()
      t match {
        case OMA(f,as) => (f::as) foreach eraseAll
        case OMBINDC(b,cont,as) =>
          (b::as) foreach eraseAll
          cont foreach {v =>
            v.tp foreach eraseAll
            v.df foreach eraseAll
          }
         case o:OML =>
            o.tp foreach eraseAll
            o.df foreach eraseAll
         case o:OMLIT =>
           eraseAll(o.rt.synType)
         case _ =>
      }
   }
}
