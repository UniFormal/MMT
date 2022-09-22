package info.kwarc.mmt.api.metadata

import info.kwarc.mmt.api._
import objects._

/**
 * convenience to access the metadata for a specific key
 *
 * @param key the key
 *
 * instantiate with the key and then apply the methods to instances of HasMetaData
 */
abstract class Annotator[A](val key: GlobalName) {
  /** convert from A into objects to allow ascription as metadata */
  def fromObject(o: Obj) : A
  /** convert from objects to A when reading from metadata */
  def toObject(a: A): Obj
  def get(e: HasMetaData) : Option[A] = e.metadata.getValues(key).headOption.map(fromObject)
  def update(e: HasMetaData, value: A): Unit = {
     e.metadata.add(new MetaDatum(key, toObject(value)))
  }
  def delete(e: HasMetaData): Unit = {
     e.metadata.delete(key)
  }
}

class TermAnnotator(k: GlobalName) extends Annotator[Term](k) {
  def toObject(t: Term) = t
  def fromObject(o: Obj) = o match {
    case t: Term => t
    case _ => throw GeneralError("not a term")
  }
}

class StringAnnotator(key: GlobalName) extends Annotator[String](key) {
  private val Str = uom.OMLiteral.OMSTR 
  def fromObject(o: Obj) = Str.unapply(o.asInstanceOf[Term]).get
  def toObject(s: String) = Str(s)  
}

/** special case where we link to an MMT URI, either as a URI or an OMID */
class GlobalNameLinker(key: GlobalName) extends Annotator[GlobalName](key) {
  def fromObject(o: Obj) = o match {
    case OMS(p) => p
    case uom.OMLiteral.URI(u) => Path.fromURI(u, NamespaceMap.empty) match {
      case g: GlobalName => g
      case _ => throw GeneralError("not a GlobalName")
    }
  }
  def toObject(g: GlobalName) = OMS(g)  
}