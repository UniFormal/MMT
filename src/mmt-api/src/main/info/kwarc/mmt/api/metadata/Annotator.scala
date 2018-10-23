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
  def update(e: HasMetaData, value: A) {
     e.metadata.add(new MetaDatum(key, toObject(value)))
  }
  def delete(e: HasMetaData) {
     e.metadata.delete(key)
  }
}

class StringAnnotator(key: GlobalName) extends Annotator[String](key) {
  private val Str = uom.OMLiteral.OMSTR 
  def fromObject(o: Obj) = Str.unapply(o.asInstanceOf[Term]).get
  def toObject(s: String) = Str(s)  
}