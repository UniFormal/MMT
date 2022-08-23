package info.kwarc.mmt.api.metadata
import info.kwarc.mmt.api._
import utils._

/**
 * convenience to access the metadata for a specific key
 *
 * @param A the type of objects to link to
 * @param key the key
 *
 * instantiate with the key and then apply the methods to instances of HasMetaData
 */
abstract class Linker[A](key: GlobalName) {
  /** convert from A into URIs to allow ascription as metadata */
  def fromURI(u: URI) : A
  /** convert from URIs to A when reading from metadata */
  def toURI(a: A): URI
  def get(e: HasMetaData) : Option[A] = e.metadata.getLinks(key).headOption.map(fromURI)
  def update(e: HasMetaData, target: A): Unit = {
     e.metadata.update(Link(key, toURI(target)))
  }
  def delete(e: HasMetaData): Unit = {
     e.metadata.delete(key)
  }
  def copy(from:HasMetaData,to:HasMetaData) = {
    get(from).foreach(md => update(to,md))
  }
}

/** special case where we link to a plain URI */
class URILinker(key: GlobalName) extends Linker[URI](key) {
  def fromURI(u: URI) = u
  def toURI(u: URI) = u
}
