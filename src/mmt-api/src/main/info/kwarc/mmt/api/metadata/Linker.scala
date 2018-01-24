package info.kwarc.mmt.api.metadata
import info.kwarc.mmt.api._

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
  def fromURI(u: utils.URI) : A
  /** convert from URIs to A when reading from metadata */
  def toURI(a: A): utils.URI
  def get(e: HasMetaData) : Option[A] = e.metadata.getLinks(key).headOption.map(fromURI)
  def update(e: HasMetaData, target: A) {
     e.metadata.update(Link(key, toURI(target)))
  }
  def delete(e: HasMetaData) {
     e.metadata.delete(key)
  }
}
