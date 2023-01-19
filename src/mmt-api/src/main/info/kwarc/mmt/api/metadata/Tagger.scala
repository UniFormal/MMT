package info.kwarc.mmt.api.metadata
import info.kwarc.mmt.api._

/**
 * convenience to access a certain tag
 *
 * instantiate with the tag and then apply the methods to instances of HasMetaData
 */
class Tagger(key: GlobalName) {
  def set(e: HasMetaData): Unit = {
     e.metadata.add(Tag(key))
  }
  def get(e: HasMetaData) = e.metadata.getTags contains key
  def apply(e: HasMetaData) = get(e)
}
