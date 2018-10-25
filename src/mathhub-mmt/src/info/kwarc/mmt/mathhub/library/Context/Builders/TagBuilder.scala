package info.kwarc.mmt.mathhub.library.Context.Builders

import info.kwarc.mmt.api.archives.lmh.{LMHHubArchiveEntry, LMHHubGroupEntry}
import info.kwarc.mmt.mathhub.library.Context.MathHubAPIContext
import info.kwarc.mmt.mathhub.library.{ITag, ITagRef}

trait TagBuilder { this: Builder =>

  /** tries to find a group with the given id */
  protected def tryTag(id: String) : Option[String] = {
    log(s"trying $id as tag")

    if(id.startsWith("@")) {
      Some(id.substring(1))
    } else {
      None
    }
  }

  /** gets a reference to a tag */
  def getTagRef(id: String): Option[ITagRef] = getReferenceOf(classOf[ITagRef], id)

  /** builds a reference to a tag group */
  protected def buildTagReference(tag: String) : Option[ITagRef] = {
    Some(ITagRef("@" + tag, tag))
  }

  /**gets a tag */
  def getTag(id: String): Option[ITag] = getObjectOf(classOf[ITag], id)

  /** builds a tag */
  protected def buildTag(tag: String): Option[ITag] = {
    val ref = getTagRef("@"+tag).getOrElse(return buildFailure(tag, "getTagRef(tag)"))

    val entries = mathHub.entries()
      .collect({ case l: LMHHubArchiveEntry if l.tags.contains(ref.name) => l })
      .map({ e => getArchiveRef(e.id).getOrElse(return buildFailure(tag, s"getArchiveRef(tag.member[${e.id}]")) })

    Some(ITag(
      ref.id, ref.name,
      None,
      entries
    ))
  }
}
