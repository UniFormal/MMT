package info.kwarc.mmt.frameit.business.datastructures

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.{ContentElement, GlobalName, MPath}
import info.kwarc.mmt.api.metadata.{MetaData, MetaDatum}
import info.kwarc.mmt.api.modules.View
import info.kwarc.mmt.api.objects.{OMMOD, Term}
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld.MetaAnnotations
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld.MetaAnnotations.MetaKeys
import info.kwarc.mmt.frameit.archives.MitM.Foundation.StringLiterals
import info.kwarc.mmt.frameit.business.InvalidMetaData

import scala.util.Try

sealed case class UserMetadata(label: Term, description: Term) {
  private[datastructures] def render(viewRenderer: ScrollViewRenderer): UserMetadata = {
    this.copy(
      label = viewRenderer(label),
      description = viewRenderer(description)
    )
  }
}

object UserMetadata {
  def parse(element: ContentElement): UserMetadata = {
    // todo: issue warnings when multiple label/desc meta datums are present (currently it'll lead to "<no label/desc>")
    UserMetadata(
      Try(MetadataUtils.readTermMetaDatum(element.metadata, MetaKeys.label)).getOrElse(StringLiterals("<no label>")),
      Try(MetadataUtils.readTermMetaDatum(element.metadata, MetaKeys.description)).getOrElse(StringLiterals("<no desc>")),
    )
  }
}

/**
  * todo: rework this with nice apply/unapply methods, possibly integrate into MMT
  *       seems one often needs to read/write metadata
  */
object MetadataUtils {
  def readTermMetaDatum(metadata: MetaData, key: GlobalName): Term = readSingleMetaDatum(metadata, key).value match {
    case x: Term => x
    case x => throw InvalidMetaData(s"Expected Term matching for ${key}, but got ${x}")
  }

  def readSingleMetaDatum(metadata: MetaData, key: GlobalName): MetaDatum = metadata.get(key) match {
    case Nil => throw InvalidMetaData(s"No metadatum found for ${key} (expected exactly one matching metadatum entry)")
    case List(datum) => datum
    case _ => throw InvalidMetaData(s"Multiple metadatums matching for ${key} (expected exactly one matching metadatum entry)")
  }

  def readMPathMetaDatum(metadata: MetaData, key: GlobalName): MPath = readSingleMetaDatum(metadata, key).value match {
    case OMMOD(mpath) => mpath
    case x => throw InvalidMetaData(s"Expected MPath matching for ${key}, but got ${x}")
  }

  def readStringMetaDatum(metadata: MetaData, key: GlobalName): String = readSingleMetaDatum(metadata, key).value match {
    case StringLiterals(str) => str
    case x => throw InvalidMetaData(s"Expected string literal matching for ${key}, but got ${x}")
  }
}
