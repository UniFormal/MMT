package info.kwarc.mmt.api.archives.lmh

import info.kwarc.mmt.api.utils.MMTSystem

import scala.collection.mutable.ListBuffer

/** represents a versioning of MMT Archives */
abstract class ArchiveVersioning {
  /** returns a list of all coded archive versions */
  val all: List[(String, String)]

  /** gets the version of a given MMT archive */
  def apply(archive: String): Option[String] = all.find(_._1 == archive).map(_._2)

  /** resolve a given version specification */
  def apply(archive: String, version: Option[String]) : Option[String] = version match {
    case Some(v) => Some(v)
    case None => apply(archive)
  }
}

object EmptyVersioning extends ArchiveVersioning {
  val all: List[(String, String)] = Nil
}

object StandardVersioning extends ArchiveVersioning {
  final private val lineRegex = "^\\s*([^\\#\\s]+)\\s+([^\\#\\s]+)\\s*$".r

  /** returns a list of all coded archive versions */
  val all: List[(String, String)] = {
    val buffer = new ListBuffer[(String, String)]
    MMTSystem.getResourceAsString("/archives/archive_versions.txt").split("\n") foreach {l => lineRegex.findFirstMatchIn(l).foreach { m =>
      buffer.append((m.group(1), m.group(2)))
    }}
    buffer.toList
  }
}