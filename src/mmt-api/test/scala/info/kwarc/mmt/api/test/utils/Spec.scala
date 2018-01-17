package info.kwarc.mmt.api.test.utils

import info.kwarc.mmt.api.frontend.actions.OAFClone

/** shared trait for specs */
trait Spec

/** an extension to be tested */
case class ExtensionSpec(name: String, args: String*)

/** an archive to be installed */
case class ArchiveSpec(id: String, version: String) extends Spec {
  def toAction: OAFClone = OAFClone(id, Some(version))
}

/**
  * versions of archives to be installed
  */
object ArchiveSpec {
  // to get the current version number, push local changes and then run
  //      oaf show <archive>
  // from an MMT shell.
  // If this complains, make sure that an oaf root is configured; either via setup, or manually by
  //      oaf root <folder> [https|ssh]
  // from an MMT shell and that your archive is installed via git.
  object MMT {
    val urtheories = ArchiveSpec("MMT/urtheories", "2dfd28b7ad17f129a8b377975ef508d0bde69eb2")
    val examples = ArchiveSpec("MMT/examples", "43946da0909a5fd01d54d1838133e0fd0afe4377")
    val LFX = ArchiveSpec("MMT/LFX", "5b7674558b46330d6e10232fc1789960438bfb0a")
  }
  object Test {
    val General = ArchiveSpec("Test/General", "fb07b504f6e0a9f7a58c353983953f0932166a00")
  }
}