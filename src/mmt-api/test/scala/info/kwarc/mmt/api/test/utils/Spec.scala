package info.kwarc.mmt.api.test.utils

/** shared trait for specs */
trait Spec

/** an extension to be tested */
case class ExtensionSpec(name: String, args: String*)

/** an archive to be installed */
case class ArchiveSpec(id: String, version: String = "master") extends Spec

/** versions of archives to be installed */
object ArchiveSpec {
  object MMT {
    val urtheories = ArchiveSpec("MMT/urtheories")
    val examples = ArchiveSpec("MMT/examples")
    val LFX = ArchiveSpec("MMT/LFX")
  }
  object Test {
    val General = ArchiveSpec("Test/General")
  }
}