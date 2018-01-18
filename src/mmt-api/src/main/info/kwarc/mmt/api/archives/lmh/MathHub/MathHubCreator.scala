package info.kwarc.mmt.api.archives.lmh.MathHub

import info.kwarc.mmt.api.archives.lmh.ArchiveHubEntry
import info.kwarc.mmt.api.utils.File

/** implements MathHub creation functionality */
trait MathHubCreator {
  self: MathHub =>

  /** creates a new repository of the given ID */
  def createEntry(id: String): Option[ArchiveHubEntry] = {

    // find the local root folder to create the repository in
    // and return nothing it nothing is created
    val root  = local_(id)

    if(root.exists){
      return None
    }

    // and return the repository
    root.mkdirs

    // create all the files needed by the repository
    // TODO: Build a proper templating system into the resources folder
    File.WriteLineWise(root / "META-INF" / "MANIFEST.MF", List(
      s"id: $id",
      s"narration-base: http://mathhub.info/$id"
    )
    )
    File.write(root / "source" / "README.txt", "commit your sources in this folder")

    // create a git repository
    git(root, "init")

    git(root, "add", (root / "META-INF" / "MANIFEST.MF").toString)
    git(root, "add", (root / "source" / "README.txt").toString)

    git(root, "commit", "-m", "\"automatically created by MMT\"")
    git(root, "remote", "add", "origin", remote_(id))

    // and return the archive we just created
    Some(new MathHubEntry(root))
  }
}
