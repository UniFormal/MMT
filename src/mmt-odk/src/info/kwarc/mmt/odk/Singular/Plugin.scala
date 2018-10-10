package info.kwarc.mmt.odk.Singular

import info.kwarc.mmt.api.frontend.Extension

class Plugin extends Extension {
  val dependencies = List("info.kwarc.mmt.lf.Plugin")
  override def start(args: List[String]) {
    controller.extman.addExtension(new SingularImporter)
    controller.extman.addExtension(new SingularSystem)
  }
}
