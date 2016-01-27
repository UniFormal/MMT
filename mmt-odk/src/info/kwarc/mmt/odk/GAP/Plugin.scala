package info.kwarc.mmt.odk.GAP

import info.kwarc.mmt.api.frontend
import info.kwarc.mmt.odk.Math

class Plugin extends frontend.Plugin {
  val theory = Math.path
  val dependencies = List("info.kwarc.mmt.lf.Plugin")
  override def start(args: List[String]) {
    controller.extman.addExtension(JSONImporter)
  }
}