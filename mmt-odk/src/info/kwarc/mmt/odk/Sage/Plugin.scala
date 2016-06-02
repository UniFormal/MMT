package info.kwarc.mmt.odk.Sage

import info.kwarc.mmt.api.{DPath, frontend}
import info.kwarc.mmt.api.utils.URI

object Sage {
  val _base = DPath(URI.http colon "www.sagemath.org")
  val theory = _base ? "Types"
  val category = theory ? "category"
}

class Plugin extends frontend.Plugin {
  val theory = Sage.theory
  val dependencies = List("info.kwarc.mmt.lf.Plugin")
  override def start(args: List[String]) {
    controller.extman.addExtension(new SageImporter)
  }
}