package info.kwarc.mmt.odk.Sage

import info.kwarc.mmt.api.objects.{OMA, OMS, Term}
import info.kwarc.mmt.api.{DPath, frontend}
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.lf.Apply

object Sage {
  val _base = DPath(URI.http colon "www.sagemath.org")
  val docpath = _base / "content"
  val theory = _base ? "Types"
  val category = theory ? "category"
  val prop = theory ? "prop"
  def ded(tm : Term) = Apply(OMS(theory ? "ded"),tm)
}

class Plugin extends frontend.Plugin {
  val theory = Sage.theory
  val dependencies = List("info.kwarc.mmt.lf.Plugin")
  override def start(args: List[String]) {
    controller.extman.addExtension(new SageImporter)
  }
}