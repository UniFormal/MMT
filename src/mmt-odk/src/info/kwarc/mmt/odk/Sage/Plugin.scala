package info.kwarc.mmt.odk.Sage

import info.kwarc.mmt.api.objects.{OMA, OMS, Term}
import info.kwarc.mmt.api.{DPath, frontend}
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.lf.Apply

object Sage {
  val _base = DPath(URI.http colon "www.sagemath.org")
  val docpath : DPath = _base / "content"
  val theory = _base ? "Types"
  val obj = theory ? "object"
  val category = theory ? "category"
  val prop = theory ? "prop"
  val catdoc = docpath / "categories"
  val clssdoc = docpath / "classes"
  val structure = theory ? "structural"
  def structof(tm : String) = Apply(OMS(theory ? "structureof"),OMS(Sage._base ? "Structures" ? tm))
  def ded(tm : Term) = Apply(OMS(theory ? "ded"),tm)
}

class Plugin extends frontend.Plugin {
  val theory = Sage.theory
  val dependencies = List("info.kwarc.mmt.lf.Plugin")
  override def start(args: List[String]) {
    controller.extman.addExtension(new SageImporter)
  }
}
