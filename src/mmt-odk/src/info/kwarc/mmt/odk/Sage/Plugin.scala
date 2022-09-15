package info.kwarc.mmt.odk.Sage

import info.kwarc.mmt.api.objects.{OMA, OMS, Term}
import info.kwarc.mmt.api.{DPath, frontend}
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.lf.Apply

object Python {
  val _base = DPath(URI.http colon "python.org")
  val cd = _base ? "Python"
  val dict = cd ? "dict"
  val list = cd ? "list"
  val tuple = cd ? "tuple"
  val dot = cd ? "getattr"
}

object Sage {
  val metabase = DPath(URI.http colon "python.org")
  val _base = Python._base
  val docpath : DPath = _base // "content"
  val theory = metabase ? "Types"
  val obj = theory ? "object"
  val category = theory ? "category"
  val prop = theory ? "prop"
  val catdoc = docpath // "categories"
  val clssdoc = docpath // "classes"
  val structure = theory ? "structural"
  def structof(tm : String) = Apply(OMS(theory ? "structureof"),OMS(Sage._base ? "Structures" ? tm))
  def ded(tm : Term) = Apply(OMS(theory ? "ded"),tm)
}

class Plugin extends frontend.Plugin {
  val theory = Sage.theory
  val dependencies = List("info.kwarc.mmt.lf.Plugin")
  override def start(args: List[String]): Unit = {
    controller.extman.addExtension(new SageImporter)
    controller.extman.addExtension(new SageSystem)
  }
}
