package info.kwarc.mmt.odk.GAP

import info.kwarc.mmt.api.objects.{OMS, Term}
import info.kwarc.mmt.api.{DPath, NamespaceMap, Path, frontend}
import info.kwarc.mmt.api.uom.{RealizedType, StandardBool, StandardDouble, StandardInt}
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.lf.{Apply, ApplySpine}
import info.kwarc.mmt.odk.Math

object GAP {
  val _base = DPath(URI.http colon "www.gap-system.org")
  val theory = _base ? "Types"

  val has = OMS(theory ? "Has")
  val get = OMS(theory ? "Is")
  val tester = OMS(theory ? "Tester")
  val catcollection = OMS(theory ? "CategoryCollection")
  val setter = OMS(theory ? "Set")

  val obj = OMS(theory ? "object")
  val cat = OMS(theory ? "category")
  val filter = OMS(theory ? "filter")

  val IsBool = OMS(theory ? "IsBool")

  def dotp(tm : Term, tp : Term) = ApplySpine(OMS(theory ? "#"),tm,tp)
  def propfilt(tm : Term) = Apply(OMS(theory ? "propertyFilter"),tm)
  def catfilt(tm : Term) = Apply(OMS(theory ? "catFilter"),tm)
}

class Plugin extends frontend.Plugin {
  val theory = GAP.theory
  val dependencies = List("info.kwarc.mmt.lf.Plugin")
  override def start(args: List[String]) {
    controller.extman.addExtension(new JSONImporter)
  }
}

object Booleans extends RealizedType(OMS(GAP.theory ? "booleans"),StandardBool)
object Integers extends RealizedType(OMS(GAP.theory ? "integers"),StandardInt)
object Floats extends RealizedType(OMS(GAP.theory ? "floats"),StandardDouble)