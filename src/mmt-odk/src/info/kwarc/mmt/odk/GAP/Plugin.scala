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
  val catfamily = OMS(theory ? "CategoryCollection") // TODO
  val setter = OMS(theory ? "Set")

  val obj = OMS(theory ? "object")
  val cat = OMS(theory ? "category")
  val filter = OMS(theory ? "filter")

  def conj(tm1 : GAPFilter, tm2 : GAPFilter) = ApplySpine(OMS(theory ? "filter_and"),tm1.toTerm,tm2.toTerm)
  def termconj(tm1 : Term, tm2 : Term) = ApplySpine(OMS(theory ? "filter_and"),tm1,tm2)

  val IsBool = theory ? "IsBool"
  val IsObject = theory ? "IsObject"

  def dotp(tm : Term, tp : GAPObject) = ApplySpine(OMS(theory ? "#"),tm,Translator.objtotype(tp))
  def propfilt(tm : GAPProperty) = Apply(OMS(theory ? "propertyFilter"),tm.toTerm)
  def propfilt(tm : GAPOperation) = Apply(OMS(theory ? "propertyFilter"),tm.toTerm)
  def catfilt(tm : GAPCategory) = Apply(OMS(theory ? "catFilter"),tm.toTerm)
}

class Plugin extends frontend.Plugin {
  val theory = GAP.theory
  val dependencies = List("info.kwarc.mmt.lf.Plugin")
  override def start(args: List[String]) {
    controller.extman.addExtension(new GAPJSONImporter)
  }
}

object Booleans extends RealizedType(OMS(GAP.theory ? "booleans"),StandardBool)
object Integers extends RealizedType(OMS(GAP.theory ? "integers"),StandardInt)
object Floats extends RealizedType(OMS(GAP.theory ? "floats"),StandardDouble)