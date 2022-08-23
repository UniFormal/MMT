package info.kwarc.mmt.odk.GAP

import info.kwarc.mmt.api.objects.{OMS, Term}
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.uom._
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.lf.{Apply, ApplySpine}

import scala.collection.mutable

object GAP {
  val _base = DPath(URI.https colon "www.gap-system.org")
  val theory = _base ? "Types"
  val importbase = _base / "mitm"

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

  def dotp(tm : Term, tp : GAPObject) = ApplySpine(OMS(theory ? "hastp"),tm,Translator.objtotype(tp))
  def propfilt(tm : GAPProperty) = Apply(OMS(theory ? "propertyFilter"),tm.toTerm)
  def propfilt(tm : GAPOperation) = Apply(OMS(theory ? "propertyFilter"),tm.toTerm)
  def catfilt(tm : GAPCategory) = Apply(OMS(theory ? "catFilter"),tm.toTerm)
  
  /*
   * from scscp import SCSCPCLI
     client = SCSCPCLI("localhost", port=26133)
     res = client.heads.scscp2.get_allowed_heads([])
     OMApplication(
      elem=OMSymbol(name='symbol_set', cd='scscp1'),
      arguments=[
        OMSymbol(name='MitM_Evaluate', cd='scscp_transient_1'),
        OMSymbol(name='MitM_Quit', cd='scscp_transient_1'),
        OMSymbol(name='MitM_Evaluate', cd='lib', cdbase='https://www.gap-system.org/mitm/')])
     But I can't it get to work with combinations names.
   */
  val scscpHead = DPath(URI("")) ? "scscp_transient_1" ? "MitM_Evaluate"
  

}

object FilterRelations extends RelationalExtractor {
  val Implies = CustomBinary("implies","implies","is implied by")
  val IsFilter = CustomUnary("IsFilter")
  val IsAttribute = CustomUnary("IsAttribute")

  def allUnary = List(IsFilter, IsAttribute)
  def allBinary = List(Implies)

  val gaprels : scala.collection.mutable.HashMap[Constant,List[RelationalElement]] = mutable.HashMap.empty

  def apply(e: StructuralElement)(implicit f: RelationalElement => Unit): Unit = {e match {
    case d: Module => d.getDeclarations foreach {
      case c: Constant => gaprels.getOrElse(c,Nil) foreach f
      case _ =>
    }
    case _ =>
  }}
}

object GAPGraphExporter extends SimpleRelationGraphExporter("gapgraph", "Gap graph", ((Includes | Declares | FilterRelations.Implies)^*) * HasType(IsConstant), List(DependsOn,FilterRelations.Implies)) {
  override def canHandle(path: Path) = path.doc == GAP._base
}

class Plugin extends frontend.Plugin {
  val theory = GAP.theory
  val dependencies = List("info.kwarc.mmt.lf.Plugin")
  override def start(args: List[String]): Unit = {
    controller.extman.addExtension(new GAPJSONImporter)
    controller.extman.addExtension(FilterRelations)
    controller.extman.addExtension(GAPGraphExporter)
    controller.extman.addExtension(new GAPSystem)
  }
}

object Booleans extends RealizedType(OMS(GAP.theory ? "booleans"),StandardBool)
object Integers extends RepresentedRealizedType(OMS(GAP.theory ? "integers"),StandardInt)
object Floats extends RealizedType(OMS(GAP.theory ? "floats"),StandardDouble)
