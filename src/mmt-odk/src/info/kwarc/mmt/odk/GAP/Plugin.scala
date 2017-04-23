package info.kwarc.mmt.odk.GAP

import info.kwarc.mmt.api.objects.{OMS, Term}
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules.DeclaredModule
import info.kwarc.mmt.api.ontology.QueryEvaluator.QuerySubstitution
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.uom.{RealizedType, StandardBool, StandardDouble, StandardInt}
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.lf.{Apply, ApplySpine}
import info.kwarc.mmt.odk.SCSCP.Client.SCSCPClient
import info.kwarc.mmt.odk.{AlignmentBasedMitMTranslation, Math, UsesSCSCP, VRESystem}

import scala.collection.mutable

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

object FilterRelations extends RelationalExtractor {
  val Implies = CustomBinary("implies","implies","is implied by")
  val IsFilter = CustomUnary("IsFilter")
  val IsAttribute = CustomUnary("IsAttribute")

  def allUnary = List(IsFilter, IsAttribute)
  def allBinary = List(Implies)

  val gaprels : scala.collection.mutable.HashMap[Constant,List[RelationalElement]] = mutable.HashMap.empty

  def apply(e: StructuralElement)(implicit f: RelationalElement => Unit): Unit = e match {
    case d: DeclaredModule => d.getDeclarations foreach {
      case c: Constant => gaprels.getOrElse(c,Nil) foreach f
      case _ =>
    }
    case _ =>
  }
}

object GAPGraphExporter extends SimpleRelationGraphExporter("gapgraph", ((Includes | Declares | FilterRelations.Implies)^*) * HasType(IsConstant), List(DependsOn,FilterRelations.Implies)) {
  override def canHandle(path: Path) = path.doc == GAP._base
}

class Plugin extends frontend.Plugin {
  val theory = GAP.theory
  val dependencies = List("info.kwarc.mmt.lf.Plugin")
  override def start(args: List[String]) {
    controller.extman.addExtension(new GAPJSONImporter)
    controller.extman.addExtension(FilterRelations)
    controller.extman.addExtension(GAPGraphExporter)
    controller.extman.addExtension(GAPSystem)
  }
}

object Booleans extends RealizedType(OMS(GAP.theory ? "booleans"),StandardBool)
object Integers extends RealizedType(OMS(GAP.theory ? "integers"),StandardInt)
object Floats extends RealizedType(OMS(GAP.theory ? "floats"),StandardDouble)

object GAPSystem extends VRESystem("GAP") with AlignmentBasedMitMTranslation with UsesSCSCP {
  val serverurl = "neptune.eecs.jacobs-university.de"
  val namespace = GAP._base

  def evaluate(q: Query, e: QueryEvaluator)(implicit substitution: QuerySubstitution): mutable.HashSet[List[BaseType]] = ???
}