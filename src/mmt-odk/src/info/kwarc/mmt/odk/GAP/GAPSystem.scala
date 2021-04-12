package info.kwarc.mmt.odk.GAP

import info.kwarc.mmt.MitM.{MitM, MitMSystems}
import info.kwarc.mmt.MitM.VRESystem.VREWithAlignmentAndSCSCP
import info.kwarc.mmt.api.objects.{OMA, OMS, Term}
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.refactoring._
import info.kwarc.mmt.odk.{IntegerLiterals}
import info.kwarc.mmt.odk.LFX.LFList
import info.kwarc.mmt.odk.OpenMath.OMSymbol

/** translation to be used in [[GAPSystem]] */
object GAPTranslations {
  object AnyInt {
    def unapply(tm : Term): Option[BigInt] = tm match {
      case Integers(i : BigInt) => Some(i)
      case IntegerLiterals(i : BigInt) => Some(i)
      case info.kwarc.mmt.api.objects.UnknownOMLIT(s, _) => unapply(IntegerLiterals.parse(s))
      case _ => None
    }
  }

  object GAPList {
    val gaplist = GAP.importbase ? "prim" ? "ListConstr"
    def unapply(tm : Term) = tm match {
      case OMA(OMS(`gaplist`),ls) => Some(ls)
      case _ => None
    }
  }

  object Poly {
    val psym = GAP.importbase ? "lib" ? "PolynomialByExtRep"
    val ratfunfam = GAP.importbase ? "lib" ? "RationalFunctionsFamily"
    val famobj = GAP.importbase ? "lib" ? "FamilyObj"

    def unapply(tm : Term) = tm match {
      case OMA(OMS(`psym`),List(OMA(OMS(`ratfunfam`),List(OMA(OMS(`famobj`),List(_)))),GAPList(ls))) => Some(ls)
      case OMA(OMS(`psym`),List(OMA(OMS(`ratfunfam`),List(OMA(OMS(`famobj`),List(_)))),LFList(ls))) => Some(ls)
      case _ => None
    }
    def apply(ls : List[Term]) =
      OMA(OMS(`psym`),List(OMA(OMS(`ratfunfam`),List(OMA(OMS(`famobj`),List(Integers(1))))),LFList(ls)))
  }


  val dihedral = GAP.importbase ? "lib" ? "DihedralGroup"
  val ispermgroup = GAP.importbase ? "lib" ? "IsPermGroup"
  val toDihedralGroup = new AcrossLibraryTranslation {
    override def applicable(tm: Term)(implicit translator: AcrossLibraryTranslator): Boolean = tm match {
      case OMA(OMS(MitM.dihedralGroup),AnyInt(_):: Nil) =>
        true
      case _ => false
    }

    override def apply(tm: Term)(implicit translator: AcrossLibraryTranslator): Term = tm match {
      case OMA(OMS(MitM.dihedralGroup),AnyInt(i):: Nil) =>
        OMA(OMS(dihedral),OMS(ispermgroup) :: Integers(2*i) :: Nil)
      case _ => ???
    }
  }

  def fromDihedralGroup : AcrossLibraryTranslation = ???

  val orbit = GAP.importbase ? "lib" ? "Orbit"
  val onindet = GAP.importbase ? "lib" ? "OnIndeterminates"

  val toPolyOrbit = new AcrossLibraryTranslation {
    override def applicable(tm: Term)(implicit translator: AcrossLibraryTranslator): Boolean = tm match {
      case OMA(OMS(MitM.polyOrbit), _ :: _ :: Nil) => true
      case _ => false
    }

    override def apply(tm: Term)(implicit translator: AcrossLibraryTranslator): Term = tm match {
      case OMA(OMS(MitM.polyOrbit),group :: poly :: Nil) =>
        OMA(OMS(orbit),group :: poly :: OMS(onindet):: Nil)
      case _ => ???
    }
  }
  def fromPolyOrbit : AcrossLibraryTranslation = ???
}

/** external computation provided by the GAP system */
class GAPSystem extends VREWithAlignmentAndSCSCP("GAP", MitMSystems.gapsym, GAP.scscpHead, "ODK/GAP") {
  import GAPTranslations._

  private object variables {
    private var ivars : List[String] = Nil
    def get(s:String) = if (ivars.indexOf(s) != -1) ivars.indexOf(s)+1 else {
      ivars = ivars ::: List(s)
      ivars.indexOf(s)+1
    }
    def get(i : Int): String = if (0 <= i-1 && i-1 < ivars.length) ivars(i-1) else {
      get(i-1)
      ivars = ivars ::: List("x" + i)
      ivars(i-1)
    }
  }

  val toPolynomials = new AcrossLibraryTranslation {
    override def applicable(tm: Term)(implicit translator: AcrossLibraryTranslator): Boolean = tm match {
      case MitM.MultiPolynomial(_,_) => true
      case _ => false
    }


    override def apply(tm: Term)(implicit translator: AcrossLibraryTranslator): Term = tm match {
      case MitM.MultiPolynomial(_,ls) =>
        Poly(ls.flatMap{
          case (vars,coeff,_) =>
            LFList(vars.flatMap(p => List(Integers(variables.get(p._1)),Integers(p._2)))) :: Integers(coeff) :: Nil
        })
    }
  }
  val fromPolynomials = new AcrossLibraryTranslation {
    override def applicable(tm: Term)(implicit translator: AcrossLibraryTranslator): Boolean = tm match {
      case Poly(ls) => true
      case _ => false
    }

    override def apply(tm: Term)(implicit translator: AcrossLibraryTranslator): Term = tm match {
      case Poly(ls) =>
        MitM.MultiPolynomial(OMS(MitM.rationalRing),deconstruct(ls))
      case _ => ???
    }

    private def deconstruct(ls : List[Term]) : List[Term] = {
      if (ls.isEmpty) Nil
      else if (ls.length == 1) ???
      else {
        val ils : List[Term] = LFList.unapply(ls.head).getOrElse(GAPList.unapply(ls.head).getOrElse(???))
        var i = 0
        var iret : List[(String,BigInt)] = Nil
        while (i < ils.length - 1) {
          val s = {
            val ind = AnyInt.unapply(ils(i)).getOrElse(???)
            variables.get(ind.toInt)
          }//"x" + AnyInt.unapply(ils(i)).getOrElse(???)
          val in : BigInt = ils(i+1) match {
            case AnyInt(n) => n
            case _ => ???
          }
          iret ::= (s,in)
          i+=2
        }
        MitM.Monomial(iret,AnyInt.unapply(ls(1)).getOrElse(???)) :: deconstruct(ls.tail.tail)
      }
    }
  }

  override val fromTranslations: List[AcrossLibraryTranslation] = fromPolynomials :: super.fromTranslations
  override val toTranslations: List[AcrossLibraryTranslation] = toPolyOrbit :: toDihedralGroup :: toPolynomials :: super.toTranslations
  
  override lazy val translator_to = translator(new TranslationTarget {
    override def inTarget(path: GlobalName, controller: Controller): Boolean = GAP._base <= path
  },toTranslations)

}