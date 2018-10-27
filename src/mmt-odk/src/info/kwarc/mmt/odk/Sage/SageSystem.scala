package info.kwarc.mmt.odk.Sage

import info.kwarc.mmt.MitM.VRESystem.VREWithAlignmentAndSCSCP
import info.kwarc.mmt.MitM.{MitM, MitMSystems}
import info.kwarc.mmt.api.GlobalName
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.refactoring.{AcrossLibraryTranslation, AcrossLibraryTranslator, TranslationTarget}
import info.kwarc.mmt.lf.ApplySpine
import info.kwarc.mmt.odk.LFX.LFList
import info.kwarc.mmt.odk.OpenMath.OMSymbol
import info.kwarc.mmt.odk.{IntegerLiterals, StringLiterals}
import info.kwarc.mmt.sequences.Sequences

/** translations to be used in SageSystem */
object SageTranslations {
  private val nf = Sage.docpath ? """sage.rings.number_field.number_field""" ? "NumberField"
  private val mitmnf = (MitM.basepath / "smglom" / "algebra") ? "NumberSpaces" ? "numberField"
  private val poly = Sage.docpath ? "sage.rings.polynomial.polynomial_element" ? "Polynomial"
  private val polyring = Sage.docpath ? ".rings.polynomial.polynomial_ring_constructor" ? "PolynomialRing"
  val numberfieldsTo = new AcrossLibraryTranslation {
    override def applicable(tm: Term)(implicit translator: AcrossLibraryTranslator): Boolean = tm match {
      case OMA(OMS(`mitmnf`),a::Nil) => true
      case _ => false
    }

    override def apply(tm: Term)(implicit translator: AcrossLibraryTranslator): Term = tm match {
      case OMA(OMS(`mitmnf`),a::Nil) =>
        OMA(OMS(`nf`),List(a,StringLiterals("x")))
    }
  }
  val numberfieldsFrom = new AcrossLibraryTranslation {
    override def applicable(tm: Term)(implicit translator: AcrossLibraryTranslator): Boolean = tm match {
      case OMA(OMS(`nf`),a::x::Nil) => true
      case _ => false
    }

    override def apply(tm: Term)(implicit translator: AcrossLibraryTranslator): Term = tm match {
      case OMA(OMS(`nf`),a::_:: Nil) =>
        OMA(OMS(`mitmnf`),List(a))
    }
  }
  val multipolyTo = new AcrossLibraryTranslation {
    override def applicable(tm: Term)(implicit translator: AcrossLibraryTranslator): Boolean = tm match {
      case OMA(OMS(MitM.multi_polycon),ring :: ls) if ls.forall(MitM.Monomial.unapply(_).isDefined) => true
      case _ => false
    }

    override def apply(tm: Term)(implicit translator: AcrossLibraryTranslator): Term = tm match {
      case OMA(OMS(MitM.multi_polycon), ring :: ls) if ls.forall(MitM.Monomial.unapply(_).isDefined) =>
        OMA(OMS(`poly`),ring :: LFList(ls.map{
          case MitM.Monomial(vars,coeff,_) =>
            LFList(LFList(vars.map(p => LFList(StringLiterals(p._1) :: IntegerLiterals(p._2) :: Nil))) :: IntegerLiterals(coeff) :: Nil)
        }) :: Nil)
    }
  }
  val multipolyFrom = new AcrossLibraryTranslation {
    override def applicable(tm: Term)(implicit translator: AcrossLibraryTranslator): Boolean = tm match {
      case OMA(OMS(`poly`),ring:: LFList(ls) :: Nil) =>
        ls forall {
          case LFList(LFList(vars) :: IntegerLiterals(coeff) :: Nil) =>
            vars forall {
              case LFList(StringLiterals(v) :: IntegerLiterals(i) :: Nil) => true
              case _ => false
            }
          case _ => false
        }
      case _ => false
    }

    override def apply(tm: Term)(implicit translator: AcrossLibraryTranslator): Term = tm match {
      case OMA(OMS(`poly`), ring :: LFList(ls) :: Nil) =>
        OMA(OMS(MitM.multi_polycon), ring :: ls map {
          case LFList(LFList(vars) :: IntegerLiterals(coeff) :: Nil) =>
            MitM.Monomial(vars map {
              case LFList(StringLiterals(v) :: IntegerLiterals(i) :: Nil) =>
                (v, i)
              case _ => ???
            }, coeff, ring)
          case _ => ???
        })
    }
  }
  val polyTo = new AcrossLibraryTranslation {
    override def applicable(tm: Term)(implicit translator: AcrossLibraryTranslator): Boolean = tm match {
      case OMA(OMS(MitM.polycons), _ :: StringLiterals(_) :: _ :: Nil) =>
        true
      case _ => false
    }

    override def apply(tm: Term)(implicit translator: AcrossLibraryTranslator): Term = tm match {
      case OMA(OMS(MitM.polycons),r :: x :: ls :: Nil) =>
        OMA(OMA(OMS(polyring),r :: x :: Nil),ls :: Nil)
    }
  }
  val polyFrom = new AcrossLibraryTranslation {
    override def applicable(tm: Term)(implicit translator: AcrossLibraryTranslator): Boolean = tm match {
      case OMA(OMA(OMS(`polyring`), _ :: _ :: Nil), _ :: Nil) =>
        true
      case _ => false
    }

    override def apply(tm: Term)(implicit translator: AcrossLibraryTranslator): Term = tm match {
      case OMA(OMA(OMS(`polyring`), r :: x :: Nil),ls :: Nil) =>
        OMA(OMS(MitM.polycons),r :: x :: ls :: Nil)
    }
  }

}

/** external computation provided by SageMath */
class SageSystem extends VREWithAlignmentAndSCSCP("Sage", MitMSystems.sagesym, MitMSystems.evaluateSym, "ODK/Sage") {
  import SageTranslations._
  override val toTranslations: List[AcrossLibraryTranslation] = polyTo :: multipolyTo :: numberfieldsTo :: super.toTranslations
  override val fromTranslations: List[AcrossLibraryTranslation] = polyFrom :: multipolyFrom :: numberfieldsFrom :: super.fromTranslations

  override protected lazy val translator_to = translator(new TranslationTarget {
    override def inTarget(path: GlobalName, controller: Controller): Boolean = Sage._base <= path
  },toTranslations)
}