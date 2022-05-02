package info.kwarc.mmt.odk.Sage

import info.kwarc.mmt.MitM.VRESystem.VREWithAlignmentAndSCSCP
import info.kwarc.mmt.MitM.{MitM, MitMSystems}
import info.kwarc.mmt.api.GlobalName
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.ontology.LogicalReference
import info.kwarc.mmt.api.refactoring.{AcrossLibraryTranslation, AcrossLibraryTranslator, TranslationTarget}
import info.kwarc.mmt.lf.ApplySpine
import info.kwarc.mmt.odk.LFX.LFList
import info.kwarc.mmt.odk.OpenMath.OMSymbol
import info.kwarc.mmt.odk.{IntegerLiterals, StringLiterals}
import info.kwarc.mmt.sequences.Sequences

/** translations to be used in SageSystem */
object SageTranslations {
  private val nf = Sage.docpath ? """sage.rings.number_field.number_field""" ? "NumberField"
  private val poly = Sage.docpath ? "sage.rings.polynomial.polynomial_element" ? "Polynomial"
  private val polyring = Sage.docpath ? "sage.rings.polynomial.polynomial_ring_constructor" ? "PolynomialRing"
  
  val numberfieldsTo = new AcrossLibraryTranslation {
    override def applicable(tm: Term)(implicit translator: AcrossLibraryTranslator): Boolean = tm match {
      case OMA(OMS(MitM.numberfield),a::Nil) => true
      case _ => false
    }

    override def apply(tm: Term)(implicit translator: AcrossLibraryTranslator): Term = tm match {
      case OMA(OMS(MitM.numberfield),a::Nil) =>
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
        OMA(OMS(MitM.numberfield),List(a))
    }
  }

  object PolyMatcher extends MitM.MultiPolynomialMatcher {
    def apply(mps: MitM.MultiPolynomialStructure) = {
      val dictitems = mps.monomials.map {m => Python.tuple(Python.tuple(m.exponents.map(IntegerLiterals.apply)), IntegerLiterals(m.coefficient))}
      poly(polyring(mps.baseRing, Python.list(mps.varTerms :_*)), Python.dict(dictitems :_*))
    }
    def unapply(t: Term): Option[MitM.MultiPolynomialStructure] = t match {
      case OMA(OMS(`poly`), OMA(OMS(`polyring`), baseRing :: OMA(OMS(Python.list), varTerms) :: Nil) :: OMA(OMS(Python.dict), dictitems) :: Nil) =>
        val names = varTerms map {
          case StringLiterals(s) => s
          case _ => return None
        }
        val monoms = dictitems map {
          case OMA(OMS(Python.tuple), List(OMA(OMS(Python.tuple), expTerms), IntegerLiterals(coeff))) =>
            val exps = expTerms map {
              case IntegerLiterals(e) => e
              case _ => return None
            }
            MitM.MultiMonomialStructure(coeff, exps)
        }
        val mps = MitM.MultiPolynomialStructure(baseRing, names, monoms)
        Some(mps)
      case _ => None
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
  override val toTranslations: List[AcrossLibraryTranslation] = polyTo :: PolyMatcher.getAlignmentTo :: numberfieldsTo :: super.toTranslations
  override val fromTranslations: List[AcrossLibraryTranslation] = polyFrom :: PolyMatcher.getAlignmentFrom :: numberfieldsFrom :: super.fromTranslations

  override lazy val translator_to = translator(new TranslationTarget {
    override def inTarget(path: GlobalName, controller: Controller): Boolean = Sage._base <= path
  },toTranslations)
}