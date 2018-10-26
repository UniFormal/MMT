package info.kwarc.mmt.odk.Singular

import info.kwarc.mmt.MitM.{MitM, MitMSystems}
import info.kwarc.mmt.MitM.VRESystem.VREWithAlignmentAndSCSCP
import info.kwarc.mmt.api.{GlobalName, Path}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects.{OMA, OMS, Term}
import info.kwarc.mmt.api.refactoring.{AcrossLibraryTranslation, AcrossLibraryTranslator, TranslationTarget}
import info.kwarc.mmt.odk.{IntegerLiterals, StringLiterals}

object SingularTranslations {
  object AnyInt {
    def unapply(tm : Term) = tm match {
      case Singular.Integers(i : BigInt) => Some(i)
      case IntegerLiterals(i : BigInt) => Some(i)
      case _ => None
    }
  }

  val monomials = Singular.polyd1CD ? "term"
  val sdmp = Singular.polyd1CD ? "SDMP"
  val dmp = Singular.polyd1CD ? "DMP"
  val toPolynomials = new AcrossLibraryTranslation {
    override def applicable(tm: Term)(implicit translator: AcrossLibraryTranslator): Boolean = tm match {
      case OMA(OMS(MitM.multi_polycon),_ :: ls) if ls.forall(MitM.Monomial.unapply(_).isDefined) => true
      case _ => false
    }

    override def apply(tm: Term)(implicit translator: AcrossLibraryTranslator): Term = tm match {
      case OMA(OMS(MitM.multi_polycon), r :: ls) if ls.forall(MitM.Monomial.unapply(_).isDefined) =>
        var length = -1
        var names : List[String] = Nil
        val monoms = ls map { it =>
          val MitM.Monomial(vars,coeff,_) = it
          names :::= vars.map(_._1)
          val args = coeff :: vars.sortBy(_._1).map(_._2)
          if (length == -1) length = args.length
          assert(length == args.length)
          OMA(OMS(monomials),args.map(Singular.Integers.apply))
        }
        val poly = OMA(OMS(`sdmp`),monoms)
        val strs : List[Term] = names.distinct.sorted.map(s => StringLiterals(s))
        OMA(OMS(`dmp`),OMA(r,strs) :: poly :: Nil)
    }
  }

  val fromPolynomials = new AcrossLibraryTranslation {
    object Monomial {
      def unapply(tm : Term) = tm match {
        case OMA(OMS(`monomials`),ls) if ls.forall(AnyInt.unapply(_).isDefined) =>
          Some(ls.flatMap(AnyInt.unapply))
        case _ => None
      }
    }
    override def applicable(tm: Term)(implicit translator: AcrossLibraryTranslator): Boolean = tm match {
      case OMA(OMS(`dmp`),r :: OMA(OMS(`sdmp`),monoms) :: Nil) if monoms.forall(Monomial.unapply(_).isDefined) =>
        true
      case _ => false
    }

    override def apply(tm: Term)(implicit translator: AcrossLibraryTranslator): Term = tm match {
      case OMA(OMS(`dmp`),OMA(r,_) :: OMA(OMS(`sdmp`),monoms) :: Nil) if monoms.forall(Monomial.unapply(_).isDefined) =>
        val monomials = monoms.map{case Monomial(ls) => MitM.Monomial(ls.zipWithIndex.tail.map(p => ("x"+p._2.toString,p._1)),ls.head)}
        OMA(OMS(MitM.multi_polycon),r :: monomials)
    }
  }
}

class SingularSystem extends VREWithAlignmentAndSCSCP("Singular",MitMSystems.singularsym, MitMSystems.evaluateSym, "ODK/Singular") {
  val namespace = Singular._base
  override protected lazy val translator_to = translator(new TranslationTarget {
    override def inTarget(path: GlobalName, controller: Controller): Boolean = {
      namespace <= path
    }
  },toTranslations)

  import SingularTranslations._
  override val fromTranslations: List[AcrossLibraryTranslation] = fromPolynomials :: super.fromTranslations
  override val toTranslations: List[AcrossLibraryTranslation] = toPolynomials :: super.toTranslations
}
