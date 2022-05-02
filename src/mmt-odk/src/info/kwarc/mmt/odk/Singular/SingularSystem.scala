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
  val polyring = Singular.polyd1CD ? "poly_ring_d_named"

  val toPolynomials = new AcrossLibraryTranslation {
    override def applicable(tm: Term)(implicit translator: AcrossLibraryTranslator): Boolean = tm match {
      case MitM.MultiPolynomial(_,_) => true
      case _ => false
    }

    override def apply(tm: Term)(implicit translator: AcrossLibraryTranslator): Term = tm match {
      case MitM.MultiPolynomial(r,ls) =>
        var length = -1
        val names : List[String] = ls.flatMap { case (vars,coeff,_) =>
          // val MitM.Monomial(vars,coeff,_) = it
          vars.map(_._1)
        }.distinct.sorted
        val strs : List[Term] = names.map(s => StringLiterals(s))
        val monoms = ls map { case (vars,coeff,_) =>
          // names :::= vars.map(_._1)
          val args = coeff :: names.map(s => vars.find(_._1 == s).map(_._2).getOrElse(BigInt(0)))//vars.sortBy(_._1).map(_._2)
          if (length == -1) length = args.length
          assert(length == args.length)
          args
        }
        val poly = OMA(OMS(sdmp),monoms.map(args => OMA(OMS(monomials),args.map(Singular.Integers.apply))))
        OMA(OMS(dmp),OMA(OMS(polyring),r :: strs) :: poly :: Nil)
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
      case OMA(OMS(`dmp`),OMA(OMS(`polyring`),_ :: _) :: OMA(OMS(`sdmp`),monoms) :: Nil) if monoms.forall(Monomial.unapply(_).isDefined) =>
        true
      case _ => false
    }

    override def apply(tm: Term)(implicit translator: AcrossLibraryTranslator): Term = tm match {
      case OMA(OMS(`dmp`),OMA(OMS(`polyring`),r :: strs) :: OMA(OMS(`sdmp`),monoms) :: Nil) if monoms.forall(Monomial.unapply(_).isDefined) =>
        val strings = strs.map(StringLiterals.unapply(_).getOrElse(???))
        val monomials = monoms.map {
          case Monomial(ls) => MitM.Monomial(ls.tail.zipWithIndex.map(p => (strings(p._2),p._1)),ls.head)
        }
        // val monomials = monoms.map{case Monomial(ls) => MitM.Monomial(ls.zipWithIndex.tail.map(p => ("x"+p._2.toString,p._1)),ls.head)}
        //OMA(OMS(MitM.multi_polycon),r :: monomials)
        MitM.MultiPolynomial(r,monomials)
    }
  }
}

class SingularSystem extends VREWithAlignmentAndSCSCP("Singular",MitMSystems.singularsym, MitMSystems.evaluateSym, "ODK/Singular") {
  val namespace = Singular._base
  override lazy val translator_to = translator(new TranslationTarget {
    override def inTarget(path: GlobalName, controller: Controller): Boolean = {
      namespace <= path
    }
  },toTranslations)

  import SingularTranslations._
  override val fromTranslations: List[AcrossLibraryTranslation] = fromPolynomials :: super.fromTranslations
  override val toTranslations: List[AcrossLibraryTranslation] = toPolynomials :: super.toTranslations
}
