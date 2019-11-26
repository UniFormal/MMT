/**
  * Various paths and helper objects for MMT theories in the MMT/examples archive
  * revolving around SFOL.
  */

package info.kwarc.mmt.moduleexpressions.operators

import info.kwarc.mmt.api.objects.{OMID, OML, OMS, Term}
import info.kwarc.mmt.api.uom.TheoryScala
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.api.{DPath, GlobalName, LocalName}
import info.kwarc.mmt.lf._

import scala.collection.mutable

object TypedTerms extends TheoryScala {
  val _base = DPath(URI("http://cds.omdoc.org/examples"))
  val _name = LocalName("TypedTerms")

  // The LF type of sorts in our theory
  val typeOfSorts: GlobalName = _path ? "tp"

  // The LF `tp -> type` function giving us "all terms" of a specific sort
  val termsOfSort: GlobalName = _path ? "tm"
}

/** MMT declarations
  *
  * namespace http://cds.omdoc.org/examples
  * fixmeta ?LF
  *
  * theory PL =
  * prop : type
  * ded : prop -> type
  *
  * theory SFOL =
  * sort : type
  * term : sort -> type
  */

object PL extends TheoryScala {
  val _base = DPath(URI("http://cds.omdoc.org/examples"))
  val _name = LocalName("PL")
  val prop: GlobalName = _path ? "prop"

  object ded extends UnaryLFConstantScala(_path, "ded")

  object implies extends BinaryLFConstantScala(_path, "impl")

}

object SFOL extends TheoryScala {
  val _base = DPath(URI("http://cds.omdoc.org/examples"))
  val _name = LocalName("SFOL")

  /**
    * Not sure whether this constant actually exists or where this is used
    */
  @deprecated("MMT mmt-lf/moduleexpressions/operators", "v18") val sort: GlobalName = _path ? "sort"

  /*
  object forall{
      val _path = SFOL._path ? "forall"
      def apply(context: Context,body : Term): Term ={
        OMBIND(OMS(_path),context,body)
        Apply(OMS(path), Lambda(..)
      }
      def unapply(t : Term): Option[(Term,Context,Term)] = t match {
        case OMBIND(binder,context,body) => Some((binder,context,body))
        case _ => None
      }
  }
  */
  object term extends UnaryLFConstantScala(_path, "term")

  object equal extends TernaryLFConstantScala(_path, "equal")

  object forall extends TypedBinderScala(_path, "forall", term)

  object exists extends TypedBinderScala(_path, "exists", term)

  private def checkConformingAndCollectSorts(args: List[Term]): Option[List[LocalName]] = {
    var sortNames = new mutable.LinkedHashSet[LocalName]()

    val conforming = args.forall({
      case ApplySpine(OMID(TypedTerms.termsOfSort), List(OML(sortName, _, _, _, _))) =>
        sortNames += sortName
        true
      case _ => false
    })

    if (conforming) {
      Some(sortNames.toList)
    } else {
      None
    }
  }

  object FunctionType {
    /**
      *
      * @param decl
      * @return List of unique occurring sort local names in the order they appear in the function type from left to right
      */
    def unapply(decl: Term): Option[List[LocalName]] = decl match {
      case FunType(args, returnType) => {
        if (!args.forall(_._1.isEmpty)) {
          None
        } else {
          checkConformingAndCollectSorts(returnType :: args.map(_._2))
        }
      }
      case _ => None
    }
  }

  object PredicateType {
    def unapply(decl: Term): Option[List[LocalName]] = decl match {
      case FunType(args, OMS(PL.prop)) =>
        if (!args.forall(_._1.isEmpty)) {
          None
        } else {
          checkConformingAndCollectSorts(args.map(_._2))
        }
      case _ => None
    }
  }

  object FunctionOrPredicateType {
    def unapply(decl: Term): Option[List[LocalName]] = decl match {
      case FunctionType(sortNames) => Some(sortNames)
      case PredicateType(sortNames) => Some(sortNames)
      case _ => None
    }
  }

  /**
    * e.g. `a: tp`, but not `b: tp | = a`
    */
  object UndefinedSortDeclaration {
    def unapply(decl: OML): Boolean = decl match {
      case OML(_, Some(OMID(TypedTerms.typeOfSorts)), None, _, _) => true
      case _ => false
    }
  }

}