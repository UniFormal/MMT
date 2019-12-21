package info.kwarc.mmt.frameit

import info.kwarc.mmt.api.metadata.{MetaData, MetaDatum}
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects.{OMLIT, OMMOD, Term}
import info.kwarc.mmt.api.symbols.{Constant, Declaration, FinalConstant, TermContainer}
import info.kwarc.mmt.api.uom.{RealizedType, StandardString, TheoryScala}
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.api.{DPath, GlobalName, LocalName, MPath, symbols}
import info.kwarc.mmt.frameit.Urtheories.Strings.string
import info.kwarc.mmt.lf.{ApplySpine, BinaryLFConstantScala, NullaryLFConstantScala}

import scala.collection.immutable.{::, Nil}

object MitMFoundation {
  object Strings extends TheoryScala {
    override val _base: DPath = DPath(URI("http://mathhub.info/MitM/Foundation"))
    override val _name: LocalName = LocalName("Strings")

    object string extends NullaryLFConstantScala(this._path, "string")

    def apply(str: String): Term = OMLIT(str, RealizedType(string.term, StandardString))

    /**
      * Unapply method for MiTM/Foundation string literals as [[Term]]s.
      *
      * @todo Ask Florian where this should go. Ideally either in mmt-lf or into the archive MitM/Foundation
      */
    def unapply(term: Term): Option[String] = term match {
      case OMLIT(str: String, RealizedType(string.term, StandardString)) => Some(str)
      case _ => None
    }
  }
}

object Urtheories {

  object Strings extends TheoryScala {
    override val _base: DPath = DPath(URI("http://cds.omdoc.org/urtheories"))
    override val _name: LocalName = LocalName("Strings")

    object string extends NullaryLFConstantScala(this._path, "string")

    def apply(str: String): Term = OMLIT(str, RealizedType(string.term, StandardString))

    /**
      * Unapply method for MMT/urtheories string literals as [[Term]]s.
      *
      * @todo Ask Florian where this should go. Ideally either in mmt-lf or into the archive MMT/urtheories
      */
    def unapply(term: Term): Option[String] = term match {
      case OMLIT(str: String, RealizedType(string.term, StandardString)) => Some(str)
      case _ => None
    }
  }
}

object StringLiteral {
  def unapply(term: Term): Option[String] = term match {
    case MitMFoundation.Strings(str) => Some(str)
    case Urtheories.Strings(str) => Some(str)
    case _ => None
  }
}

object FrameIT {

  object Annotations extends TheoryScala {
    override val _base: DPath = DPath(URI("https://example.com/frameit/annotation"))
    override val _name: LocalName = LocalName("Annotations")

    def getScrollsFor(theory: Theory): Seq[Scroll] = {
      theory.metadata.getAll.collect {
        case ScrollAnnotation(label, description, problemTheory) => Scroll(
          label,
          description,
          problemTheory,
          solutionTheory = theory.path
        )
      }
    }

    object ScrollAnnotation {
      def unapply(metadatum: MetaDatum): Option[(String, String, MPath)] = metadatum match {
        case MetaDatum(hasScrollProblemTheory.path, ApplySpine(scrollProblem.term, OMMOD(problemTheory) :: StringLiteral(label) :: StringLiteral(description) :: Nil)
        ) => Some((label, description, problemTheory))

        case MetaDatum(hasScrollProblemTheory.path, _) =>
          // TODO: Log error: s"Theory ${theory.path} has malformed metadata"
          None
        case _ => None
      }
    }

    object hasScrollProblemTheory extends NullaryLFConstantScala(this._path, "hasScrollProblemTheory")

    object scrollProblem extends BinaryLFConstantScala(this._path, "scrollProblem")

    object hasLabel extends NullaryLFConstantScala(this._path, "hasLabel")
    object hasUnityIdentifier extends NullaryLFConstantScala(this._path, "hasUnityIdentifier")

    trait Annotation[T] {
      def unapply(metadatum: MetaDatum): Option[T]
    }

    trait MultiAnnotation[T] extends Annotation[T] {
      def unapply(metadata: MetaData): List[T] = metadata.getAll.flatMap((m: MetaDatum) => unapply(m)) // scalac needs explicit typing here
    }

    // Forces to exactly a single annotation be there
    trait SingleAnnotation[T] extends Annotation[T] {
      def unapply(metadata: MetaData): Option[T] = {
        var value: Option[T] = None

        for (metadatum <- metadata.getAll) {
          unapply(metadatum) match {
            case Some(x) =>
              if (value.isDefined)
                return None
              else
                value = Some(x)

            case _ =>
          }
        }

        value
      }
    }

    trait SimpleStringAnnotation extends SingleAnnotation[String] {
      val annotationKey: GlobalName

      def apply(str: String): MetaDatum = MetaDatum(`annotationKey`, Urtheories.Strings(str))

      override def unapply(metadatum: MetaDatum): Option[String] = metadatum match {
        case MetaDatum(`annotationKey`, StringLiteral(str)) => Some(str)
        case _ => None
      }
    }

    object LabelAnnotation extends SimpleStringAnnotation {
      override val annotationKey: GlobalName = hasLabel.path
    }

    object UnityIdentifierAnnotation extends SimpleStringAnnotation {
      override val annotationKey: GlobalName = hasUnityIdentifier.path
    }

    trait UnityFactAnnotation[T <: UnityFact] {
      def apply(fact: T, home: Term): Constant
      def unapply(c: Constant): Option[T]
      def unapply(d: Declaration): Option[T] = d match {
        case c: Constant => unapply(c)
        case _ => None
      }
    }

    object PointAnnotation extends UnityFactAnnotation[UnityPoint] {
      override def apply(point: UnityPoint, home: Term): Constant = {
        val c = new FinalConstant(
          home = home,
          name = LocalName(point.label),
          alias = Nil,
          tpC = TermContainer.asParsed(None),
          dfC = TermContainer.asParsed(None),
          rl = None,
          notC = new NotationContainer,
          vs = symbols.Visibility.public
        )

        c.metadata.add(LabelAnnotation(point.label), UnityIdentifierAnnotation(point.unityIdentifier))

        c
      }

      override def unapply(c: Constant): Option[UnityPoint] = (c.metadata, c.metadata) match {
        case (LabelAnnotation(label), UnityIdentifierAnnotation(identifier)) =>
          Some(UnityPoint(label, identifier, 0, 0, 0))
        case _ => None
      }
    }

    object DistanceAnnotation extends UnityFactAnnotation[UnityDistance] {
      override def apply(distance: UnityDistance, home: Term): Constant = {
        ???
      }

      override def unapply(c: Constant): Option[UnityDistance] = None
    }
  }
}