package info.kwarc.mmt.frameit

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.metadata.MetaDatum
import info.kwarc.mmt.api.modules.{Module, Theory}
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects.{OMID, OMLIT, OMMOD, Term}
import info.kwarc.mmt.api.symbols.{Constant, Declaration, FinalConstant, TermContainer}
import info.kwarc.mmt.api.uom.{RealizedType, StandardString}
import info.kwarc.mmt.lf.ApplySpine
import io.circe.generic.auto._
import io.circe.syntax._

import scala.collection.immutable.{::, List, Nil}

sealed abstract class UnityFact(label: String, unityIdentifier: String)

sealed case class UnityPoint(label: String, unityIdentifier: String, x: Double, y: Double, z: Double) extends UnityFact(label, unityIdentifier)

sealed case class UnityDistance(label: String, unityIdentifier: String, start: UnityPoint, end: UnityPoint) extends UnityFact(label, unityIdentifier)

// Need a wrapper to be able to import the inner implicit encoder
object FactEnconderWrapper {
  implicit val factEncoder: io.circe.Encoder[UnityFact] = fact => {
    val (underlyingJson, underlyingType) = fact match {
      case point: UnityPoint => (point.asJson, "point")
      case distance: UnityDistance => (distance.asJson, "distance")
    }

    underlyingJson.asJson.asObject.get.+:("type", underlyingType.asJson).asJson
  }
}

sealed case class Scroll(label: String, description: String, problemTheory: MPath, solutionTheory: MPath)

/**
  * Object providing an [[MPath]] encoder such that encoder and decoder for [[Scroll]] can be automatically derived by Circe.
  *
  * We need a wrapper to be able to import the inner implicit encoder.
  */
object MPathEncoderWrapper {
  implicit val mpathEncoder: io.circe.Encoder[MPath] = mpath => mpath.toString.asJson
}

object Scroll {
  def findAllIn(modules: Seq[Module]): Seq[Scroll] = {
    modules
      .filter(_.isInstanceOf[Theory])
      .map(_.asInstanceOf[Theory])
      .flatMap(TheoryMetaBridgeToScroll.unapply)
  }

  /*implicit val scrollEncoder: io.circe.Encoder[Scroll] = scroll => Json.obj(
    ("label",  scroll.label),
    ("description", scroll.description),
    ("problemTheory", scroll.pr)
  )
      case point: UnityPoint =>
        Json.obj(
          ("unityIdentifier", point.unityIdentifier.asJson),
          ("x", point.x.asJson),
          ("y", point.y.asJson),
          ("z", point.z.asJson)
        )
      case _ => ???
  }*/
}

object MMTConstantUnityFactBridge {
  def apply(point: UnityPoint, home: Term): Constant = {
    new FinalConstant(
      home = home,
      name = LocalName(point.label),
      alias = Nil,
      tpC = TermContainer.asParsed(None),
      dfC = TermContainer.asParsed(None),
      rl = None,
      notC = new NotationContainer,
      vs = symbols.Visibility.public
    )

    // TODO: Add unity identifier to constant
    //       pointConstant.metadata.add()
  }

  def apply(distance: UnityDistance, home: Term): Constant = {
    ???
  }

  def unapply(declaration: Declaration): Option[UnityFact] = declaration match {
    case c: FinalConstant => unapply(c)
    case _ => None
  }

  def unapply(constant: Constant): Option[UnityFact] = {
    Some(UnityPoint("A", "1", 4, 5, 6))
  }
}

/**
  * Unapply method for MMT/urtheories string literals as [[Term]]s.
  *
  * @todo Ask Florian where this should go. Ideally either in mmt-lf or into the archive MMT/urtheories
  */
object UrtheoriesString {
  private val urtheoriesStringType: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?Strins?string", NamespaceMap.empty)

  def unapply(term: Term): Option[String] = term match {
    case OMLIT(str: String, RealizedType(OMID(urtheoriesStringType), StandardString)) => Some(str)
    case _ => None
  }
}

object TheoryMetaBridgeToScroll {
  def unapply(theory: Theory): List[Scroll] = {
    val hasScrollProblemTheory: GlobalName = Path.parseS("https://example.com/frameit/annotation?ScrollMeta?hasScrollProblemTheory", NamespaceMap.empty)
    val scrollProblem: GlobalName = Path.parseS("https://example.com/frameit/annotation?ScrollMeta?scrollProblem", NamespaceMap.empty)

    theory.metadata.get(hasScrollProblemTheory).flatMap {
      case MetaDatum(
      _,
      ApplySpine(OMID(scrollProblem), OMMOD(problemTheory) :: UrtheoriesString(label) :: UrtheoriesString(description) :: Nil)
      ) => Some(Scroll(label, description, problemTheory, theory.path))

      case _ =>
        // Log error: s"Theory ${theory.path} has malformed metadata"
        None
    }
  }
}