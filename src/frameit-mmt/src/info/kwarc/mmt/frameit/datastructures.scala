package info.kwarc.mmt.frameit

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules.{Module, Theory}
import info.kwarc.mmt.api.presentation.MMTSyntaxPresenter
import info.kwarc.mmt.api.symbols.Declaration
import io.circe.Json
import io.circe.generic.auto._
import io.circe.syntax._

sealed abstract class Fact(label: String)

/**
  * Facts of which the Unity side is aware of.
  */
sealed abstract class UnityFact(label: String, unityIdentifier: String) extends Fact(label)

sealed case class UnityPoint(label: String, unityIdentifier: String, x: Double, y: Double, z: Double) extends UnityFact(label, unityIdentifier)

sealed case class UnityDistance(label: String, unityIdentifier: String, start: UnityPoint, end: UnityPoint) extends UnityFact(label, unityIdentifier)

sealed case class UnknownFact(d: Declaration) extends Fact(d.name.toString)

// Need a wrapper to be able to import the inner implicit encoder
object FactEnconderWrapper {
  /**
    * Get an encoder for [[Fact facts]].
    *
    * @param presenter A function returning an [[MMTSyntaxPresenter]].
    *                  We need this indirection because the encoder has to be present as an implicit value
    *                  "at compile time" for [[io.circe.generic]] to work, however, we can only construct
    *                  the presenter later on in [[FrameitServerExtension]].
    *
    *                  Ideally this function is as cheap as just returning a variable.
    * @return
    */
  def getFactEncoder(presenter: () => MMTSyntaxPresenter): io.circe.Encoder[Fact] = fact => {
    val (underlyingJson, underlyingType) = fact match {
      case point: UnityPoint => (point.asJson, "point")
      case distance: UnityDistance => (distance.asJson, "distance")
      case UnknownFact(decl) =>
        val declAsString = {
          val stringRenderingHandler = new info.kwarc.mmt.api.presentation.StringBuilder
          presenter()(decl)(stringRenderingHandler)
          stringRenderingHandler.get
        }

        (Json.obj(("str", declAsString.asJson)), "unknown")
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
      .flatMap(FrameIT.Annotations.getScrollsFor)
  }
}
