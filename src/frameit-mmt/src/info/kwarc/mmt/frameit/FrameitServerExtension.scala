package info.kwarc.mmt.frameit

import cats.effect.IO
import com.twitter.finagle.Http
import info.kwarc.mmt.api.archives.Archive
import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.frontend.{Extension, Logger}
import info.kwarc.mmt.api.modules.{Module, Theory}
import info.kwarc.mmt.api.objects.{Context, Obj}
import info.kwarc.mmt.api.presentation.MMTSyntaxPresenter
import info.kwarc.mmt.api.symbols.{Constant, Declaration, FinalConstant, TermContainer}
import info.kwarc.mmt.api.uom.SimplificationUnit
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.api.{DPath, LocalName, MPath, RuleSet}
import io.circe.generic.auto._
import io.finch._
import io.finch.circe._

import scala.collection.immutable._

class FrameitServerExtension extends Extension with Logger with Endpoint.Module[IO] {
  override val logPrefix = "frameit"
  val FRAMEIT_ARCHIVE_ID = "Playground/frameit"

  var frameitArchive: Archive = _
  var factTheory: Theory = _
  var presenter: MMTSyntaxPresenter = _

  implicit val factEncoder = FactEnconderWrapper.getFactEncoder(() => presenter)
  import MPathEncoderWrapper.mpathEncoder  // possibly shown as unused by IntelliJ. Do not remove, it IS used!

  override def start(args: List[String]): Unit = {
    super.start(args)

    val endpoints =
      init :+: build :+: rebuild :+: addPointFact :+: addDistanceFact :+: getAllFacts :+: getAllScrolls

    Http.server.serve(
      ":8081",
      endpoints.toServiceAs[Application.Json]
    )
  }

  def init: Endpoint[IO, Unit] = post(path("init")) {
    presenter = controller.extman.getOrAddExtension(classOf[MMTSyntaxPresenter], classOf[MMTSyntaxPresenter].getCanonicalName, Nil).get

    frameitArchive = {
      // Try quering the archive, possibly building once, then giving up
      controller.backend.getArchive(FRAMEIT_ARCHIVE_ID).getOrElse {
        controller.handleLine(s"build $FRAMEIT_ARCHIVE_ID mmt-omdoc")
        controller.backend.getArchive(FRAMEIT_ARCHIVE_ID).getOrElse(
          throw info.kwarc.mmt.api.GetError(s"Archive $FRAMEIT_ARCHIVE_ID not found")
        )
      }
    }
    factTheory = controller.getTheory((DPath(frameitArchive.narrationBase) / "annotation") ? "FactTheory")

    Ok(())
  } handle {
    case e: info.kwarc.mmt.api.Error => ServiceUnavailable(e)
  }

  def build: Endpoint[IO, Unit] = post(path("build")) {
    controller.handleLine(s"build $FRAMEIT_ARCHIVE_ID mmt-omdoc")

    Ok(())
  }

  def rebuild: Endpoint[IO, Unit] = post(path("rebuild")) {
    controller.handleLine(s"build $FRAMEIT_ARCHIVE_ID -mmt-omdoc")
    controller.handleLine(s"build $FRAMEIT_ARCHIVE_ID mmt-omdoc")

    Ok(())
  }

  def addPointFact: Endpoint[IO, Unit] = post(path("fact") :: path("add") :: path("point") :: jsonBody[UnityPoint]) { (point: UnityPoint) =>
    factTheory.add(FrameIT.Annotations.PointAnnotation(point, factTheory.toTerm))

    Ok(())
  }

  def addDistanceFact: Endpoint[IO, Unit] = post(path("fact") :: path("add") :: path("distance") :: jsonBody[UnityDistance]) { distance: UnityDistance =>
    factTheory.add(FrameIT.Annotations.DistanceAnnotation(distance, factTheory.toTerm))

    Ok(())
  }

  def getAllFacts: Endpoint[IO, List[Fact]] = get(path("fact") :: path("list") :: paramOption[Boolean]("simplified").withDefault(false)) { simplified: Boolean => {
    // TODO How to get solver out of controller?
    val simplificationUnit = SimplificationUnit(factTheory.getInnerContext, expandDefinitions = true, fullRecursion = true, solverO = None)

    def simplify(obj: Obj): obj.ThisType = controller.simplifier.apply(obj, simplificationUnit)

    val declarations: Seq[Declaration] = {
      if (!simplified) {
        factTheory.getDeclarations
      } else {
        // Simplify type and definiens components of constant declarations
        factTheory.getDeclarations.map {
          case c: Constant =>
            new FinalConstant(
              home = c.home,
              name = c.name,
              alias = c.alias,
              tpC = TermContainer.asParsed(c.tp.map(simplify(_))),
              dfC = TermContainer.asParsed(c.df.map(simplify(_))),
              rl = c.rl,
              notC = c.notC,
              vs = c.vs
            )
          case x => x
        }
      }
    }

    val reconstructedFacts: Seq[Fact] = declarations.collect {
        case FrameIT.Annotations.PointAnnotation(point) => point
        case FrameIT.Annotations.DistanceAnnotation(distance) => distance
        case d => UnknownFact(d)
    }

    Ok(reconstructedFacts.toList)
  }
  }

  def getAllScrolls: Endpoint[IO, List[Scroll]] = get(path("scroll") :: path("list")) {
    val scrolls = Scroll.findAllIn(
      loadFrameItModules().map(controller.getAs(classOf[Module], _))
    ).toList

    Ok(scrolls)
  }

  def loadFrameItModules(): List[MPath] = {
    controller.getAs(classOf[Document], DPath(frameitArchive.narrationBase)).getModules(controller.globalLookup)
  }

  /*lazy val checker = {
    val ret = new MMTStructureChecker(new RuleBasedChecker)
    controller.extman.addExtension(ret)
    ret
  }*/
  /*implicit lazy val ce : CheckingEnvironment = new CheckingEnvironment(controller.simplifier,ErrorThrower,RelationHandler.ignore, this)*/
}
