package info.kwarc.mmt.frameit

import cats.effect.IO
import com.twitter.finagle.Http
import info.kwarc.mmt.api.archives.Archive
import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.frontend.{Extension, Logger}
import info.kwarc.mmt.api.modules.{Module, Theory}
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.api.{DPath, LocalName, MPath}
import io.circe.generic.auto._
import io.finch._
import io.finch.circe._
import io.circe.syntax._

import scala.collection.immutable._

class FrameitServerExtension extends Extension with Logger with Endpoint.Module[IO] {
  override val logPrefix = "frameit"
  val FRAMEIT_ARCHIVE_ID = "Playground/frameit"

  var frameitArchive: Archive = _
  var factTheory: Theory = _

  import FactEnconderWrapper.factEncoder
  import MPathEncoderWrapper.mpathEncoder

  override def start(args: List[String]): Unit = {
    super.start(args)

    val endpoints =
      init :+: addPointFact :+: addDistanceFact :+: getAllFacts :+: getAllScrolls

    Http.server.serve(
      ":8081",
      endpoints.toServiceAs[Application.Json]
    )
  }

  // val home : Term, val name : LocalName, val alias: List[LocalName],
  //               val tpC : TermContainer, val dfC : TermContainer, val rl : Option[String], val notC: NotationContainer, val vs: Visibility)

  def init: Endpoint[IO, Unit] = post(path("init")) {
    controller.handleLine(s"build $FRAMEIT_ARCHIVE_ID mmt-omdoc")
    frameitArchive = controller.backend.getArchive(FRAMEIT_ARCHIVE_ID).getOrElse(
      throw info.kwarc.mmt.api.GetError(s"Archive $FRAMEIT_ARCHIVE_ID not found")
    )
    factTheory = Theory.empty(
      DPath(URI("https://examples.com/frameit")),
      LocalName("factTheory"),
      None
    )

    Ok(())
  } handle {
    case e: info.kwarc.mmt.api.Error => ServiceUnavailable(e)
  }

  def addPointFact: Endpoint[IO, Unit] = post(path("fact") :: path("point") :: path("add") :: jsonBody[UnityPoint]) { (point: UnityPoint) =>
    factTheory.add(MMTConstantUnityFactBridge(point, factTheory.toTerm))

    Ok(())
  }

  def addDistanceFact: Endpoint[IO, Unit] = post(path("fact") :: path("distance") :: path("add") :: jsonBody[UnityDistance]) { distance: UnityDistance =>
    factTheory.add(MMTConstantUnityFactBridge(distance, factTheory.toTerm))

    Ok(())
  }

  def getAllFacts: Endpoint[IO, List[UnityFact]] = get(path("fact") :: path("list") :: paramOption[Boolean]("simplified").withDefault(false)) { simplified: Boolean => {
    // implicit lazy val ce: CheckingEnvironment = new CheckingEnvironment(controller.simplifier, ErrorThrower, RelationHandler.ignore, this)

    // controller.simplifier.apply()

    Ok(
      factTheory
        .getDeclarations
        .flatMap(d => MMTConstantUnityFactBridge.unapply(d))
    )
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
  /*
    def apply(request: ServerRequest): ServerResponse = try {
      request.path match {
        case pathPrefix :: "init" :: Nil =>
          // controller.handleLine("build FrameIT/FrameIT mmt-omdoc")
          ServerResponse.TextResponse("Success")


        case pathPrefix :: "facts" :: "add" :: factToAdd :: Nil => factToAdd match {
          case "point" =>
            request.body.asXML match {
              case pointsXml@<ints></points> =>
                val points = pointsXml.child.map {
                  case pointXml@<point/> =>
                    val malformedError = () => throw MalformedCommandError("<point> must have x, y, z attributes")
                    List(
                      pointXml.attribute("x").getOrElse(malformedError).toString.toFloat,
                      pointXml.attribute("y").getOrElse(malformedError).toString.toFloat,
                      pointXml.attribute("z").getOrElse(malformedError).toString.toFloat
                    )

                  case _ => throw MalformedCommandError("<points> tag must have <point> in it")
                }
                ServerResponse.TextResponse(points.mkString("\n"))
              case _ => throw MalformedCommandError("Must have <points> tag")
            }
          case _ => throw UnknownCommandError()
        }

        case _ => throw UnknownCommandError()
      }
    } catch {
      case _: UnknownCommandError => ServerResponse.errorResponse(s"Unknown request path and/or command: ${request.path.mkString("/")}")
      case e: Exception => ServerResponse.errorResponse("Error while processing command: " + e.getMessage)
    }*/
}
