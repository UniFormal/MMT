package info.kwarc.mmt.frameit

import cats.effect.IO
import cats.instances.int._
import com.twitter.finagle.Http
import com.twitter.util.Await
import info.kwarc.mmt.api.GetError
import info.kwarc.mmt.api.frontend.{Extension, Logger}
import io.finch._

import scala.collection.immutable._

class FrameitServerExtension extends Extension with Logger with Endpoint.Module[IO] {
  override val logPrefix = "frameit"

  def init: Endpoint[IO, Unit] = post(path("init")) {
    controller.handleLine("build FrameIT/FrameIT mmt-omdoc")
    Ok()
  } handle {
    case e: info.kwarc.mmt.api.Error => ServiceUnavailable(e)
  }

  def addPointFact: Endpoint[IO, Int] = post(path("fact") :: path("add") :: path("point") :: path[Int] :: path[Int] :: path[Int]) { (x: Int, y: Int, z: Int) =>
    Ok(x + y + z)
  }

  override def start(args: List[String]): Unit = {
    super.start(args)

    Http.server.serve(
      ":8081",
      (init :+: addPointFact).toServiceAs[Text.Plain]
    )
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
