package info.kwarc.mmt.frameit.communication

import cats.effect.IO
import com.twitter.finagle.Http
import com.twitter.util.Await
import info.kwarc.mmt.api.{DPath, LocalName, MPath}
import info.kwarc.mmt.api.frontend.{ConsoleHandler, Controller}
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.ontology.IsTheory
import info.kwarc.mmt.api.utils.{File, FilePath}
import info.kwarc.mmt.frameit.archives.Archives
import info.kwarc.mmt.frameit.business.Scroll
import info.kwarc.mmt.frameit.communication.SimpleOMDoc.SDeclaration
import io.finch.{Application, Endpoint, EndpointModule, Ok, Text}
import io.finch.circe._

object ServerEntrypoint extends App {
  Server.start(args.toList)
}

object Server extends EndpointModule[IO] {

  import io.circe.Json
  import io.circe.syntax._
  import io.circe.generic.extras.auto._
  import io.circe.generic.extras.Configuration

  implicit val jsonConfig: Configuration = Configuration.default.withDiscriminator("species")

  class State {
    var situationTheory: Theory = _
    var controller: Controller = _
  }

  private def getEndpoints(state: State) =
    buildArchive(state) :+: buildArchiveLight(state) :+: addFact(state) :+: listScrolls(state) :+: applyScroll(state)

  def start(args: List[String]): Unit = args match {
    case List(archiveRoot, port) =>
      println(s"Starting server on port ${port} with archive root ${archiveRoot}...")
      val state = initState(Archives.getPaths(File(archiveRoot)))

      Await.result(Http.server.serve(s":${port}", getEndpoints(state).toServiceAs[Application.Json]))

    case _ =>
      sys.error("Invalid usage")
      println(s"Usage: ${args.headOption.getOrElse("<java -jar ... see readme>")} <PathToArchiveRoot> <Port>")
      println("PathToArchiveRoot should point to your working copy of https://github.com/UFrameIT/archives")
  }

  private def initState(archivePaths: List[File]): State = {
    val ctrl = new Controller()
    ctrl.report.addHandler(ConsoleHandler)
    archivePaths.foreach(ctrl.addArchive)
    val frameitArchive = ctrl.backend.getArchive(Archives.FrameWorld.archiveID).getOrElse {
        throw info.kwarc.mmt.api.GetError(s"Archive ${Archives.FrameWorld.archiveID} could not be found!")
    }

    // TODO hack to read latest scroll meta data, should not be needed
    ctrl.handleLine(s"build ${Archives.FrameWorld.archiveID} mmt-omdoc Scrolls/OppositeLen.mmt")

    frameitArchive.allContent

    // force-read relational data as somewhere (TODO say where) we use the depstore
    // to get meta tags on things
    frameitArchive.readRelational(FilePath("/"), ctrl, "rel")

    val situationTheory = Theory.empty(
      DPath(frameitArchive.narrationBase),
      LocalName("SituationTheory"),
      Some(Archives.FrameWorld.FactCollection)
    )
    ctrl.add(situationTheory)

    val state = new State
    state.situationTheory = situationTheory
    state.controller = ctrl

    state
  }

  private def buildArchiveLight(state: State): Endpoint[IO, Unit] = post(path("archive") :: path("build-light")) {
    state.controller.handleLine(s"build ${Archives.FrameWorld.archiveID} mmt-omdoc Scrolls/OppositeLen.mmt")

    Ok(())
  }

  private def buildArchive(state: State): Endpoint[IO, Unit] = post(path("archive") :: path("build")) {
    state.controller.handleLine(s"build ${Archives.FrameWorld.archiveID} mmt-omdoc")

    Ok(())
  }

  private def addFact(state: State): Endpoint[IO, Unit] = post(path("fact") :: path("add") :: jsonBody[SimpleOMDoc.SDeclaration]) {
    (sdecl: SDeclaration) => {
      state.controller.add(SimpleOMDoc.OMDocBridge.decode(sdecl))
      Ok(())
    }
  }

  private def listScrolls(state: State): Endpoint[IO, List[SScroll]] = get(path("scroll") :: path("list")) {
    val allTheories = state.controller.depstore.getInds(IsTheory).map(_.asInstanceOf[MPath]).map(state.controller.getTheory)

    Ok(
      allTheories.flatMap(t => Scroll.fromTheory(t)(state.controller.globalLookup)).map(SScroll.fromScroll).toList
    )
  }

  private def applyScroll(state: State): Endpoint[IO, List[SDeclaration]] = get(path("scroll") :: jsonBody[SScrollApplication]) {
    (sscrollApp: SScrollApplication) => {
        Ok(List[SDeclaration]())
      }
  }
/*
  def getHintsForPartialScroll(gameLogic: FrameItLogic): Endpoint[IO,String] = post(path("scroll") :: path("hints") :: stringBody) {data: String => {
    /**
      * Example:
      * {
      *   domainTheory: http://...?SituationTheory,
      *   scroll: {
      *     "problem": http://...?OppositeLen_Problem,
      *     "solution": // unused
      *   },
      *   assignments: {
      *     "http://...?pA": {x: ..., y: ..., z: ...}
      *     ...
      *   }
      * }
      */
    JSON.parseFull(data)
    Ok("abc")
  }}

  def addView( gameLogic: FrameItLogic) : Endpoint[IO,String] = post(path("view"):: path("add"):: stringBody) {
    v : String => {
      println(v)
      JSON.parseFull(v ) match {
        case Some(map: Map[String,Any]) if map.isDefinedAt("from") && map.isDefinedAt("to") && map.isDefinedAt("mappings") => {
          val viewCode  = new ViewCode(map("from").asInstanceOf[String], map("to").asInstanceOf[String],map("mappings").asInstanceOf[Map[String,String]])
          try{
           val ret = gameLogic.addView(viewCode)
            if(ret.isDefined) Ok(ret.get) else BadRequest(new IllegalArgumentException())
          } catch {
            case e: Exception => BadRequest(new IllegalArgumentException())
          }
        }
        case default =>  {
          println("Failed to parse" )
          BadRequest(new IllegalArgumentException() )
        }
      }
    }
  }

  def getPushOut(gameLogic: FrameItLogic): Endpoint[IO,String ] =
    get(
      path("pushout")
        ::param("problem")
        ::param("solution")
        ::param("view")
    ){
    (prob : String,sol:String,view:String ) =>{
      val ret = gameLogic.applyScroll(prob, sol,view)
      if(ret.isDefined) Ok(ret.get) else BadRequest(new IllegalArgumentException())
    }
  }

*/
}
