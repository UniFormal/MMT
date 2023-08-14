package info.kwarc.mmt.frameit.communication.server

import java.net.InetSocketAddress
import com.twitter.finagle.Http
import com.twitter.server.TwitterServer
import com.twitter.util.Await
import info.kwarc.mmt.MitM.MitM
import info.kwarc.mmt.api.frontend.{ConsoleHandler, Controller}
import info.kwarc.mmt.api.utils.{File, FilePath}
import info.kwarc.mmt.api._
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld
import info.kwarc.mmt.frameit.business.{SituationSpace, SituationTheory, StandardContentValidator}

object Server extends TwitterServer {
  override def failfastOnFlagsNotParsed: Boolean = true

  private val debug = flag("debug", false, "Server in debug mode?")
  private val bindAddress = flag("bind", new InetSocketAddress(8080), "Bind address")
  private val archiveRoot = flag("archive-root", "", "Path to archive root (preferably without spaces), e.g. to a clone of <https://github.com/UFrameIT/archives>")

  def main(): Unit = {
    if (debug()) {
      println("Server started in debugging mode.")
    }

    val state = initServerState(File(archiveRoot()))

    val restService = ConcreteServerEndpoints.getServiceForState(state)
    val server = Http.serve(bindAddress(), restService)
    onExit {
      server.close()
    }


    // Hack to run a dummy request to warm-up all MMT API caches and the JVM
    //
    // This reduces the time for a subsequent (user-initiated) request of /scroll/listall
    // by up to 9 seconds.
    //
    // todo: warm-up blocks requests, see also https://finagle.github.io/finch/best-practices.html#do-not-block-an-endpoint.

    // no warm-up currently
    /*new Thread {
      // remember to not throw exceptions in run(), but to print stack traces and [[System.exit]]
      // otherwise, the exceptions could get swallowed in this thread and never touch the surface
      override def run(): Unit = {
        // perform one listAllRequests as warm-up
        val listAllRequest = Input.get("/scroll/listall").request

        restService.apply(listAllRequest)
          .onFailure(throwable => {
            throwable.printStackTrace()
            System.exit(1)
          })
          .onSuccess(response => {
            if (response.status.code == 200) {
              println("Warm up completed.")
            } else {
              new Exception(s"Warm-up request failed with code ${response.status.code}. Response was: ${response}")
                .printStackTrace()

              System.exit(1)
            }
          })
      }
    }.run() */


    Await.ready(server)

  }



  def initServerState(archiveRoot: File): LobbyState = {

    val state = new ServerState(archiveRoot, debug())
    val lstate = state.initLobbyState_a()

    lstate

    /* to ServerState/LobbyState
    implicit val ctrl: Controller = new Controller()
    ctrl.report.addHandler(ConsoleHandler)
    ctrl.handleLine(s"log+ ${ServerState.logPrefix}")

    ctrl.handleLine(s"mathpath archive ${archiveRoot}")
    val frameitArchive = ctrl.backend.getArchive(FrameWorld.archiveID).getOrElse {
      throw ArchiveError(FrameWorld.archiveID, "archive could not be found")
    }

    // force-read relational data as [[info.kwarc.mmt.frameit.business.datastructures.Scroll]] uses
    // the depstore
    frameitArchive.readRelational(FilePath("/"), ctrl, "rel")
    // increase performance by prefetching archive content? ctrl.backend.getArchives.foreach(_.allContent)

    val situationTheory = if (debug()) Some(new SituationTheory(FrameWorld.debugSituationTheory)) else None
    val state = new ServerState(new StandardContentValidator, situationTheory)

    //println(state.contentValidator.checkTheory(ctrl.getTheory(Path.parseM("http://mathhub.info/FrameIT/frameworld/integrationtests?SituationSpace"))))
    //sys.exit(0)

    (if (debug()) state.check() else Nil) match {
      case Nil =>
        println("Situation space successfully set-up and typechecked (the latter only in release mode).")
        state

      case errors =>
        errors.foreach(System.err.println)
        throw InvalidElement(state.situationSpace, "Initial situation space does not typecheck, see stderr output for errors.")
    }

     */
  }

}
