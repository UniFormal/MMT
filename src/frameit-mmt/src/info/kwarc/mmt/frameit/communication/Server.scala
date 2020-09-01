package info.kwarc.mmt.frameit.communication

import java.net.InetSocketAddress

import com.twitter.finagle.Http
import com.twitter.server.TwitterServer
import com.twitter.util.Await
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend.{ConsoleHandler, Controller}
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.utils.{File, FilePath}
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld

object Server extends TwitterServer {
  override def failfastOnFlagsNotParsed: Boolean = true

  private val bindAddress = flag("bind", new InetSocketAddress(8080), "Bind address")
  private val archiveRoot = flag("archive-root", "", "Path to archive root (preferably without spaces), e.g. to a clone of <https://github.com/UFrameIT/archives>")

  def main(): Unit = {
    val state = initServerState(File(archiveRoot()))
    val server = Http.serve(bindAddress(), ServerEndpoints.getServiceForState(state))
    onExit {
      server.close()
    }
    Await.ready(server)
  }

  private def initServerState(archiveRoot: File): ServerState = {
    val ctrl = new Controller()
    ctrl.report.addHandler(ConsoleHandler)

    ctrl.handleLine(s"mathpath archive ${archiveRoot}")
    val frameitArchive = ctrl.backend.getArchive(FrameWorld.archiveID).getOrElse {
      throw GetError(s"Archive ${FrameWorld.archiveID} could not be found!")
    }

    // TODO hack to read latest scroll meta data, should not be needed
    //      due to https://github.com/UniFormal/MMT/issues/528
    ctrl.handleLine(s"build ${FrameWorld.archiveID} mmt-omdoc Scrolls/OppositeLen.mmt")

    frameitArchive.allContent

    // force-read relational data as somewhere (TODO say where) we use the depstore
    // to get meta tags on things
    frameitArchive.readRelational(FilePath("/"), ctrl, "rel")

    val situationTheory = Theory.empty(
      DPath(frameitArchive.narrationBase),
      LocalName("SituationTheory"),
      Some(FrameWorld.metaTheoryForSituationTheory)
    )
    ctrl.add(situationTheory)

    val state = new ServerState(ctrl, situationTheory.path.parent, situationTheory.path)

    state.contentValidator.checkTheory(situationTheory) match {
      case Nil =>
        state

      case errors =>
        sys.error("Created situation theory, but cannot successfully typecheck it. Server will not be started. Errors below:")
        sys.error(errors.mkString("\n"))
        throw new Exception("")
    }
  }
}
