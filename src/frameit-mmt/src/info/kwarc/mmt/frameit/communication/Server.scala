package info.kwarc.mmt.frameit.communication

import java.net.InetSocketAddress

import com.twitter.finagle.Http
import com.twitter.server.TwitterServer
import com.twitter.util.Await
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend.{ConsoleHandler, Controller}
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.utils.{File, FilePath}
import info.kwarc.mmt.frameit.archives.Archives

object Server extends TwitterServer {
  override def failfastOnFlagsNotParsed: Boolean = true

  private val bindAddress = flag("bind", new InetSocketAddress(8080), "Bind address")
  private val archiveRoot = flag("archive-root", "", "Archive root, a default set can be obtained as a clone of https://github.com/UFrameIT/archives")

  def main(): Unit = {
    val state = initServerState(Archives.getPaths(File(archiveRoot())))
    val server = Http.serve(bindAddress(), ServerEndpoints.getServiceForState(state))
    onExit {
      server.close()
    }
    Await.ready(server)
  }

  private def initServerState(archivePaths: List[File]): ServerState = {
    val ctrl = new Controller()
    ctrl.report.addHandler(ConsoleHandler)
    archivePaths.foreach(ctrl.addArchive)
    val frameitArchive = ctrl.backend.getArchive(Archives.FrameWorld.archiveID).getOrElse {
      throw GetError(s"Archive ${Archives.FrameWorld.archiveID} could not be found!")
    }

    // TODO hack to read latest scroll meta data, should not be needed
    //      due to https://github.com/UniFormal/MMT/issues/528
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
