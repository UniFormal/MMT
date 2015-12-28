package info.kwarc.mmt.odk.LMFDB

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.backend.Storage
import info.kwarc.mmt.api.frontend.Controller

class Plugin extends frontend.Plugin {
  val theory = Path.parseM("http://mathhub.info/ODK?LMFDB",NamespaceMap.empty)
  val dependencies = List("info.kwarc.mmt.lf.Plugin")
  override def start(args: List[String]) {
    // content enhancers
    controller.backend.addStore(LmfdbStore)
  }
}

object LmfdbStore extends Storage {
  def load(path: Path)(implicit controller: Controller) {
    val lmfdbpattern = "http://mathhub.info/ODK?([a-zA-Z0-9]+)".r
    val ellipticcurves = "EC_([0-9]+)([a-z])([0-9])".r
    path.toString match {
        // Elliptic curves
      case lmfdbpattern(ellipticcurves(n1,a,n2)) =>
      case _ =>
    }
  }
}
