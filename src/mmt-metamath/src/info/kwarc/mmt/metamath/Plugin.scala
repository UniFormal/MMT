package info.kwarc.mmt.metamath

import info.kwarc.mmt.api.frontend

class Plugin extends frontend.Plugin {
  val theory = MetaMath.theory
  val dependencies = List("info.kwarc.mmt.lf.Plugin")
  override def start(args: List[String]) {
    val em = controller.extman
    // content enhancers
    em.addExtension(new Importer)
  }
}