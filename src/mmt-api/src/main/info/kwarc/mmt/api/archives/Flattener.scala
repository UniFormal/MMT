package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import archives._
import documents._
import modules._
import symbols._
import objects._
import libraries._

class FlatExporter extends Exporter {
   def key = "flat"
   def exportDocument(doc : Document, bf: BuildTask) {}
   def exportView(view : DeclaredView, bf: BuildTask) {
      // TODO
   }
   def exportNamespace(dpath: DPath, bd: BuildTask, namespaces: List[BuildTask], modules: List[BuildTask]) {}

   def exportTheory(thy : DeclaredTheory, bf: BuildTask) {
      val me = controller.simplifier
      me(thy)
      val node = thy.toNodeElab
      rh(node)
   }
}
