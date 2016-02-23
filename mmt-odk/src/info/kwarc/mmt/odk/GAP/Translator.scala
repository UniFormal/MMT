package info.kwarc.mmt.odk.GAP

import info.kwarc.mmt.api.archives.BuildTask
import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.frontend.Controller

class Translator(controller: Controller, bt: BuildTask, index: Document => Unit) {
  var dones : List[GAPObject] = Nil

  private var gap : GAPReader = null

  def apply(ngap:GAPReader) = {
    gap = ngap
    println("sorting...")
    var i = 0
    dones = gap.all.sortBy(o => o.depweight(gap.all))
    //dones.foreach(println(_))
    println("imported " + dones.length + "GAP Objects. Head:\n" + dones.head)
    // println("\n\n--------------------------------------------------\n Throwaways:")
    // testers.foreach(println(_))
  }
}
