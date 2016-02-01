package info.kwarc.mmt.odk.GAP

import info.kwarc.mmt.api.archives.BuildTask
import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.frontend.Controller

class Translator(controller: Controller, bt: BuildTask, index: Document => Unit) {
  var dones : List[GAPObject] = Nil
  var notready : List[GAPObject] = Nil

  private var gap : GAPReader = null

  private def sort(list:List[GAPObject]) : Unit = {
    list.foreach { g =>
      val refs = g.implications(gap.all)
        if (refs.forall(p => dones contains p.getInner)) dones::=g
        else notready::=g
    }
    if (dones.length < gap.all.length) sort({
      val old = notready
      notready = Nil
      old
    })
  }

  def apply(ngap:GAPReader) = {
    gap = ngap
    print("sorting...")
    sort(gap.all)
    dones.foreach(println(_))
    println(dones.length)
    // println("\n\n--------------------------------------------------\n Throwaways:")
    // testers.foreach(println(_))
  }
}
