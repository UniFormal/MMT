package info.kwarc.mmt.odk.LMFDB

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.backend.Storage
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.utils.{JSON, URI}

class Plugin extends frontend.Plugin {
  val theory = Path.parseM("http://mathhub.info/ODK?LMFDB",NamespaceMap.empty)
  val dependencies = List("info.kwarc.mmt.lf.Plugin")
  override def start(args: List[String]) {
    // content enhancers
    controller.backend.addStore(LmfdbStore)
  }
}

object LmfdbStore extends Storage {
  private def doc = DPath(URI("http://mathhub.info/ODK"))
  private def meta = MPath(doc,LocalName("ec"))

  def load(path: Path)(implicit controller: Controller) {
    val ellipticcurves = """EC_(\d+)(\w)(\d+)""".r

    val (ndoc,mod,dec) = path.toTriple
    if (ndoc.isDefined && ndoc.get==doc) {
      mod.map(_.toString) match {
        case Some(ellipticcurves(n1,a,n2)) =>
          controller.add(getelliptic(n1.toInt,a,n2.toInt))
        case _ =>
      }
    }
  }

  def getelliptic(n1:Int,a:String,n2:Int) = {
    val th = new DeclaredTheory(doc,LocalName("EC_"+n1+a+n2),Some(meta))
    val url = "http://www.lmfdb.org/api/elliptic_curves/curves?_format=json&label="+n1+a+n2
    val json = query(url)
    println(json)
    th
  }

  def query(url:String) = JSON.parse(io.Source.fromURL(url).mkString)
}
