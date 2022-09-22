package info.kwarc.mmt.odk.Singular

import info.kwarc.mmt.api.DPath
import info.kwarc.mmt.api.frontend.Extension
import info.kwarc.mmt.api.objects.OMS
import info.kwarc.mmt.api.uom.{RepresentedRealizedType, StandardInt}
import info.kwarc.mmt.api.utils.URI

class Plugin extends Extension {
  val dependencies = List("info.kwarc.mmt.lf.Plugin")
  override def start(args: List[String]): Unit = {
    controller.extman.addExtension(new SingularImporter)
    controller.extman.addExtension(new SingularSystem)
  }
}

object Singular {
  val dpath = DPath(URI.http colon "www.singular.uni-kl.de")
  val meta = dpath ? "Types"
  def tp(s : String) = OMS(meta ? s)
  val _base = DPath(URI.https colon "www.singular.uni-kl.de")
  val singularCD = _base ? "singular"
  val polyd1CD = _base ? "polyd1"
  val ring3CD = _base ? "ring3"


  object Integers extends RepresentedRealizedType(OMS(ring3CD ? "integers"),StandardInt)
}