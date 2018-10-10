package info.kwarc.mmt.MiTM.Config

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.utils._

/** configuration for the MiTM Architecture */
case class MiTMConfig(gap: MiTMSystemLocation, singular: MiTMSystemLocation, sage: MiTMSystemLocation) {
  /** gets the location of a given system */
  def apply(id: String): MiTMSystemLocation = id.toLowerCase match {
    case "gap" => gap
    case "singular" => singular
    case "sage" => sage
    case _ => throw new Exception("Unknown System")
  }
}

object MiTMConfig {
  lazy val default: MiTMConfig = apply(MMTSystem.getResourceAsString("mitm/config.default.json"))
  def apply(config: String): MiTMConfig = apply(JSON.parse(config).asInstanceOf[JSONObject])
  def apply(json: JSONObject): MiTMConfig = {
    val parser = new JSONObjectParser(json)

    implicit val converter: JSONConverter[MiTMSystemLocation] = MiTMSystemLocation
    MiTMConfig(parser.take("gap"), parser.take("singular"), parser.take("sage"))
  }
}

/** Represents the location of a single MiTM SCSCP System */
case class MiTMSystemLocation(hostname: String, port: Int)
object MiTMSystemLocation extends JSONConverter[MiTMSystemLocation] {
  def toJSON(obj: MiTMSystemLocation): JSON = JSONString(obj.hostname + ":" + obj.port)
  def fromJSONOption(j: JSON): Option[MiTMSystemLocation] = j match {
    case JSONString(s) => Some(apply(s))
    case _ => None
  }

  val pattern = "^([^\\:]+)(?:\\:(\\d+))?$".r
  def apply(spec: String): MiTMSystemLocation = {
    val (hn, port) = pattern
    .findFirstMatchIn(spec)
    .map({m => (m.group(1), m.group(2))})
    .getOrElse((spec, null))
    MiTMSystemLocation(hn, Option(port).getOrElse("26133").toInt)
  }
}