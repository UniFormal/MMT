package src.main.scala.yaml

/** represents any literal value */
sealed abstract class YAMLLiteral(val value: String, val comment: Option[String]) extends YAMLImplementation {
  def serialize: String = if(comment.isDefined) s"$value ${YAML.serializeComment(comment.get, value + " ")}" else value
}

/** represents a string */
case class YAMLString(s: String, override val comment: Option[String]) extends YAMLLiteral(YAMLString.serializeString(s), comment) {
  def setComment(comment : Option[String]) : YAMLString = copy(comment = comment)
}
object YAMLString {
  private[yaml] def serializeString(s: String) : String = {
    lazy val hasBlackList = !List("-", "\n", ":", "[", ">", "{").exists(s.contains)
    if (s.matches("^[A-Za-z_]+[A-Za-z0-9_]$") || hasBlackList) s else {
      if(!s.contains("\"")) "\"" + s + "\"" else "'" + s + "'"
    }
  }
  implicit def toString(ys: YAMLString) : String = ys.s
  implicit def fromString(s: String) : YAMLString = YAMLString(s, None)
}

/** represents a number */
case class YAMLNumber(d: Double, override val comment: Option[String]) extends YAMLLiteral(d.toString, comment) {
  def setComment(comment : Option[String]) : YAMLNumber = copy(comment = comment)
}
object YAMLNumber {
  implicit def toDouble(yn: YAMLNumber) : Double = yn.d
  implicit def fromDouble(d : Double) : YAMLNumber = YAMLNumber(d, None)
}

/** represents a boolean */
case class YAMLBoolean(b : Boolean, override val comment: Option[String]) extends YAMLLiteral(b.toString, comment) {
  def setComment(comment : Option[String]) : YAMLBoolean = copy(comment = comment)
}
object YAMLBoolean {
  implicit def toBoolean(yb: YAMLBoolean) : Boolean = yb.b
  implicit def fromBoolean(b : Boolean) : YAMLBoolean = YAMLBoolean(b, None)
}