package travis.yaml

/**
  * A Literal, i.e. an object that is not composed of other YAML objects
  * @param value Value this literal is serialized into
  * @param comment Optional comment
  */
sealed abstract class YAMLLiteral(val value: String, val comment: Option[String] = None) extends YAMLImplementation {
  /** serialize this literal into a string */
  def serialize: String = if(comment.isDefined) s"$value ${YAML.serializeComment(comment.get, value + " ")}" else value
}

/**
  *
  * @param comment Optional comment
  */
case class YAMLNull(override val comment: Option[String] = None) extends YAMLLiteral("null", comment) {
  /** Creates a new [[YAMLNull]] with the given comment */
  protected def setComment(comment : Option[String]) : YAMLNull = copy(comment = comment)
}

/**
  * A String
  * @param s String that this [[YAMLString]] represents
  * @param comment Optional comment
  */
case class YAMLString(s: String, override val comment: Option[String] = None) extends YAMLLiteral(YAMLString.serializeString(s), comment) {
  /** Creates a new [[YAMLString]] with the given comment */
  protected def setComment(comment : Option[String]) : YAMLString = copy(comment = comment)
}

object YAMLString {
  private val wordBlackList = List("true", "false", "null", "yes", "no")

  private[yaml] def serializeString(s: String) : String = {
    if (s.matches("^[A-Za-z_]+[A-Za-z0-9_]*$") && !wordBlackList.contains(s.toLowerCase)) s else {
      if(!s.contains("\"")) "\"" + s + "\"" else "'" + s.replace("'", "''") + "'"
    }
  }

  implicit def toString(ys: YAMLString) : String = ys.s
  implicit def fromString(s: String) : YAMLString = YAMLString(s, None)
}

/** represents a number */
case class YAMLNumber(d: Double, override val comment: Option[String]) extends YAMLLiteral(d.toString, comment) {
  /** Creates a new [[YAMLNumber]] with the given comment */
  protected def setComment(comment : Option[String]) : YAMLNumber = copy(comment = comment)
}
object YAMLNumber {
  implicit def toDouble(yn: YAMLNumber) : Double = yn.d
  implicit def fromDouble(d : Double) : YAMLNumber = YAMLNumber(d, None)
}

/** represents a boolean */
case class YAMLBoolean(b : Boolean, override val comment: Option[String]) extends YAMLLiteral(b.toString, comment) {
  /** Creates a new [[YAMLBoolean]] with the given comment */
  protected def setComment(comment : Option[String]) : YAMLBoolean = copy(comment = comment)
}
object YAMLBoolean {
  implicit def toBoolean(yb: YAMLBoolean) : Boolean = yb.b
  implicit def fromBoolean(b : Boolean) : YAMLBoolean = YAMLBoolean(b, None)
}
