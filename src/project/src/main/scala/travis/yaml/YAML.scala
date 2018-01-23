package travis.yaml

/** Implements a subset of YAML objects which can be easily serialized */
sealed trait YAML {
  /** an optional comment attached to this YAML object */
  val comment: Option[String]
  /** helper function to implement setting a given comment */
  protected def setComment(comment : Option[String]) : YAML

  /** creates a copy of this object, with the given comment */
  def withComment(comment: String) : YAML = setComment(Some(comment))
  /** creates a copy of this object, with the comment removed */
  def dropComment : YAML = setComment(None)

  /** serialize this YAML object into a string */
  def serialize: String

  /** helper function to serialize a comment with a given prefix */
  private[yaml] def serializeComment(prefix : String = "") : Option[String] = comment.map(YAML.serializeComment(_, prefix))
  /** helper function to serialize with a given prefix (i.e. spacing) in front of new lines */
  private[yaml] def serializeAs(prefix: String) : String = prefix + YAML.serializeWithPrefix(serialize, prefix)
}

private[yaml] object YAML {
  def serializeWithPrefix(value: String, prefix: String) : String = {
    val spacePrefix = " " * prefix.length

    val lines : List[String] = value.split("\n").toList
    if(lines.isEmpty) "" else (lines.head :: lines.tail.map(spacePrefix +)).mkString("\n")
  }
  def serializeComment(comment: String, prefix : String = "") : String = {
    serializeWithPrefix(comment.split("\n").map("# " +).mkString("\n"), prefix)
  }
  def prefixComment[T <: YAML](obj: T, comment: Option[String]) : T = {
    val newComment = obj.comment match {
      case Some(c) => Some(comment.map(c + "\n" + _).getOrElse(c))
      case None => comment
    }
    obj.setComment(newComment).asInstanceOf[T]
  }
}

/** helper trait used to split implementation of YAML into several files */
private[yaml] trait YAMLImplementation extends YAML