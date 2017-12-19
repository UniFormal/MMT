package src.main.scala.yaml

import scala.language.postfixOps

/** Implements a subset of YAML objects which can be easily serialized */
sealed trait YAML {
  val comment: Option[String]
  private[yaml] def setComment(comment : Option[String]) : YAML
  def withComment(comment: String) : YAML = setComment(Some(comment))
  def dropComment : YAML = setComment(None)

  def serialize: String
  private[yaml] def serializeComment(prefix : String = "") : Option[String] = comment.map(YAML.serializeComment(_, prefix))
  private[yaml] def serializeWith(prefix: String) : String = YAML.serializeWithPrefix(serialize, prefix)
  private[yaml] def serializeAs(prefix: String) : String = prefix + serializeWith(prefix)
}

private[yaml] trait YAMLImplementation extends YAML

object YAML {
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





