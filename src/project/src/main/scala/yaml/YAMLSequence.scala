package src.main.scala.yaml

import scala.collection.mutable.ListBuffer

/** Represents a sequence of YAML objects */
case class YAMLSequence(items: Seq[YAML], comment: Option[String]) extends YAMLImplementation {
  def setComment(comment : Option[String]) : YAMLSequence = copy(comment = comment)

  def serialize: String = {
    // create a new array of lines for this string
    val structure = new ListBuffer[String]

    // add a comment (if needed)
    serializeComment().foreach(structure +=)

    items.foreach(ai => {
      val (i: YAML, c: Option[String]) = (ai.dropComment, ai.comment)
      c.foreach(c => structure += YAML.serializeComment(c))
      structure += i.serializeAs("- ")
    })

    structure.toList.mkString("\n")
  }

  /** generates a new sequence by adding values at the end of this existing list */
  def ++(s : Seq[YAML]) : YAMLSequence = YAMLSequence(items ++ s, comment)
  def ++(s : YAMLSequence) : YAMLSequence = s.items.toList match {
    case Nil => this
    case h :: tail => ++(YAML.prefixComment(h, s.comment) :: tail)
  }
}
object YAMLSequence {
  def empty : YAMLSequence = YAMLSequence(Nil, None)
  implicit def toSequence(ys: YAMLSequence): Seq[YAML] = ys.items
  def from(elems: YAML*) : YAMLSequence = fromSequence(elems)
  implicit def fromSequence(s : Seq[YAML]) : YAMLSequence = YAMLSequence(s, None)
}
