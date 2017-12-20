package src.main.scala.yaml

import scala.collection.mutable.ListBuffer

/** represents a key-value map of YAML Objects */
case class YAMLStructure(map: Map[String, YAML], comment: Option[String]) extends YAMLImplementation {
  def setComment(comment : Option[String]) : YAMLStructure = copy(comment = comment)

  /** creates a new YAMLStructure by adding a set of key-value pairs to this structure */
  def ++(m : Map[String, YAML]) : YAMLStructure = YAMLStructure(map ++ m, comment)

  /** creates a new YAML Structure by adding a second set of key-value pairs to this strucutre */
  def ++(s : YAMLStructure) : YAMLStructure = {
    // prefix the first comment to the list that is being added
    val newMap = s.map.toList
    (newMap.head._1, YAML.prefixComment[YAML](newMap.head._2, s.comment)) :: newMap.tail
    // and add that map
    ++(newMap)
  }

  /** creates a new YAML Structure by removing a given set of key-value pairs from this structure */
  def --(keys: String*) : YAMLStructure = YAMLStructure(map -- keys, comment)

  /** serializes this structure into a string */
  def serialize: String = {
    // create a new array of lines for this string
    val structure = new ListBuffer[String]

    // add a comment (if needed)
    serializeComment().foreach(structure +=)

    // add key: value for each string
    map.foreach(kv => {
      val (key: String, comment: Option[String], value: YAML) = (kv._1, kv._2.comment, kv._2.dropComment)

      // add the comment
      comment.map(YAML.serializeComment(_)).foreach(structure +=)


      val keySer = YAMLString.serializeString(key) + ": "

      if(value.isInstanceOf[YAMLLiteral]){
        structure += value.serializeAs(keySer)
      } else {
        structure += keySer
        structure += value.serializeAs(s"  ")
      }
    })

    // and make it a string
    structure.toList.mkString("\n")
  }
}
object YAMLStructure {
  def empty : YAMLStructure = YAMLStructure(Map(), None)
  implicit def toMap(ys: YAMLStructure): Map[String, YAML] = ys.map.map(kv => (YAMLString.toString(kv._1), kv._2))
  implicit def fromMap(mp: Map[String, YAML]) : YAMLStructure = YAMLStructure(mp, None)
}
