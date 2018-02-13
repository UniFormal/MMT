package travis.yaml

import scala.collection.mutable.ListBuffer

/**
  * A Structure, i.e. key-value mapping of Strings to YAML Options
  * @param map Mappings within this YAMLStructure
  * @param comment Optional comment
  */
case class YAMLStructure(map: Map[String, YAML], comment: Option[String] = None) extends YAMLImplementation {
  /** Creates a new [[YAMLStructure]] with the given comment */
  protected def setComment(comment : Option[String]) : YAMLStructure = copy(comment = comment)

  /** creates a new [[YAMLStructure]] by adding a set of key-value pairs to this structure */
  def ++(m : Map[String, YAML]) : YAMLStructure = YAMLStructure(map ++ m, comment)

  /** creates a new [[YAMLStructure]] by adding another structure to the end of this list */
  def ++(s : YAMLStructure) : YAMLStructure = {
    val newMap = s.map.toList
    ++(
      // prefix the first comment to the list that is being added
      ((newMap.head._1, YAML.prefixComment[YAML](newMap.head._2, s.comment)) :: newMap.tail).toMap
    )
  }

  /** creates a new [[YAMLStructure]] by removing a given set of key-value pairs from this structure */
  def --(keys: String*) : YAMLStructure = YAMLStructure(map -- keys, comment)

  /** serialize this structure into a string */
  def serialize: String = {
    // create a new array of lines for this string
    val structure = new ListBuffer[String]

    // add a comment (if needed)
    serializeComment().foreach(structure +=)

    // add key: value for each string
    map.toList.sortBy(_._1).foreach(kv => {
      val (key: String, comment: Option[String], value: YAML) = (kv._1, kv._2.comment, kv._2.dropComment)

      // add the comment
      comment.map(YAML.serializeComment(_)).foreach(structure +=)

      // literals get serialized in-line
      val keySer = YAMLString.serializeString(key) + ":"
      if(value.isInstanceOf[YAMLLiteral]){
        structure += value.serializeAs(keySer + " ")
      // other values are serialized in a new line with more spaces
      } else {
        structure += keySer
        structure += value.serializeAs(s"  ")
      }
    })

    // and make it a string
    structure.toList.mkString("\n")
  }

  /** turns this Structure into the underlying map object */
  implicit def toMap: Map[String, YAML] = map
}

object YAMLStructure {
  /** create an empty [[YAMLStructure]] */
  def empty : YAMLStructure = YAMLStructure(Map(), None)
  /** creates a new [[YAMLStructure]] using only a map */
  implicit def apply(map: Map[String, YAML]) : YAMLStructure = new YAMLStructure(map, None)
}
