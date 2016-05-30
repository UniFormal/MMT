package info.kwarc.mmt.odk.SCSCP

import scala.xml.{Attribute, Text, Null}

/**
  * Represents a single XML processing instruction
 */
case class XMLPi(name : String, attributes : Map[String, String]) {
  override def toString : String = {
      // create a normal node
      val node = attributes.toList.map(np => Attribute(None, np._1, Text(np._2), Null)).foldLeft(scala.xml.XML.loadString("<"+name+" />"))(_ % _).toString

      // and replace with a regex
      "^<(.*)/>$".r.replaceFirstIn(node, "<?$1 ?>")
  }
}

object XMLPi {
  def apply(data : String): XMLPi = {
    // Regex turn it into a normal node
    val nn = "^<\\?(.*)\\?>$".r.replaceFirstIn(data, "<$1/>")

    // parse it
    val node = scala.xml.XML.loadString(nn)

    // and return a proper XMLPi
    XMLPi(node.label, node.attributes.map((att: scala.xml.MetaData) => (att.key, att.value.head.text)).toMap)
  }
}