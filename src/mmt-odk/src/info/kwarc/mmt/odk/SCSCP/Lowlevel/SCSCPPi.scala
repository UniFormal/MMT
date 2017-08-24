package info.kwarc.mmt.odk.SCSCP.Lowlevel

import scala.xml.{Attribute, Null, Text}

/**
  * Represents a single XML processing instruction
 */
case class SCSCPPi(key : Option[String], attributes : Map[String, String]) {
  override def toString : String = {
      // create a normal node
      val node = attributes.toList.map(np => Attribute(None, np._1, Text(np._2), Null)).foldLeft(<scscp />)(_ % _).toString

      // and replace with a regex
      "^<scscp(\\s*)(.*)(\\s*)/>$".r.replaceFirstIn(node, key match {
        case Some(k) => "<?scscp "+k+" $2 ?>"
        case None => "<?scscp $2 ?>"
      })
  }

  /**
    * Returns the value of the given attribute
    * @param key
    * @return
    */
  def apply(key : String) = attributes(key)

  /**
    * Returns the key of this SCSCPI
    * @return
    */
  def apply() = key.get
}

object SCSCPPi {
  private val regex_parse = "^<\\?scscp\\s+(([a-zA-Z0-9]+)\\s)?(.*)\\?>$".r

  /**
    * Parses a string into an SCSCP Processing instruction
    * @param data
    * @return
    */
  def apply(data : String): SCSCPPi = {
    // match the regular expression to the data
    val data_match = regex_parse.findFirstMatchIn(data) match {
      case Some(thing) => thing
      case None => return SCSCPPi(Map.empty[String,String])
    }

    // extract the key
    val key = data_match.group(1) match {
      case null => None
      case k => Some(k.dropRight(1))
    }

    //parse the attributes
    val node = scala.xml.XML.loadString("<scscp "+data_match.group(3)+" />")
    val atts = node.attributes.map((att: scala.xml.MetaData) => (att.key, att.value.head.text)).toMap

    // and return a proper XMLPi
    SCSCPPi(key, atts)
  }

  def apply(attributes : Map[String, String]) : SCSCPPi = SCSCPPi(None, attributes)
}