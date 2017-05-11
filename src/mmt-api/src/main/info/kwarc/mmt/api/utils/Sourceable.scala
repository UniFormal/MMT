package info.kwarc.mmt.api.utils

import scala.xml.Node

/** data structure for everything that can be turned into a scala-compatible source string */
trait Sourceable {
  /** turns this object into a string that can be parsed by scala */
  def toSourceString: String
}

/** helper class for requirement that anything which can be safely sourced */
sealed trait SafeSourceableType[-T] {
  def toSource(member : T) : String
}

object SafeSourceableType {

  /** a sourceable itself can be safely turned into a source string */
  implicit object SourceableType extends SafeSourceableType[Sourceable] {
    def toSource(member : Sourceable) : String = member.toSourceString
  }


  // XML Node
  implicit object NodeType extends SafeSourceableType[Node] {
    def toSource(member : Node) : String = s"scala.xml.XML.loadString(${StringType.toSource(member.toString)})"
  }

  // Strings and Characters
  implicit object StringType extends SafeSourceableType[String] {
    def toSource(member: String): String =
      if(member.contains("\n")){
        "\"\"\"" + member.toString + "\"\"\""
      } else {
        "\"" + StandardStringEscaping(member) + "\""
      }
  }
  implicit object CharType extends SafeSourceableType[Char] {
    def toSource(member: Char): String = s"'$member'"
  }

  // Boolean
  implicit object BooleanType extends SafeSourceableType[Boolean] {
    def toSource(member: Boolean): String = member.toString
  }

  // Numeric Types
  implicit object ByteType extends SafeSourceableType[Byte] {
    def toSource(member: Byte): String = s"${IntType.toSource(member.toInt)}.toByte"
  }

  implicit object ShortType extends SafeSourceableType[Short] {
    def toSource(member: Short): String = s"${StringType.toSource(member.toString)}.toShort"
  }
  implicit object IntType extends SafeSourceableType[Int] {
    def toSource(member: Int): String = s"${StringType.toSource(member.toString)}.toInt"
  }
  implicit object LongType extends SafeSourceableType[Long] {
    def toSource(member: Long): String = s"${StringType.toSource(member.toString)}.toLong"
  }
  implicit object FloatType extends SafeSourceableType[Float] {
    def toSource(member: Float): String = s"${StringType.toSource(member.toString)}.toFloat"
  }
  implicit object DoubleType extends SafeSourceableType[Double] {
    def toSource(member: Double): String = s"${StringType.toSource(member.toString)}.toDouble"
  }

  // higher order types
  implicit def ListType[T: SafeSourceableType] : SafeSourceableType[List[T]] = new SafeSourceableType[List[T]] {
    override def toSource(member: List[T]): String = {
      val evidence = implicitly[SafeSourceableType[T]]
      member.map(evidence.toSource).mkString(",")
    }
  }

  implicit def OptionType[T: SafeSourceableType] : SafeSourceableType[Option[T]] = new SafeSourceableType[Option[T]] {
    override def toSource(member: Option[T]): String = {
      val evidence = implicitly[SafeSourceableType[T]]
      member.map(e => s"Some(${evidence.toSource(e)})").getOrElse("None")
    }
  }
}


object Sourceable {
  def apply[T](s : T)(implicit evidence: SafeSourceableType[T]) : Sourceable = new Sourceable {
    override def toSourceString: String = evidence.toSource(s)
    override def toString: String = toSourceString
  }
}