package info.kwarc.mmt.api.utils


/** A class to quickly convert objects between Scala and JSON */
@MMT_TODO("this is unused, but is kept to be unified with ScalaTo in a future refactor")
trait JSONConverter[T] {
  def toJSON(obj: T) : JSON

  def fromJSON(j: JSON): T = fromJSONOption(j).getOrElse(throw ConverterNotApplicable(j))
  def fromJSONOption(j: JSON) : Option[T]
}
case class ConverterNotApplicable(json: JSON) extends Exception(s"Not applicable: $json")


object JSONConverter {

  def toJSON[T](obj: T)(implicit converter: JSONConverter[T]) : JSON = converter.toJSON(obj)
  def fromJSON[T](json: JSON)(implicit converter: JSONConverter[T]): T = converter.fromJSON(json)
  def fromJSONOption[T](json: JSON)(implicit converter: JSONConverter[T]): Option[T] = converter.fromJSONOption(json)

  implicit object convertJSON extends JSONConverter[JSON] {
    def toJSON(j: JSON): JSON = j
    def fromJSONOption(j: JSON): Option[JSON] = Some(j)
  }

  implicit object convertIn extends JSONConverter[BigInt] {
    def toJSON(i: BigInt) = JSONInt(i)
    def fromJSONOption(j : JSON): Option[BigInt] = j match {
      case JSONInt(x) => Some(x)
      case _ => None
    }
  }

  implicit object convertDouble extends JSONConverter[BigDecimal] {
    def toJSON(d: BigDecimal) = JSONFloat(d)
    def fromJSONOption(j : JSON): Option[BigDecimal] = j match {
      case JSONFloat(x) => Some(x)
      case _ => None
    }
  }

  implicit object convertBoolean extends JSONConverter[Boolean] {
    def toJSON(b: Boolean) = JSONBoolean(b)
    def fromJSONOption(j: JSON): Option[Boolean] = j match {
      case JSONBoolean(x) => Some(x)
      case _ => None
    }
  }

  implicit object convertString extends JSONConverter[String] {
    def toJSON(s: String) = JSONString(s)
    def fromJSONOption(j: JSON): Option[String] = j match {
      case JSONString(x) => Some(x)
      case _ => None
    }
  }

  implicit def OptC[T](implicit member: JSONConverter[T]) = new  JSONConverter[Option[T]] {
    def toJSON(opt: Option[T]): JSON = opt.map(m => member.toJSON(m)).getOrElse(JSONNull)
    def fromJSONOption(j: JSON): Option[Option[T]] = j match {
      case JSONNull => Some(None)
      case x@_ => member.fromJSONOption(x).map(y => Some(y))
    }
  }

  implicit def ListC[T](implicit member: JSONConverter[T]) = new JSONConverter[List[T]] {
    def toJSON(seq: List[T]) = JSONArray(seq.map(m => member.toJSON(m)):_*)
    def fromJSONOption(j: JSON): Option[List[T]] = j match {
      case JSONArray(x @ _*) =>
        val y = x.map(member.fromJSONOption).toList
        if(y.forall(_.nonEmpty)){
          Some(y.map(_.get))
        } else {
          None
        }
      case _ => None
    }
  }
}