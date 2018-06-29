package info.kwarc.mmt.odk.OpenMath.Coding

import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.odk.OpenMath._

/**
  * Represents OpenMath encoded as JSON
  */
class OMJSONCoding extends OMCoding[JSON] {

  /**
    * Decodes a JSON encoded OpenMath object
    *
    * @param json JSON Object to decode
    * @return any OpenMath related object
    */
  def decode(json : JSON) : OMAny = json match {
    case o: JSONObject => o.getAsString("kind") match {
      case "OMOBJ" =>
        val parser = new JSONObjectParser(o)

        val omel = decodeExpression(parser.take[JSON]("object"))
        val version = parser.takeO[String]("version")

        val id = parser.takeO[String]("id")
        val cdbase = parser.takeO[String]("cdbase").map(URI.apply)

        OMObject(omel, version, id, cdbase)
      case _ =>
        decodeAnyVal(o)
    }
    // in case of an array,
    // we need to try to decode an attribution pair
    // or a bind variable
    case a: JSONArray => try {
        decodeAttributionPairs(a)
      } catch {
        case _: Exception => decodeBindVariables(a)
      }

    case _ => throw new Exception("decode() must get JSONObject or JSONArray")
  }

  private def decodeAttributionPairs(ary : JSONArray) : OMAttributionPairs = {
    val pairs = ary.values.map({
      case JSONArray(a, b) => (decodeSymbol(a), decodeAnyVal(b))
      case _ => throw new Exception("decodeAttributionPairs() got invalid JSOnArray")
    }).toList

    OMAttributionPairs(pairs, None, None)
  }

  private def decodeVar(json : JSON) : OMVar = json.asInstanceOf[JSONObject].getAsString("kind") match {
      case "OMV" => OMVarVar(decodeVariable(json))
      case "OMATTR" => {
        val parser = new JSONObjectParser(json.asInstanceOf[JSONObject])

        val pairs = decodeAttributionPairs(parser.take[JSON]("attributes").asInstanceOf[JSONArray])
        val value = decodeVar(parser.take[JSON]("object"))

        val id = parser.takeO[String]("id")

        OMAttVar(pairs, value, id)
      }
  }

  private def decodeBindVariables(ary : JSONArray) : OMBindVariables = {
    OMBindVariables(ary.values.map(decodeVar).toList, None)
  }

  def decodeAnyVal(json : JSON): OMAnyVal = decodeAnyValInt(new JSONObjectParser(json.asInstanceOf[JSONObject]))
  private def decodeAnyValInt(parser: JSONObjectParser): OMAnyVal = parser.take[String]("kind") match {
    case "OMR" =>
      val href = URI(parser.take[String]("href"))
      val id = parser.takeO[String]("id")

      OMReference(href, id)
    // Basic Elements
    case "OMI" =>
      val id = parser.takeO[String]("id")

      lazy val integer = parser.takeO[Int]("integer").map(BigInt(_))
      lazy val decimalInteger = parser.takeO[String]("decimalInteger").map(s => BigInt(s))
      lazy val hexInteger = parser.takeO[String]("hexInteger").map(
        s => BigInt(java.lang.Long.parseUnsignedLong(s.toLowerCase, 16))
      )

      val value = integer.getOrElse(decimalInteger.getOrElse(hexInteger.getOrElse(throw new Error("Invalid integer"))))

      OMInteger(value, id)

    case "OMF" =>
      val id = parser.takeO[String]("id")

      lazy val float = parser.takeO[Double]("float")
      lazy val decimal = parser.takeO[String]("decimal").map(s => s.trim.toDouble)
      lazy val hexadecimal = parser.takeO[String]("hexadecimal").map(
        s => OMCoding.hex2Double(s.trim)
      )

      val value = float.getOrElse(decimal.getOrElse(hexadecimal.getOrElse(throw new Error("invalid float"))))

      OMFloat(value, id)

    case "OMSTR" =>
      val id = parser.takeO[String]("id")
      val text = parser.take[String]("string")

      OMString(text, id)

    case "OMB" =>
      val id = parser.takeO[String]("id")

      lazy val base64 = parser.takeO[String]("base64").map(s => OMCoding.hex2Bytes(s))
      lazy val bytes = parser.takeO[List[Int]]("bytes").map(ary => ary.map(_.toByte))

      val value = base64.getOrElse(bytes.getOrElse(throw new Error("invalid bytes")))

      OMBytes(value, id)

    case "OMS" =>
      val name = parser.take[String]("name")
      val cd = parser.take[String]("cd")

      val id = parser.takeO[String]("id")
      val cdbase = parser.takeO[String]("cdbase").map(URI.apply)

      OMSymbol(name, cd, id, cdbase)

    case "OMV" =>
      val name = parser.take[String]("name")
      val id = parser.takeO[String]("id")

      OMVariable(name, id)

    // Derived Elements
    case "OMFOREIGN" =>
      val id = parser.takeO[String]("id")
      val cdbase = parser.takeO[String]("cdbase").map(URI.apply)
      val encoding = parser.takeO[String]("encoding")

      val obj = parser.take[JSON]("object")

      val fo: Any = try {
        decode(obj)
      } catch {
        case _: Exception => obj
      }

      OMForeign(fo, encoding, id, cdbase)

    // Compound elements
    case "OMA" =>
      val id = parser.takeO[String]("id")
      val cdbase = parser.takeO[String]("cdbase").map(URI.apply)

      val elem = decodeExpression(parser.take[JSON]("applicant"))
      val args = parser.takeO[List[JSON]]("arguments").getOrElse(List())
        .map(decodeExpression)

      OMApplication(elem, args, id, cdbase)

    case "OMATTR" =>
      val id = parser.takeO[String]("id")
      val cdbase = parser.takeO[String]("cdbase").map(URI.apply)

      val pairs = decodeAttributionPairs(parser.take[JSON]("attributes").asInstanceOf[JSONArray])
      val obj = decodeExpression(parser.take[JSON]("object"))

      OMAttribution(pairs, obj, id, cdbase)

    case "OMBIND" =>
      val id = parser.takeO[String]("id")
      val cdbase = parser.takeO[String]("cdbase").map(URI.apply)

      val binder = decodeExpression(parser.take[JSON]("binder"))
      val variables = decodeBindVariables(parser.take[JSON]("variables").asInstanceOf[JSONArray])
      val obj = decodeExpression(parser.take[JSON]("object"))

      OMBinding(binder, variables, obj, id, cdbase)

    case "OME" =>
      val id = parser.takeO[String]("id")
      val cdbase = parser.takeO[String]("cdbase").map(URI.apply)

      val error = decodeSymbol(parser.take[JSON]("error"))
      val params = parser.takeO[List[JSON]]("arguments").getOrElse(List())
        .map(decodeAnyVal)

      OMError(error, params, id, cdbase)

    case _ => throw new Error("Invalid OpenMath JSON object")
  }

  /**
    * Encodes an OpenMath object as JSON
    *
    * @param om OpenMath Object to encode
    * @return
    */
  def encode(om : OMAny) : JSON = om match {
    // match a top level object in here
    case OMObject(omel, version, id, cdbase) =>
      val obj = new JSONObjectBuffer

      obj.add("kind", "OMOBJ")
      obj.addO("openmath", version)

      obj.addO("id", id)
      obj.addO("cdbase", cdbase.map(_.toString))
      obj.add("object", encodeNode(omel))

      obj.result()

    // special cases => match specific function
    case p: OMAttributionPairs => encodeAttributionPairs(p)
    case v : OMVar => encodeVar(v)
    case b : OMBindVariables => encodeBindVariables(b)

    // fallback to matching a generic OpenMath NOde
    case n: OMAnyVal => encodeNode(n)
  }

  private def encodeAttributionPairs(p : OMAttributionPairs) : JSONArray = p match {
    case OMAttributionPairs(pairs, id, cdbase) =>
      val ary = new JSONListBuffer

      pairs.foreach({ab =>
        val pair = new JSONListBuffer
        pair += encode(ab._1)
        pair += encode(ab._2)
        ary += pair.result().asInstanceOf[JSON]
      })

      ary.result()
  }

  private def encodeVar(v : OMVar) : JSON = v match {
    case OMVarVar(vv) => encodeNode(vv)
    case OMAttVar(pairs, value, id) =>
      val obj = new JSONObjectBuffer

      obj.add("kind", "OMATTR")
      obj.addO("id", id)
      obj.add("attributes", encodeAttributionPairs(pairs).asInstanceOf[JSON])
      obj.add("object", encodeVar(value))

      obj.result()
  }
  private def encodeBindVariables(v : OMBindVariables) : JSONArray = v match {
    case OMBindVariables(vars, id) =>
      val ary = new JSONListBuffer

      vars.foreach({ v => ary += encodeVar(v)})

      ary.result()
  }

  private def encodeNode(om: OMAnyVal): JSON = om match {
    case OMReference(href, id) =>
      val obj = new JSONObjectBuffer

      obj.add("kind", "OMR")
      obj.add("href", href.toString)
      obj.addO("id", id)

      obj.result()
    // Basic Elements
    case OMInteger(int, id) =>
      val obj = new JSONObjectBuffer

      obj.add("kind", "OMI")
      obj.addO("id", id)
      obj.add("decimal", int.toString(10))

      obj.result()
    case OMFloat(dbl, id) =>
      val obj = new JSONObjectBuffer

      obj.add("kind", "OMF")
      obj.addO("id", id)
      obj.add("decimal", dbl.toString)

      obj.result()
    case OMString(text, id) =>
      val obj = new JSONObjectBuffer

      obj.add("kind", "OMSTR")
      obj.addO("id", id)
      obj.add("string", text)

      obj.result()
    case OMBytes(bytes, id) =>
      val obj = new JSONObjectBuffer

      obj.add("kind", "OMB")
      obj.addO("id", id)
      obj.add("base64", OMCoding.bytes2Hex(bytes))

      obj.result()
    case OMSymbol(name, cd, id, cdbase) =>
      val obj = new JSONObjectBuffer

      obj.add("kind", "OMS")
      obj.add("name", name)
      obj.add("cd", cd)
      obj.addO("id", id)
      obj.addO("cdbase", cdbase.map(_.toString))

      obj.result()
    case OMVariable(name, id) =>
      val obj = new JSONObjectBuffer

      obj.add("kind", "OMV")
      obj.add("name", name)
      obj.addO("id", id)

      obj.result()

    // Derived Elements
    case OMForeign(o, encoding, id, cdbase) =>
      val obj = new JSONObjectBuffer

      obj.add("kind", "OMFOREIGN")
      obj.addO("encoding", encoding)
      obj.addO("id", id)
      obj.addO("cdbase", cdbase.map(_.toString))

      o match {
        case a: OMAny =>
          obj.add("foreign", encode(a))
        case j: JSON =>
          obj.add("foreign", j)
        case _ =>
          obj.add("foreign", o.toString)
      }

      obj.result()

    // Compound elements
    case OMApplication(elem, args, id, cdbase) =>
      val obj = new JSONObjectBuffer

      obj.add("kind", "OMA")
      obj.addO("id", id)
      obj.addO("cdbase", cdbase.map(_.toString))
      obj.add("applicant", encode(elem))

      val oargs = new JSONListBuffer
      args.foreach { a => oargs += encode(a)}
      obj.add("arguments", oargs.result().asInstanceOf[JSON])

      obj.result()

    case OMAttribution(pairs, o, id, cdbase) =>
      val obj = new JSONObjectBuffer

      obj.add("kind", "OMATTR")
      obj.addO("id", id)
      obj.addO("cdbase", cdbase.map(_.toString))

      obj.add("attributes", encodeAttributionPairs(pairs).asInstanceOf[JSON])
      obj.add("object", encode(o))

      obj.result()
    case OMBinding(a, vars, c, id, cdbase) =>
      val obj = new JSONObjectBuffer

      obj.add("kind", "OMBIND")
      obj.addO("id", id)
      obj.addO("cdbase", cdbase.map(_.toString))

      obj.add("binder", encode(a))
      obj.add("variables", encodeBindVariables(vars).asInstanceOf[JSON])
      obj.add("object", encode(c))

      obj.result()
    case OMError(name, params, id, cdbase) =>
      val obj = new JSONObjectBuffer

      obj.add("kind", "OME")
      obj.addO("id", id)
      obj.addO("cdbase", cdbase.map(_.toString))

      obj.add("error", encode(name))

      val pargs = new JSONListBuffer
      params.foreach { a => pargs += encode(a)}
      obj.add("arguments", pargs.result().asInstanceOf[JSON])

      obj.result()
  }

}


