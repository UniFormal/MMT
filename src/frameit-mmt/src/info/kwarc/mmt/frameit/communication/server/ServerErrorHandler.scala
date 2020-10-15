package info.kwarc.mmt.frameit.communication.server

import java.io.{PrintWriter, StringWriter}

import io.circe.{Encoder, Json}

/**
  * Provides an implicit [[io.circe.Encoder JSON encoder]] for [[Exception exceptions]].
  *
  * Import this implicit value before you declare [[io.finch.Endpoint endpoints]] in order
  * for exceptions occurring in those endpoints (say, when the JSON payload sent by the HTTP
  * client was ill-formed) to be sent back to the client in an informative way.
  *
  * This helps to deviate from the unhelpful (at least for devs) behavior of Finch to swallow
  * all exceptions and to just send "BadRequest" to the client without any further information.
  *
  * Code copied from Finch's official documentation [1], under Apache License 2.0 [2].
  *
  * [1]: https://finagle.github.io/finch/user-guide.html#errors (
  * [2]: https://github.com/finagle/finch/blob/be0e7647b2fd8ac617d668313164e9a8c1c39af7/LICENSE
  */
object ServerErrorHandler {
  private def encodeErrorList(es: List[Exception]): Json = {
    val messages = es.map(x => Json.fromString(x.getMessage))
    Json.obj("errors" -> Json.arr(messages: _*))
  }

  private def formatThrowable(e: Throwable): String = {
    val sw = new StringWriter
    val pw = new PrintWriter(sw)

    e.printStackTrace(pw)
    e.getMessage + "\n\n" + sw.toString
  }

  implicit val encodeThrowable: Encoder[Throwable] = Encoder.instance({
    case e: io.finch.Errors => encodeErrorList(e.errors.toList)
    case e: io.finch.Error =>
      e.getCause match {
        case e: io.circe.Errors => encodeErrorList(e.errors.toList)
        case _ => Json.obj("message" -> Json.fromString(formatThrowable(e)))
      }
    case e: FactValidationException => e.asJson
    case e: Throwable => Json.obj("message" -> Json.fromString(formatThrowable(e)))
  })

  implicit val encodeException: Encoder[Exception] = Encoder.instance(e => encodeThrowable(e))
}
