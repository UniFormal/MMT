package info.kwarc.mmt.frameit.communication.server

import java.io.{PrintWriter, StringWriter}

import cats.effect.IO
import com.twitter.finagle.Service
import com.twitter.finagle.http.{Request, Response, Status}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.presentation.MMTSyntaxPresenter
import info.kwarc.mmt.api.symbols.Constant
import io.circe.{Encoder, Json}
import io.finch._

import scala.util.Try

sealed abstract class ValidationException(message: String, cause: Throwable = None.orNull)
  extends Exception(message, cause)

sealed case class ProcessedFactDebugInfo(tpAST: String, dfAST: String, presentedString: String, omdocXml: String) {
  def asJson: Json = Json.obj(
    "tpAST" -> Json.fromString(tpAST),
    "dfAST" -> Json.fromString(dfAST),
    "presentedString" -> Json.fromString(presentedString),
    "omdocXml" -> Json.fromString(omdocXml)
  )
}
object ProcessedFactDebugInfo {
  def fromConstant(c: Constant)(implicit ctrl: Controller, presenter: MMTSyntaxPresenter): ProcessedFactDebugInfo = {
    ProcessedFactDebugInfo(
      // avoid bubbling up exceptions to always have at least some debug information instead of none
      c.tp.map(_.toString).getOrElse("<no type available>"),
      c.df.map(_.toString).getOrElse("<no definiens available>"),
      Try(presenter.asString(c)).getOrElse("<presentation threw exception>"),
      Try(c.toNode.toString()).getOrElse("<conversion to XML threw exception>")
    )
  }
}

final case class FactValidationException(message: String, processedFacts: List[ProcessedFactDebugInfo], cause: Throwable = None.orNull) extends ValidationException(message, cause) {
  def asJson: Json = Json.obj(
    "message" -> Json.fromString(message),
    "processedFacts" -> Json.arr(processedFacts.map(_.asJson) : _*),
    "cause" -> Json.fromString(Option(cause).toString)
  )
}

/**
  * Some general-purpose boilerplate for Finch servers whose endpoints are encapsulated in an object.
  *
  * - succeeded and failed requests are logged to stdout
  * - failed requests (due to exceptions) are served with an HTTP-500 response with a body detailing
  *   the exception and its stracktrace
  *
  * The latter greatly enhances the debugging experience.
  *
  * How to use:
  *
  * '''
  * object MyConcreteEndpoints extends ServerEndpoints {
  *   import ServerErrorHandler._ // don't forget lest you get uninformative errors!
  *
  *   override protected def getCompiledOverallEndpoint(state: ServerState): Endpoint.Compiled[IO] = {
  *     // ...
  *   }
  * '''
  *
  * See [[ConcreteServerEndpoints]] for an example.
  */
trait ServerEndpoints extends Endpoint.Module[IO] {

  import cats.Applicative.ops.toAllApplicativeOps
  // vvvvvvv CAREFUL WHEN REMOVING IMPORTS (IntelliJ might wrongly mark them as unused)
  import ServerErrorHandler._
  // ^^^^^^^ END

  protected def getCompiledOverallEndpoint(state: ServerState): Endpoint.Compiled[IO]

  def getServiceForState(state: ServerState): Service[Request, Response] = {
    // Endpoint.toService(filters(getCompiledOverallEndpoint(state)))
    // TODO: see https://github.com/finagle/finch/issues/1630
    ???
  }

  private def filters = Function.chain(Seq(exceptionLogging, logging))

  private def logging: Endpoint.Compiled[IO] => Endpoint.Compiled[IO] = compiled => {
    compiled.tapWithF { (req, res) =>
      IO(println(s"[${res._2.map(_.statusCode).getOrElse("BAD")}] $req")) *> IO.pure(res)
    }
  }

  private def exceptionLogging: Endpoint.Compiled[IO] => Endpoint.Compiled[IO] = compiled => {
    compiled.tapWithF { (_, res) => res match {
        case (trace, Left(throwable)) =>
          IO(throwable.printStackTrace()) *> IO({
            val newResponse = Response(Status.InternalServerError)
            newResponse.write(ThrowableUtils.formatThrowableToJson(throwable).toString())

            (trace, Right(newResponse))
          })
        case _ => IO.pure(res)
      }
    }
  }

  private object ThrowableUtils {
    private def formatStackTraceToString(throwable: Throwable): String = {
      val sw = new StringWriter
      val pw = new PrintWriter(sw)

      try {
        throwable.printStackTrace(pw)
        sw.toString
      } finally {
        sw.close()
        pw.close()
      }
    }

    def formatThrowableToJson(throwable: Throwable): Json = {
      try {
        Json.obj(
          "message" -> Json.fromString(throwable.getMessage),
          "stacktrace" -> Json.fromString(formatStackTraceToString(throwable))
        )
      } catch {
        case _: Throwable =>
          Json.obj(
            "message" -> Json.fromString(
              "Exception occurred during formatting a previous exception\nTo prevent further recursion down the rabbit hole, no exception information is formatted or even output this time."
            )
          )
      }
    }
  }
}


/**
  * Provides an implicit [[io.circe.Encoder JSON encoder]] for [[Exception exceptions]].
  *
  * Import this implicit value before you declare [[io.finch.Endpoint endpoints]] and/or
  * compile those endpoints in order for exceptions occurring in those endpoints (say, when
  * the JSON payload sent by the HTTP client was ill-formed) to be sent back to the client
  * in an informative way.
  *
  * This helps to deviate from the unhelpful (at least for devs) behavior of Finch to swallow
  * all exceptions and to just send "BadRequest" to the client without any further information.
  *
  * Code copied and adapted from Finch's official documentation [1], under Apache License 2.0 [2].
  *
  * [1]: https://finagle.github.io/finch/user-guide.html#errors (
  * [2]: https://github.com/finagle/finch/blob/be0e7647b2fd8ac617d668313164e9a8c1c39af7/LICENSE
  */
private[server] object ServerErrorHandler {
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

  implicit val encodeException: Encoder[Exception] = Encoder.instance({
    case e: io.finch.Errors => encodeErrorList(e.errors.toNonEmptyList.toList)
    case e: io.finch.Error =>
      e.getCause match {
        case e: io.circe.Errors => encodeErrorList(e.errors.toList)
        case _ => Json.obj(
          "message" -> Json.fromString(e.getMessage),
          "details" -> Json.fromString(formatThrowable(e))
        )
      }
  })
}
