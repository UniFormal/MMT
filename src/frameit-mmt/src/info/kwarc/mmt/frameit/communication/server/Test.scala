package info.kwarc.mmt.frameit.communication.server

import io.finch._
import cats.effect.IO
import com.twitter.finagle.Http
import com.twitter.io.Buf
import com.twitter.util.Await

object Test extends App with Endpoint.Module[IO] {
  implicit val encodeException: Encode.Text[Exception] =
    Encode.text((_, cs) => Buf.ByteArray.Owned("ERR!".getBytes(cs.name)))

  val api: Endpoint[IO, String] = get("hello") {
    throw new Exception("test")
    Ok("Hello, World!")
  }
  Await.ready(Http.server.serve(":8080", api.toServiceAs[Text.Plain]))
}