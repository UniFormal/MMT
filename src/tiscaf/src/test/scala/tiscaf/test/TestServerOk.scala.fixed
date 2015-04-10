package tiscaf
package test

import org.scalatest._
import scala.concurrent.ExecutionContext.Implicits.global
import dispatch._

class TestServerOk extends FlatSpec with ShouldMatchers with BeforeAndAfterAll {
  object server extends HServer {
    protected def ports = Set(8910)
    protected val apps = List(theApp)

    override protected def startStopListener {}

   object theApp extends HApp {
      override def keepAlive = true
      override def chunked   = false
      override def buffered  = false
      override def gzip      = false
      override def tracking  = HTracking.NotAllowed

      def resolve(req: HReqData) = Some(Let)
    }

    object Let extends HSimpleLet {
      private val bytes = "Ok".getBytes("UTF-8")
      def act(tk: HTalk) {
        tk.setContentLength(bytes.size)
          .setContentType("text/plain; charset=UTF-8")
          .setContentLength(bytes.length) // if not buffered
          .write(bytes)
          ()
      }
    }

  }

  override def beforeAll(configMap: ConfigMap) {
    server.start
  }

  override def afterAll(configMap: ConfigMap) {
    server.stop
    Http.shutdown
  }

  def get(path: String) = Http(url("http://localhost:8910" + path) OK as.String)

  "the server" should "answer OK for each request" in {

    val results: List[String] =
      (for (
      res1 <- get("");
      res2 <- get("/test");
      res3 <- get("/some/other/path.html");
      res4 <- get("/gruik")) yield List(res1, res2, res3, res4)).apply()

    results foreach (_ should be("Ok"))

  }

}
