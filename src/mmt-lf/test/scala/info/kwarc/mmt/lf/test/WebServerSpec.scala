package scala.info.kwarc.mmt.api.test
/*
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend._

import org.scalatest._

@Ignore
class WebServerSpec extends FlatSpec with Matchers {
  val controller = new Controller()
  val testLocation = "src/test/resources/WebServerTest/"
  //controller.handleLine("log console")
  //controller.handleLine("log+ archive")
  //controller.handleLine("log+ build")
  //controller.handleLine("log+ controller") 
  
  controller.handleLine("archive add " + testLocation)
  //controller.handleLine("build test mmt-omdoc")
  
  "Starting the web server" should "not throw an error" in {
    controller.handleLine("server on 8080")
  }
  
  it should "start the server on the given port" in {
	val server = controller.server.get
	server.port should be (8080)
  }
  
  "Stopping the web server" should "bring down the web interface" in {
    controller.handleLine("server off")
    a [NoSuchElementException] should be thrownBy {
      controller.server.get
    }
  }
  
}
*/