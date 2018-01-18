package tiscaf
package test

object EchoServer extends HServer with App {

  // implementing HServer
  protected def ports = Set(8080) // ports to listen to
  protected def apps = List(theApp)

  // the only HApp
  object theApp extends HApp {

    def resolve(req: HReqData) = Some(echoLet) // the only handler 

    object echoLet extends HSimpleLet {
      def act(talk: HTalk): Unit = {
        println(talk.req.param("source"))
        println(talk.req.softParam("source"))
      }
    }
  }

  start
}

object EchoServerStop { def main(args: Array[String]) = new tiscaf.HStop("localhost", 8911) stop }
