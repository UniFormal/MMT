package tiscaf
package test

object HomeServer extends HServer with App {

  // implementing HServer
  protected def ports = Set(8910) // ports to listen to
  protected def apps = List(theApp)

  // the only HApp
  object theApp extends HApp {

    def resolve(req: HReqData) = Some(homeLet) // the only handler 

    object homeLet extends let.FsLet {
      protected def dirRoot = System.getProperty("user.home")

      override protected def uriRoot = "" // i.e. localhost:8910/
      override protected def allowLs = true // show directory listing
      override protected def indexes: Seq[String] = Nil
      override protected def bufSize = HomeServer.bufferSize
    }
  }

  start
}

object HomeServerStop { def main(args: Array[String]) = new tiscaf.HStop("localhost", 8911) stop }
