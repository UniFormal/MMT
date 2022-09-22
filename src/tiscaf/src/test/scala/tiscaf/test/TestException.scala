// twiesing 18-09-2018: Remove call to deprecated methods
package tiscaf
package test

import scala.io.StdIn

object TestException extends HServer with App {

  protected def ports = Set(8080)
  protected def apps = List(exnApp)

  object exnApp extends HApp {
    def resolve(req: HReqData) = Some(exnLet)

    object exnLet extends HSimpleLet {
      def act(talk: HTalk): Unit = {
        throw new Exception("plop")
      }
    }
  }

  start
  StdIn.readLine()
  new HStop("localhost", 8911).stop

}
