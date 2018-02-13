package tiscaf
package test

object TestException extends HServer with App {

  protected def ports = Set(8080)
  protected def apps = List(exnApp)

  object exnApp extends HApp {
    def resolve(req: HReqData) = Some(exnLet)

    object exnLet extends HSimpleLet {
      def act(talk: HTalk) {
        throw new Exception("plop")
      }
    }
  }

  start
  Console.readLine
  new HStop("localhost", 8911).stop

}
