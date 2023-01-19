// twiesing 18-09-2018: Remove call to deprecated methods
// twiesing 23-08-2022: Remove calls to deprecated APIs
package tiscaf

import scala.jdk.CollectionConverters._

object Test extends App with HServer {
  def ports = Set(8910)
  def apps = List(app)

  override def startStopListener: Unit = { }

  object app extends HApp {
    def resolve(req: HReqData) = req.uriPath match {
      case "resume" => Some(resumeLet)
      case path => Some(new SuspendLet(path))
    }
  }

  val pending = new java.util.concurrent.ConcurrentHashMap[Suspended[Int],Unit]()

  var i = 0
  def next = {
    i += 1
    i
  }

  object resumeLet extends HSimpleLet {
    def act(talk: HTalk): Unit = {
      pending.keys.asScala map { s =>
        s.resume(next)
        s
      } foreach pending.remove
      talk.setContentLength(3).write("ok\n")
    }
  }

  import scala.reflect.runtime.{universe => ru}
  class SuspendLet(path: String) extends HLet with HSuspendable {

    import scala.concurrent.ExecutionContext.Implicits.global

    def aact(talk: HTalk) = {

      println("path: " + path)

      for {
        i <- suspend[Int]
        j <- suspend[Int]
        result = path + "#" + i + "#" + j + "\n"
      } yield talk.setContentLength(result.size).write(result)

    }


    def onSuspend[T: ru.TypeTag](suspended: Suspended[T]): Unit = {
      val mirror = ru.runtimeMirror( getClass.getClassLoader )
      val classSym = mirror.classSymbol( suspended.getClass.getComponentType )


      suspended match {
        case s: Suspended[Int@unchecked] if classSym.toType <:< implicitly[ru.TypeTag[Int]].tpe =>
          pending.asScala.put(s, ())
        case _ => // ignore
      }
    }

  }

  start
}
