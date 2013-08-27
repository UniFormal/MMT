package tiscaf

import scala.concurrent._
import scala.util._

object Test extends App with HServer {
  def ports = Set(8910)
  def apps = List(app)

  override def startStopListener { }

  object app extends HApp {
    def resolve(req: HReqData) = req.uriPath match {
      case "resume" => Some(resumeLet)
      case path => Some(new SuspendLet(path))
    }
  }

  import scala.collection.mutable._

  val pending = new HashSet[Suspended[Int]] with SynchronizedSet[Suspended[Int]]

  var i = 0
  def next = {
    i += 1
    i
  }

  object resumeLet extends HSimpleLet {
    def act(talk: HTalk) {
      val toRemove = pending map { s =>
        s.resume(next)
        s
      }
      pending --= toRemove
      talk.setContentLength(3).write("ok\n")
    }
  }

  class SuspendLet(path: String) extends HLet with HSuspendable {

    def aact(talk: HTalk)(implicit executionContext: ExecutionContext) = {

      println("path: " + path)

      for {
        i <- suspend[Int]
        j <- suspend[Int]
        result = path + "#" + i + "#" + j + "\n"
      } yield talk.setContentLength(result.size).write(result)

    }

    def onSuspend[T: Manifest](suspended: Suspended[T]) {
      suspended match {
        case s: Suspended[Int] if manifest[T] <:< manifest[Int] =>
          pending += s
        case _ => // ignore
      }
    }

  }

  start
}
