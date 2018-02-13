package info.kwarc.mmt.api.web

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.frontend.actions._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.utils.URI.toJava

/**
 * client for remote administration
 *
 * creates a separate thread which perpetually connects to a [[RemoteAdminServer]] to
 * - send the log output of the previous action
 * - retrieve the next action to execute
 */
class RemoteAdminClient extends Extension {
   private val repHand = new RecordingHandler("remote")

   override def start(args: List[String]) {
      val (id,urlS) = args match {
        case List(a,b) => (a,b)
        case _ => throw LocalError("expected 2 arguments")
      }
      controller.report.addHandler(repHand)
      val t = new ListenThread(URI(urlS).toURL, id)
      t.start
   }

   override def destroy {
     synchronized {
       continue = false
     }
     controller.report.removeHandler(repHand.id)
   }

   private var continue = true

   private class ListenThread(url: java.net.URL, id: String) extends Thread {
      var data: List[String] = Nil
      override def run {
        while (continue) {
          try {
            val actN = utils.xml.post(url, <ready id={id}>{data.map(s => <data value={s}/>)}</ready>)
            data = Nil
            actN match {
              case <action/> =>
                val actS = xml.attr(actN, "value")
                repHand.record
                controller.handleLine(actS)
                data = repHand.stop
                repHand.clear
              case <quit/> =>
                continue = false
              case <error>{data @ _*}</error> =>
                report("user", "error in server: " + actN.toString)
            }
          } catch {case e: Exception =>
            data = List("error while processing previous action: " + Error(e).toStringLong)
          }
        }
      }
   }
}

/** a [[RemoteAdminClient]] as as seen from a [[RemoteActionServer]] */
class RemoteAdminConnection(val client: String) {
   private var actions: List[RemoteAction] = Nil
   /** enqueue an action to be run on this client */
   def apply(a: RemoteAction) {
     actions = actions ::: List(a)
   }
   /** dequeue the next action to run on this client */
   def get = {
     actions match {
       case Nil => None
       case h::t =>
         actions = t
         Some(h)
     }
   }
}

import ServerResponse._

/** can be contacted by a [[RemoteAdminClient]] instances running on any machine to receive actions
 *
 *  the protocol is as follows:
 *    client sends <ready> <data value=OUTPUTLINE/>* </ready>
 *    server sends <action value=ACTION/> and reports all OUTPUTLINEs
 *              or <quit/> to terminate the connection
 */
class RemoteAdminServer extends ServerExtension("remote") {
   private var clients: List[RemoteAdminConnection] = Nil

   def apply(request: ServerRequest): ServerResponse = {
      request.body.asXML match {
        case n @ <ready>{dataN @ _*}</ready> =>
          val id = xml.attr(n, "id")
          val client = clients.find(_.client == id).getOrElse {
            val c = new RemoteAdminConnection(id)
            clients ::= c
            c
          }
          val data = dataN collect {
            case n @ <data/> => xml.attr(n, "value")
          }
          if (data.nonEmpty)
            report("user", s"*** remote output at $id: " + data.mkString("\n","\n","\n") + "*** end remote output")
          val act = While.undefined(client.get) {
            Thread.sleep(1000)
          }
          XmlResponse(<action value={act.action.toString}/>)
      }
   }

   /** enqueue an action that is to be handled on a remote client */
   def apply(ra: RemoteAction) {
      val client = clients.find(_.client == ra.id).getOrElse {
        throw LocalError("unknown client: " + ra.id)
      }
      client(ra)
   }
}
