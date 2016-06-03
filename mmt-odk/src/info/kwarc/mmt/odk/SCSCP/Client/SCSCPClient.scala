package info.kwarc.mmt.odk.SCSCP.Client

import java.net.Socket

import info.kwarc.mmt.odk.SCSCP.{ProtocolError, SCSCPPi, SCSCPSocket}


/**
  * Implement an Symbolic Computation Software Composability Protocol Client
  *
  * @param host Hostname of server to connect to
  * @param port Port number to connect to. Defaults to 26133.
  * @param versions Version(s) to be used by this SCSCP client. Will be prioritised in order.
  */
class SCSCPClient(host : String, port : Int = SCSCPClient.default_port, versions : List[String] = "1.3" :: Nil) {

  /**
    * The underlying SCSCPSocket used by this SCSCPClient.
    */
  private val socket : SCSCPSocket = new SCSCPSocket(new Socket(host, port))

  /**
    * Connection code
    */
  val (service_name : String, service_version : String, service_id : String, scscp_version : String) =
  {
    // expect a processing instruction with none
    val vpi = socket.expect_any_inst_with(None, "service_name", "service_version", "service_id")

    //find the supported server versions
    val server_versions = vpi("scscp_versions").split(" ")

    // and select the one we can use or quit
    val used_version = versions.filter(server_versions.contains(_)) match {
      case Nil =>
        quit(Some("version not supported"))
        throw new ProtocolError("Server does not support SCSCP version")
      case h :: _ => h
    }

    // make a PI for the version
    val version_pi = SCSCPPi(Map(("version", used_version)))

    // send it an expect it back
    socket.provide(version_pi)
    socket.expect_this_inst(version_pi)

    // return the service name + version + id
    (vpi("service_name"), vpi("service_version"), vpi("service_id"), used_version)
  }

  /**
    * Checks if this socket is still connected
    * @return
    */
  def isConnected : Boolean = socket.socket.isConnected

  /**
    * Quits the session with the SCSCP server
    *
    * @param reason Reason for quitting. Optional.
    */
  def quit(reason : Option[String] = None) : Unit = {
    // build the reson
    val mp : Map[String, String] = reason match {
      case Some(r) => Map(("reason", r))
      case None => Map()
    }

    // send the quit message
    socket.provide(SCSCPPi(Some("quit"), mp))

    // and close the connection
    socket.socket.close()
  }
}

object SCSCPClient {
  val default_port = 26133
}