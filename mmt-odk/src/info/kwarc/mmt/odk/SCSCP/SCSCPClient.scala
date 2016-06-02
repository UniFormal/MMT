package info.kwarc.mmt.odk.SCSCP

import java.net.Socket


/**
  * Implement an Symbolic Computation Software Composability Protocol Client
  * @param host Hostname of server to connect to
  * @param port Port number to connect to. Defaults to 26133.
  * @param versions Version(s) to be used by this SCSCP client. Will be prioritised in order.
  */
class SCSCPClient(host : String, port : Int = 26133, versions : List[String] = "1.3" :: Nil) {

  /**
    * stores if the connection has been initialised
    */
  private var initialised = false

  /**
    * The underlying SCSCPSocket used by this SCSCPClient.
    */
  var socket : SCSCPSocket = new SCSCPSocket(new Socket(host, port))

  /**
    * Checks if this SCSCPClient has initialised
 *
    * @return
    */
  def isInitialised : Boolean = initialised

  /**
    * Initialises the connection to the SCSCP server
    * @return a boolean indicating if version negotiation was ok
    */
  def init : Boolean = {

    // we may only init once
    if(initialised){
      throw new Exception("Already initialised, can not initialise again!")
    }

    // block any other calls
    initialised = true

    // read the welcome message
    val vpi = socket.read_pi

    // extract the basics
    scscp_service_name = vpi("service_name")
    scscp_service_version = vpi("service_version")
    scscp_service_id = vpi("service_id")

    // and check if we support the right version
    val s_versions = vpi("scscp_versions").split(" ")

    versions.filter(s_versions.contains(_)) match {
      case Nil =>
        quit(Some("version not supported"))
        false
      case (v:String)::_ =>
        // store the version locally
        scscp_server_version = v
        val version_pi = SCSCPPi(Map(("version", v)))

        // send the version and receive and expect it back
        socket(version_pi)

        // read the response
        val resp = socket.read_pi

        // if the version is correct, we are done
        if (resp == version_pi){
          true
        // else the server quit
        } else if(resp.key.contains("quit")){
          false
        } else {
          false
        }
    }
  }

  /**
    * Quits the session with the SCSCP server
    * @param reason Reason for quitting. Optional.
    */
  def quit(reason : Option[String] = None) : Unit = {
    // build the reson
    val mp : Map[String, String] = reason match {
      case Some(r) => Map(("reason", r))
      case None => Map()
    }

    // send the quit message
    socket(SCSCPPi(Some("quit"), mp))

    // and close the connection
    socket.socket.close()
  }


  /**
    * stores the scscp service name
    */
  private var scscp_service_name : String = null

  /**
    * Returns the service name of the server
    * @return
    */
  def service_name = scscp_service_name

  /**
    * stores the scscp service version
    */
  private var scscp_service_version : String = null

  /**
    * Returns the service version of the server
    * @return
    */
  def service_version = scscp_service_version

  /**
    * stores the scscp service id
    */
  private var scscp_service_id : String = null

  /**
    * Returns the service id assigned by the server
    * @return
    */
  def service_id = scscp_service_id

  /**
    * stores the scscp version used by the server
    */
  private var scscp_server_version : String = null

  /**
    * Returns the scscp version used by this client
    * @return
    */
  def scscp_version = scscp_server_version
}
