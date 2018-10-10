package info.kwarc.mmt.odk.SCSCP.Server

import info.kwarc.mmt.odk.OpenMath.{OMExpression, OMObject, OMSymbol}
import info.kwarc.mmt.odk.SCSCP.Protocol.{SCSCPCall, SCSCPResult}

sealed abstract class SCSCPServerEvent(server: SCSCPServer, client: Option[SCSCPServerClient]) {
  override def toString: String = client.map("client" + _.identifier + ": ").getOrElse("") + message
  def message: String
}

case class SCSCPRegisteredHandler(symbol: OMSymbol, server: SCSCPServer) extends SCSCPServerEvent(server, None) {
  def message: String = "Registered handler for " + symbol
}

case class SCSCPUnregistered(symbol: OMSymbol, server: SCSCPServer) extends SCSCPServerEvent(server, None) {
  def message: String = "Removed handler for " + symbol
}

case class SCSCPAddedClient(client: SCSCPServerClient, server: SCSCPServer) extends SCSCPServerEvent(server, Some(client)) {
  def message: String = "Added new client"
}

case class SCSCPRemovedClient(client: SCSCPServerClient, server: SCSCPServer) extends SCSCPServerEvent(server, Some(client)) {
  def message: String = "Client removed"
}

case class SCSCPQuittingServer(reason: Option[String], server: SCSCPServer) extends SCSCPServerEvent(server, None) {
  def message: String = "Server quitting: " + reason
}

sealed abstract class SCSCPServerClientEvent(client: SCSCPServerClient) extends SCSCPServerEvent(client.server, Some(client))

case class SCSCPUnsupportedVersion(version: String, client: SCSCPServerClient) extends SCSCPServerClientEvent(client) {
  def message: String = "Got unsupported version: " + version
}

case class SCSCPNegotiatedVersion(version: String, client: SCSCPServerClient) extends SCSCPServerClientEvent(client) {
  def message: String = "Negotiated version: " + version
}

case class SCSCPQuittingClient(reason: Option[String], client: SCSCPServerClient) extends SCSCPServerClientEvent(client) {
  def message: String = "Sending quit to client: " + reason
}

case class SCSCPHandlingInfo(info: String, client: SCSCPServerClient) extends SCSCPServerClientEvent(client) {
  def message: String = "Got client INFO: " + info
}

case class SCSCPHandlingQuit(reason: Option[String], client: SCSCPServerClient) extends SCSCPServerClientEvent(client) {
  def message: String = "Got client QUIT: " + reason
}

case class SCSCPHandlingTerminate(client: SCSCPServerClient) extends SCSCPServerClientEvent(client) {
  def message: String = "Got client TERMINATE"
}

case class SCSCPUnknownInstruction(client: SCSCPServerClient) extends SCSCPServerClientEvent(client) {
  def message: String = "Got unknown instruction"
}

case class SCSCPProcedureCall(call: OMObject, client: SCSCPServerClient) extends SCSCPServerClientEvent(client) {
  def message: String = "handling procedure call: " + call
}

case class SCSCPCallingHandler(call: SCSCPCall, handler: SCSCPHandler, client: SCSCPServerClient) extends SCSCPServerClientEvent(client) {
  def message: String = "calling handler " + handler
}

case class SCSCPHandlerReturned(call: SCSCPCall, result: OMExpression, client: SCSCPServerClient) extends SCSCPServerClientEvent(client) {
  def message: String = s"handler returned result: $result"
}

case class SCSCPSignatureMismatch(call: SCSCPCall, mismatch: SignatureMismatchException, client: SCSCPServerClient) extends SCSCPServerClientEvent(client) {
  def message: String = s"SignatureMismatch occurred: " + mismatch
}

case class SCSCPCallException(call: SCSCPCall, exception: Exception, client: SCSCPServerClient) extends SCSCPServerClientEvent(client) {
  def message: String = s"Exception occurred: " + exception
}

case class SCSCPWriteResult(call: SCSCPCall, result: SCSCPResult, client: SCSCPServerClient) extends SCSCPServerClientEvent(client) {
  def message: String = s"Writing call result: " + result
}