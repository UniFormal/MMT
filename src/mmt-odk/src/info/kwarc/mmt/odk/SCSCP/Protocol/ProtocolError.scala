package info.kwarc.mmt.odk.SCSCP.Protocol

/**
  * Common class for all exceptions based on SCSCP
  *
  * @param message Message of the error
  */
class ProtocolError(message: String) extends Exception("SCSCP Protocol Error: " + message)

class UnsupportedProtocolVersion extends ProtocolError("Unsupported Protocol Version")

class ClientVersionExpected extends ProtocolError("Client did not select an SCSCP version")

class VersionEchoExpected extends ProtocolError("Server did not echo back the SCSCP version")

class UnknownProcessingInstruction extends ProtocolError("Unknown Processing Instruction")

case class OpenMathError(s : String) extends ProtocolError("Unable to parse OpenMath: " + s)