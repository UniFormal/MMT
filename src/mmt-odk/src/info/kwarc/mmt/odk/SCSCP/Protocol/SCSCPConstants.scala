package info.kwarc.mmt.odk.SCSCP.Protocol

/** All attributes used in the SCSCP Protocol */
object SCSCPAttributes {
  final val SERVICE_NAME: String = "service_name"
  final val SERVICE_VERSION: String = "service_version"
  final val SERVICE_ID: String = "service_id"
  final val SCSCP_VERSIONS: String = "scscp_versions"
  final val VERSION: String = "version"
  final val INFO: String = "info"
  final val REASON: String = "reason"
  final val CALL_ID : String = "call_id"
}

/** Keys for all messages in the SCSCP Protocol */
object SCSCPMessageKeys {
  final val QUIT : String = "quit"
  final val TERMINATE : String = "terminate"
}

/** All constants related to SCSCP */
object SCSCPConstants {
  final val QUIT_MESSAGE_KEY: String = "quit"

  /** Versions supported by the SCSCP Client */
  final val VERSIONS: List[String] = "1.3" :: Nil
}
