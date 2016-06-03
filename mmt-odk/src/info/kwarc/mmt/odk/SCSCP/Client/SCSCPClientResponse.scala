package info.kwarc.mmt.odk.SCSCP.Client

import info.kwarc.mmt.odk.SCSCP.SCSCPResult

/**
  * Created by twiesing on 02/06/16.
  */
abstract class SCSCPClientResponse(val client : SCSCPClient) {
  def interrupt(): Unit

  def get() : SCSCPResult
}
