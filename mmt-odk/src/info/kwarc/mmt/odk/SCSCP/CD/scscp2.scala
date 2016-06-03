package info.kwarc.mmt.odk.SCSCP.CD

import info.kwarc.mmt.odk.OpenMath.OMSymbol

object scscp2 {

  /* The name of the cd itself */
  def apply(name : String) = {
    OMSymbol(name, "scscp2", None, None)
  }

  /* Procedures for work with remote objects */
  val storeSession = "store_session"
  val storePersistent = "store_persistent"
  val retrieve = "retrieve"
  val unbind = "unbind"

  /* Special procedures to obtain meta-information about SCSCP service */
  val getAllowedHeads = "get_allowed_heads"
  val isAllowedHead = "is_allowed_head"
  val getTransientCD = "get_transient_cd"
  val getSignature = "get_signature"
  val getServiceDescription = "get_service_description"

  /* Special symbols */
  val signature = "signature"
  val serviceDescription = "service_description"
  val symbolSet = "symbol_set"
  val symbolSetAll = "symbol_set_all"
  val noSuchTransientCD = "no_such_transient_cd"
}