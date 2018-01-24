package info.kwarc.mmt.odk.SCSCP.CD

import info.kwarc.mmt.odk.OpenMath.{OMApplication, OMExpression, OMObject, OMSymbol}

object scscp2 {

  /* The name of the cd itself */
  def apply(name : String) = {
    OMSymbol(name, "scscp2", None, None)
  }

  /* Procedures for work with remote objects */
  final val storeSession = "store_session"
  final val storePersistent = "store_persistent"
  final val retrieve = "retrieve"
  final val unbind = "unbind"

  /* Special procedures to obtain meta-information about SCSCP service */
  final val getAllowedHeads = "get_allowed_heads"
  final val isAllowedHead = "is_allowed_head"
  final val getTransientCD = "get_transient_cd"
  final val getSignature = "get_signature"
  final val getServiceDescription = "get_service_description"

  /* Special symbols */
  val signature = "signature"
  val serviceDescription = "service_description"
  val symbolSet = "symbol_set"

  val symbolSetAll = "symbol_set_all"
  val noSuchTransientCD = "no_such_transient_cd"
}

object SymbolSet {
  def apply(elements : List[OMExpression]) : OMApplication = {
    OMApplication(scscp2(scscp2.symbolSet), elements, None, None)
  }
  def unapply(oma : OMApplication) : Option[List[OMExpression]] =  oma match {
    case OMApplication(ss, elements, _, _) if ss == scscp2(scscp2.symbolSet) => Some(elements)
    case _ => None
  }
}
