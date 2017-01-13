package info.kwarc.mmt.odk.SCSCP.CD

import info.kwarc.mmt.odk.OpenMath.OMSymbol


object scscp1 {

  /* The name of the cd itself */
  def apply(name: String) = {
    OMSymbol(name, "scscp1", None, None)
  }

  /* Main Messages */
  final val procedureCall: String = "procedure_call"
  final val procedureCompleted: String = "procedure_completed"
  final val procedureTerminated = "procedure_terminated"

  /* Call and Response Identifiers */
  final val callId = "call_id"

  /* Options in procedure calls */
  final val optionMaxMemory = "option_max_memory"
  final val optionMinMemory = "option_min_memory"
  final val optionRuntime = "option_runtime"
  final val optionDebugLevel = "option_debuglevel"
  final val optionReturnCookie = "option_return_cookie"
  final val optionReturnObject = "option_return_object"
  final val optionReturnNothing = "option_return_nothing"

  /* Information attributes */
  final val infoMemory = "info_memory"
  final val infoRuntime = "info_runtime"
  final val infoMessage = "info_message"

  /* Standard errors */
  final val errorMemory = "error_memory"
  final val errorRuntime = "error_runtime"
  final val errorSystemSpecific = "error_system_specific"
}