package info.kwarc.mmt.odk.SCSCP.CD

import info.kwarc.mmt.odk.OpenMath.OMSymbol


object scscp1 {

  /* The name of the cd itself */
  def apply(name : String) = {
    OMSymbol(name, "scscp1", None, None)
  }

  /* Main Messages */
  val procedureCall = "procedure_call"
  val procedureCompleted = "procedure_completed"
  val procedureTerminated = "procedure_terminated"

  /* Call and Response Identifiers */
  val callId = "call_id"

  /* Options in procedure calls */
  val optionMaxMemory = "option_max_memory"
  val optionMinMemory = "option_min_memory"
  val optionRuntime = "option_runtime"
  val optionDebugLevel = "option_debuglevel"
  val optionReturnCookie = "option_return_cookie"
  val optionReturnObject = "option_return_object"
  val optionReturnNothing = "option_return_nothing"

  /* Information attributes */
  val infoMemory = "info_memory"
  val infoRuntime = "info_runtime"
  val infoMessage = "info_message"

  /* Standard errors */
  val errorMemory = "error_memory"
  val errorRuntime = "error_runtime"
  val errorSystemSpecific = "error_system_specific"
}