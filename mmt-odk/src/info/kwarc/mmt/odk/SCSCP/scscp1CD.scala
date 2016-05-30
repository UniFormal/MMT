package info.kwarc.mmt.odk.SCSCP


object scscp1CD {
  /* Main Messages */
  val procedure_call = "procedure_call"
  val procedure_completed = "procedure_completed"
  val procedure_terminated = "procedure_terminated"

  /* Call and Response Identifiers */
  val call_id = "call_id"

  /* Options in procedure calls */
  val option_max_memory = "option_max_memory"
  val option_min_memory = "option_min_memory"
  val option_runtime = "option_runtime"
  val option_debuglevel = "option_debuglevel"
  val option_return_cookie = "option_return_cookie"
  val option_return_object = "option_return_object"
  val option_return_nothing = "option_return_nothing"

  /* Information attributes */
  val info_memory = "info_memory"
  val info_runtime = "info_runtime"
  val info_message = "info_message"

  /* Standard errors */
  val error_memory = "error_memory"
  val error_runtime = "error_runtime"
  val error_system_specific = "error_system_specific"
}