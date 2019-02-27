package info.kwarc.mmt.api.utils

/**
  * The MMT_TODO class is an alternative to the {{@deprecated}} annotation.
  * This allows us to mark a part of the code as needing review and/or re-work
  * @param message
  */
class MMT_TODO(message: String) extends scala.deprecated(message, since="forever")
// class MMT_TODO(message: String) extends scala.annotation.StaticAnnotation