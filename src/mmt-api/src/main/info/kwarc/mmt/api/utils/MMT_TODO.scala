package info.kwarc.mmt.api.utils

/**
  * The MMT_TODO class is an alternative to the {{@deprecated}} annotation.
  * This allows us to mark a part of the code as needing review and/or re-work
  * @param message
  */
@deprecated("Use @deprecated(s\"MMT_TODO: $message\", since=\"forever\" instead)", since="2022-08-28")
final class MMT_TODO(message: String) extends scala.deprecated("MMT_TODO: " + message, since="forever")