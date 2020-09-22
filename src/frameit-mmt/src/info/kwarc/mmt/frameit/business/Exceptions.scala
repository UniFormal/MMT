package info.kwarc.mmt.frameit.business

final case class InvalidMetaData(private val message: String = "",
                                 private val cause: Throwable = None.orNull)
  extends Exception(message, cause)

final case class InvalidFactConstant(private val message: String = "",
                                     private val cause: Throwable = None.orNull)
  extends Exception(message, cause)

final case class InvalidScroll(private val message: String = "",
                               private val cause: Throwable = None.orNull)
  extends Exception(message, cause)