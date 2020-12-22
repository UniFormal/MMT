package info.kwarc.mmt.frameit.business

/**
  * Exception for invalid meta data of facts or scrolls.
  * It can be wrapped in an [[InvalidFactConstant]] or [[InvalidScroll]] exception.
  */
final case class InvalidMetaData(private val message: String = "",
                                 private val cause: Throwable = None.orNull)
  extends Exception(message, cause)

/**
  * Exception for MMT constants representing unknown/invalid facts
  * or putative facts sent by the game engine that are unknown/invalid.
  */
final case class InvalidFactConstant(private val message: String = "",
                                     private val cause: Throwable = None.orNull)
  extends Exception(message, cause)

/**
  * Exception for MMT problem and solution theory pairs representing unknown/invalid scrolls,
  * e.g. by having invalid meta data.
  */
final case class InvalidScroll(private val message: String = "",
                               private val cause: Throwable = None.orNull)
  extends Exception(message, cause)