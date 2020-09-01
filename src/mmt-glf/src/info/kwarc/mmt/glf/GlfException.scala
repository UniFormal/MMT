package info.kwarc.mmt.glf

class GlfException(private val message: String = "",
                   private val cause: Throwable = None.orNull)
  extends Exception(message, cause)


final case class LangTheoryIncomplete(private val message: String = "",
                                private val cause: Throwable = None.orNull)
  extends GlfException(message, cause)
