package info.kwarc.mmt.gf

class GfImportException(private val message: String = "",
                        private val cause: Throwable = None.orNull)
  extends Exception(message, cause)


case class GfLexerException(private val message: String = "",
                            private val cause: Throwable = None.orNull)
  extends GfImportException(message, cause)

final case class GfEmptySyntaxException() extends GfImportException("The abstract syntax appears to be empty")
