package info.kwarc.mmt.gf

class GfImportException(private val message: String = "",
                        private val cause: Throwable = None.orNull)
  extends Exception(message, cause)


case class GfLexerException(private val message: String = "")
  extends GfImportException(message)


class GfParserException(private val message: String = "",
                             private val cause: Throwable = None.orNull)
  extends GfImportException(message, cause)

final case class GfEmptySyntaxException()
  extends GfParserException("The abstract syntax appears to be empty")

final case class GfUnexpectedEOF(private val message: String = "")
  extends GfParserException(message)

final case class GfUnexpectedTokenException(private val message: String = "")
  extends GfParserException(message)
