package info.kwarc.mmt.api.utils

/**
  * Utilities to produce nice-looking Unicode strings, e.g. sub- or superscripted integers.
  */
@deprecated("only used in diagrams, should probably be moved/deleted")
object UnicodeStrings {
  private val digitSubscripts = List("₀", "₁", "₂", "₃", "₄", "₅", "₆", "₇", "₈", "₉")
  private val digitSuperscripts = List("⁰", "¹", "²", "³", "⁴", "⁵", "⁶", "⁷", "⁸", "⁹")

  /**
    * Stringifies an integer with the provided digits.
    *
    * @param digits A 10-element list of strings representing digits 0 - 9.
    */
  private def translateInteger(digits: List[String])(i: Int): String = {
    if (i < 0) {
      "₋" + translateInteger(digits)(-1 * i)
    } else {
      val prefix = if (i >= 10) translateInteger(digits)(i / 10) else ""
      prefix + digits(i % 10)
    }
  }

  def subscriptInteger(i: Int): String = translateInteger(digitSubscripts)(i)

  def superscriptInteger(i: Int): String = translateInteger(digitSuperscripts)(i)
}
