package info.kwarc.mmt.api.utils

object Escape {
   /**
    * encode all occurrences of certain characters in a string
    */
   def apply(s: String, escapes: (Char,String)*) : String = {
      var in = s
      var out = ""
      // invariant: s = out + in
      while (in != "") {
         val c = in(0)
         val e = escapes.find(_._1 == c).map(_._2).getOrElse(c.toString)
         in = in.substring(1)
         out = out + e
      }
      out
   }
}

/** escapes all non-legal characters according to some rules */
abstract class Escaping {
  /** the escape character */
  val escapeChar: Char
  /** characters that are not escaped
   *  default: ASCII 32-126 without the plain/custom escapes; override as needed
   */
  def legal(c: Char) = {
    c.toInt >= 32 && c.toInt <= 126 && !escapedChars.contains(c)
  }
  /** characters that are escaped by escape-char + themselves
   *  default: only escapeChar; override as needed
   */
  def usePlainEscape: List[Char] = List(escapeChar)
  private lazy val usePlainEscapeVal = usePlainEscape
  /** characters that are escaped by a custom string
   *  default: n/t/s for newline/tab/space; override as needed
   */
  def useCustomEscape: List[(Char,String)] = List('\n' -> "n", '\t' -> "t")
  private lazy val useCustomEscapeVal = useCustomEscape

  /** the escape used for characters outside the legal range that are not otherwise escaped */
  def defaultEscape(c: Char) = c.toInt.formatted("%4h").replace(" ", "0")
  /** pulls an escaped character from the beginning of a string and returns that char and the number of characters consumed */
  def defaultUnescape(s: String) : Option[(Char,Int)] = {
    try {
      val hex = s.substring(0,4)
      val char = Integer.parseInt(hex,16).toChar
      Some((char,5))
    } catch {
      case _: Exception => None
    }
  }

  /** characters that are escaped because of an escape rule */
  protected lazy val escapedChars = usePlainEscapeVal ::: useCustomEscapeVal.map(_._1)

  /** check invariant that guarantees invertibility of escaping (not guaranteed if the default functions are overridden) */
  def check: Unit = {
    useCustomEscapeVal foreach {case (c,s) =>
      if (s == "")
        throw Error("empty custom escape")
      if (usePlainEscapeVal contains s(0))
        throw Error("clash between encoding of " + c + " and " + s(0))
      if (s.length >= 4 && s.substring(0,4).toLowerCase.forall(c => c.isDigit || "abcdef".contains(c)))
        throw Error("clash between encoding of " + c + " and arbitrary characters")
      useCustomEscapeVal.find(a => a._1 != c && a._2.startsWith(s)) foreach {a =>
        throw Error(s"clash between encoding of '$c' and '${a._1}'")
      }
    }
  }

  def apply(c: Char): String = {
     if (legal(c)) c.toString
     else {
       val e = if (usePlainEscapeVal contains c)
         c.toString
       else
         useCustomEscapeVal.find(_._1 == c).map(_._2).getOrElse {defaultEscape(c)}
       escapeChar + e
     }
  }
  def apply(s: String): String = s flatMap {c => apply(c)}

  case class Error(msg: String) extends Exception(msg)

  def unapply(s: String): String = {
      var escaped = s
      var unescaped = ""
      while (escaped.nonEmpty) {
        val first = escaped(0)
        val rest = escaped.substring(1)
        // the next character to produce, and the number of characters that were consumed
        val (next, length): (Char,Int) = if (first != escapeChar) {
          (first,1)
        } else {
            useCustomEscapeVal.find(cs => rest.startsWith(cs._2)) match {
              case Some((c,s)) => (c,1+s.length)
              case None =>
                 val second = rest(0)
                if (usePlainEscapeVal contains second)
                  (second,2)
                else {
                  defaultUnescape(rest).getOrElse {
                    throw Error("illegal escape: " + rest)
                  }
                }
            }
        }
        unescaped += next
        escaped = escaped.substring(length)
      }
      unescaped
  }
}

object StandardStringEscaping extends Escaping {
  val escapeChar = '\\'
  override def usePlainEscape = super.usePlainEscape ::: List('"')
}

/** escapes a string into one that is a legal file name on Windows and Unix */
object FileNameEscaping extends Escaping {
  val escapeChar = '$'

  override def usePlainEscape = Range(0,26).toList.map(i => (65+i).toChar)
  override def useCustomEscape = super.useCustomEscape ::: List(
      ' '  -> "sp", '/' -> "sl", '?' -> "qm", '<' -> "le", '>' -> "gr", '\\' -> "bs",
      ':' -> "co", '*' -> "st", '|' -> "pi", '"' -> "qu", '^' -> "ca",
      '\'' -> "pr" // not sure why this is escaped; it's legal in file names but MMT archives used to escape it anyway
  )
}

/** escapes a string using the &-escapes of XML for <>&" */
object XMLEscaping extends Escaping {
  val escapeChar = '&'
  override def usePlainEscape = Nil
  override val useCustomEscape = List(
    '>' -> "gt", '<' -> "lt", '"' -> "quot", '&' -> "amp"//, '\'' -> "#39;"
  )

  override def apply(c: Char): String = {
    if (legal(c)) c.toString
    else {
        val e = useCustomEscape.find(_._1 == c).map(_._2).getOrElse {
          "#" + c.toInt.toString
        }
      escapeChar + e + ";"
    }
  }
  override def unapply(s: String): String = {
    var escaped = s
    var unescaped = ""
    while (escaped.nonEmpty) {
      val first = escaped(0)
      val rest = escaped.substring(1)
      // the next character to produce, and the number of characters that were consumed
      val (next, length): (Char,Int) = if (first != escapeChar) {
        (first,1)
      } else {
        useCustomEscape.find(cs => rest.startsWith(cs._2 + ";")) match {
          case Some((c,s)) => (c,2+s.length)
          case None =>
              val ind = if (rest.head == '#') 1 else 0
              val endind = rest.indexOf(';')
              try {
                val int = rest.substring(ind,endind)
                val char = Integer.parseInt(int).toChar
                (char, endind)
              } catch {case _: Exception =>
                throw Error("illegal escape: " + escaped)
              }
        }
      }
      unescaped += next
      escaped = escaped.substring(length)
    }
    unescaped
  }
  override def defaultEscape(c: Char) = c.toString
}

/** escapes a string using the %-escapes for URLs */
object URLEscaping extends Escaping {
  val escapeChar = '%'
  override def usePlainEscape = Nil
  override def useCustomEscape = List(
    ' ' -> "20"
  )
}
