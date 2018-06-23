package info.kwarc.mmt.imps

import info.kwarc.mmt.api.utils.UnparsedParsers

import scala.util.parsing.combinator.{PackratParsers, Parsers, RegexParsers}

object FooParser extends UnparsedParsers with Parsers
{
  class DefFormParser[T <: DefForm](p: Parser[T])
  {
    // Relevant snippet source:
    // stackoverflow.com/questions/14707127/accessing-position-information-in-a-scala-combinatorparser-kills-performance

    def ^^~(f: ((Int, Int), T) => T): Parser[DefForm] = Parser { in =>

      val source = in.source
      val offset = in.offset
      val start = handleWhiteSpace(source, offset)
      val inwo = in.drop(start - offset)

      p(inwo) match {
        case Success(t, in1) => {
          val ip = inwo.pos
          val a = ip.line
          val b = ip.column

          Success(f((a, b), t), in1)
        }
        case ns: NoSuccess => ns
      }
    }

    lazy val parseName = {
      regex("""[^\t\r\n ]+""".r)
    }

    lazy val parseHeraldingOld: PackratParser[Heralding] = {
      ("(herald " ~> parseName <~ ")") ^^ { case name => Heralding(name, None) }
    }
  }

}