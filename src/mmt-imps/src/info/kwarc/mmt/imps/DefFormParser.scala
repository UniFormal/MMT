package info.kwarc.mmt.imps

import info.kwarc.mmt.api.utils.UnparsedParsers

import scala.util.parsing.combinator.Parsers

object DefFormParser extends Parsers with UnparsedParsers
{
  class DefFormParser[T <: DefForm](p : Parser[T]) extends Parser[T]
  {
    // Source of a relevant (yet changed) code snippet:
    // stackoverflow.com/questions/14707127/accessing-position-information-in-a-scala-combinatorparser-kills-performance

    override def apply(in: Input): ParseResult[T] =
    {
      println("call to apply")

      val source = in.source
      val offset = in.offset
      val start = handleWhiteSpace(source, offset)
      val inwo = in.drop(start - offset)

      val posb4 = in.pos
      val before = (offset, posb4.line, posb4.column)

      p(inwo) match {
        case Success(t, in1) => {
          val ip = inwo.pos
          val after = (in1.offset, ip.line, ip.column)
          val sourceInfo: SourceInfo = Some(Left((before, after)))

          println("~~~~~~~~~ apply success: " + before + " " + after)
          println("~~~~~~~~~ " + t.toString + "\n")

          t.src = sourceInfo
          assert(t.src == sourceInfo)

          Success(t, in1)
        }
        case ns: NoSuccess => ns
      }
    }
  }

  lazy val parseImpsSource : PackratParser[List[DefForm]] = { rep1(parseDefForms) }

  lazy val parseDefForms : PackratParser[DefForm] = { parseHeraldingNew }

  lazy val parseName = { regex("""[^()\t\r\n ]+""".r) }

  lazy val parseHeraldingOld: PackratParser[Heralding] = {
    ("(herald " ~> parseName <~ ")") ^^ { case name => Heralding(name, None) }
  }

  def parseHeraldingNew : DefFormParser[Heralding] = { new DefFormParser[Heralding](parseHeraldingOld) }
}