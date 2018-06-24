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
      val source = in.source
      val offset = in.offset
      val start = handleWhiteSpace(source, offset)
      val inwo = in.drop(start - offset)

      val posb4 = in.pos
      val before = (offset, posb4.line, posb4.column)

      p(inwo) match {
        case Success(t, in1) =>
        {
          val ip = inwo.pos
          val after = (in1.offset, ip.line, ip.column)
          val u = updateSourceInfo(t, Some(Left((before, after))))
          Success(u.asInstanceOf[T], in1)
        }
        case ns: NoSuccess => ns
      }
    }
  }

  def updateSourceInfo(d : DefForm, sourceInfo : SourceInfo) : DefForm =
  {
    d match {
      case LineComment(s,_) => LineComment(s, sourceInfo)
      case Heralding(s,_,c) => Heralding(s,sourceInfo,c)
      case _ => ???
    }
  }

  lazy val parseImpsSource : PackratParser[List[DefForm]] = { rep1(parseDefForms) }

  lazy val parseDefForms : PackratParser[DefForm] = { parseLineComment | parseHeralding }

  lazy val parseName = { regex("""[^()\t\r\n ]+""".r) }
  lazy val parseText = { regex("""[^\r\n]+""".r) }

  lazy val pLineComment: PackratParser[LineComment] = {
    (";" ~> parseText) ^^ { case txt => LineComment(txt.dropWhile(_ == ';').trim, None) }
  }

  lazy val pHeralding: PackratParser[Heralding] = {
    (("(herald " ~> parseName <~ ")") ~ (parseLineComment?)) ^^ { case name ~ c => Heralding(name, None, c) }
  }

  def parseLineComment : DefFormParser[LineComment] = { new DefFormParser[LineComment](pLineComment) }
  def parseHeralding   : DefFormParser[Heralding]   = { new DefFormParser[Heralding](pHeralding) }
}