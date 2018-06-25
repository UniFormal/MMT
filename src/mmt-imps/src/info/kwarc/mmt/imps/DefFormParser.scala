package info.kwarc.mmt.imps

import info.kwarc.mmt.api.utils.UnparsedParsers

import scala.util.parsing.combinator.Parsers

/* TODO: Refactor into "PositionParser" or something more accurate. */
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
          t.addSource(before,after)
          Success(t.asInstanceOf[T], in1)
        }
        case ns: NoSuccess => ns
      }
    }
  }

  lazy val parseImpsSource : PackratParser[List[DefForm]] = { rep1(parseDefForm) }

  lazy val parseDefForm : PackratParser[DefForm] = { parseLineComment | parseHeralding }

  lazy val parseName = { regex("""[^()\t\r\n ]+""".r) }
  lazy val parseText = { regex("""[^\r\n]+""".r) }

  lazy val pLineComment: PackratParser[LineComment] = {
    (";" ~> parseText) ^^ { case txt => LineComment(txt.dropWhile(_ == ';').trim, None, None) }
  }

  def parseLineComment : DefFormParser[LineComment] = { new DefFormParser[LineComment](pLineComment) }

  /* I <3 Parser Combinators */
  def withComment[T <: DefForm](dfp : DefFormParser[T]) : Parser[T] = {
    (dfp ~ (parseLineComment?)) ^^ { case r ~ c => r.addComment(c) ; r }
  }

  def fullParser[T <: DefForm](p : Parser[T]) : Parser[T] = { withComment(new DefFormParser[T](p)) }

  /* Positional Arguments must all appear in exactly the order given */
  def positional(ps : List[Parser[DefForm]]) : Parser[List[DefForm]] = {
    (fullParser(ps.head) ~ positional(ps.tail)) ^^ { case p ~ ps => List(p) ::: ps }
  }

  /* Keyword Arguments are all optional and can appear in any order */
  def keyworded(ks : List[Parser[DefForm]]) : Parser[List[DefForm]] = {
    /* Too tired now, but idea: transform all parsers in list from Parsers for X to Parsers for (X,i) where i is
     * the index in the original list, then return arrangement sorted via index. Should be doable? */
    ???
  }

  lazy val pHeralding: PackratParser[Heralding] = {
    ("(herald " ~> parseName <~ ")") ^^ { case name => Heralding(name, None, None) }
  }

  def parseHeralding : Parser[Heralding] = fullParser(pHeralding)
}