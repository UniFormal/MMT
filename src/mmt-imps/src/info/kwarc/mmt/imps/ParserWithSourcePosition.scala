package info.kwarc.mmt.imps

import info.kwarc.mmt.api.utils.UnparsedParsers

import scala.util.parsing.combinator.Parsers

object ParserWithSourcePosition extends Parsers with UnparsedParsers
{
  class ParserWithSourcePosition[T <: DefForm](p : Parser[T]) extends Parser[T]
  {
    // Source of a relevant (yet changed) code snippet:
    // stackoverflow.com/questions/14707127/accessing-position-information-in-a-scala-combinatorparser-kills-performance

    override def apply(in: Input): ParseResult[T] =
    {
      val source = in.source
      val offset = in.offset
      val start  = handleWhiteSpace(source, offset)
      val inwo   = in.drop(start - offset)

      val posb4  = in.pos
      val before = (offset, posb4.line, posb4.column)

      p(inwo) match
      {
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

  lazy val parseName = regex("""[^()\t\r\n ]+""".r)
  lazy val parseText = regex("""[^\r\n]+""".r)

  lazy val parseTName : Parser[Name] = fullParser(parseName ^^ { case nm => Name(nm,None,None)} )

  lazy val pLineComment: PackratParser[LineComment] = {
    (";" ~> parseText) ^^ { case txt => LineComment(txt.dropWhile(_ == ';').trim, None, None) }
  }

  def parseLineComment : ParserWithSourcePosition[LineComment] = { new ParserWithSourcePosition[LineComment](pLineComment) }

  /* I <3 Parser Combinators */
  def withComment[T <: DefForm](dfp : ParserWithSourcePosition[T]) : Parser[T] = {
    (dfp ~ (parseLineComment?)) ^^ { case r ~ c => r.addComment(c) ; r }
  }

  def fullParser[T <: DefForm](p : Parser[T]) : Parser[T] = { withComment(new ParserWithSourcePosition[T](p)) }

  /* Positional Arguments must all appear in exactly the order given */
  def positional(parsers : List[Parser[DefForm]]) : Parser[List[DefForm]] = {
    println("positional: " + parsers.length)
    if (parsers.isEmpty) { success(List.empty) } else
      { (fullParser(parsers.head) ~ positional(parsers.tail)) ^^ { case p ~ ps => List(p) ::: ps } }
  }

  /* Keyword Arguments are all optional and can appear in any order */
  def keyworded(ks : List[Parser[DefForm]]) : Parser[List[DefForm]] = {
    /* Too tired now, but idea: transform all parsers in list from Parsers for X to Parsers for (X,i) where i is
     * the index in the original list, then return arrangement sorted via index. Should be doable? */
    ???
  }

  def composeParser[T <: DefForm](name : String, pos : List[Parser[DefForm]], x : Comp[T]) : Parser[T] =
  {
    val c = implicitly[Baz].snafu
    (("(" + name) ~> positional(pos) <~ ")") ^^ { case p => x.build(p) }
  }

  val parseHeralding : Parser[Heralding] = composeParser("herald", List(parseTName), Heralding)
}