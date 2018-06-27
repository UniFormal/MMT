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
    if (parsers.isEmpty) { success(List.empty) } else
      { (fullParser(parsers.head) ~ positional(parsers.tail)) ^^ { case p ~ ps => List(p) ::: ps } }
  }

  def anyOf[A](ks : List[Parser[A]]) : Parser[A] = {
    if (ks.isEmpty) { failure("none of anyOf") } else { ks.head | anyOf(ks.tail) }
  }

  /* Keyword Arguments are all optional and can appear in any order */
  def keyworded(ks : List[Parser[DefForm]]) : Parser[List[DefForm]] =
  {
    /* General idea: transform all parsers in list from Parsers for X to Parsers for (X,i) where i is
     * the index in the original list, then return arrangement sorted via index. */

    val ks0 = ks.map(p => fullParser(p))
    val ks1 = ks0.map(r => r.map(d => (d,ks0.indexOf(r))))
    rep(anyOf(ks1)).map(l => l.sortWith((x,y) => x._2 < y._2).map(_._1))
  }

  def composeParser[T <: DefForm](name : String, pos : List[Parser[DefForm]], key : List[Parser[DefForm]], x : Comp[T]) : Parser[T] = {
    fullParser((("(" + name) ~> positional(pos) ~ keyworded(key) <~ ")") ^^ { case p ~ k => x.build(p ::: k) })
  }

  val parseHeralding : Parser[Heralding] = composeParser("herald", List(parseTName), List.empty, Heralding)
}