package info.kwarc.mmt.imps

import info.kwarc.mmt.api.utils.UnparsedParsers
import scala.util.parsing.combinator.Parsers

object ParserWithSourcePosition extends Parsers with UnparsedParsers
{
  /**
    * This little ray of sunshine class allows the usage of Parser Combinators while also delivering
    * SourceRefs (or at least SourcePositions), which previously required state.
    *
    * @param p "Innocent" parser that just does the parsing, without SourceRefs.
    * @tparam T The type of the def-form, needs to implement trait DefForm.
    */
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
      val before = (in.offset, posb4.line, posb4.column)

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

  lazy val parseText = regex("""[^\r\n]+""".r)

  lazy val parseLineComment: PackratParser[LineComment] = {
    (";" ~> parseText) ^^ { case txt => LineComment(txt.dropWhile(_ == ';').trim, None, None) }
  }

  def pLineComment : ParserWithSourcePosition[LineComment] = {
    new ParserWithSourcePosition[LineComment](parseLineComment)
  }

  /* Super useful combinators for parsing everything awesomely! */
  /* I <3 Parser Combinators */

  def fullParser[T <: DefForm](p : Parser[T]) : Parser[T] = { withComment(new ParserWithSourcePosition[T](p)) }

  def withComment[T <: DefForm](dfp : ParserWithSourcePosition[T]) : Parser[T] = {
    (dfp ~ (pLineComment?)) ^^ { case r ~ c => r.addComment(c) ; r }
  }

  /* Positional Arguments must all appear in exactly the order given */
  def positional(parsers : List[Parser[DefForm]]) : Parser[List[DefForm]] = {
    if (parsers.isEmpty) { success(List.empty) } else
      { (fullParser(parsers.head) ~ positional(parsers.tail)) ^^ { case p ~ ps => List(p) ::: ps } }
  }

  def anyOf[A](ks : List[Parser[A]]) : Parser[A] = {
    if (ks.isEmpty) { failure("none of anyOf") } else { ks.head | anyOf(ks.tail) }
  }


  def keyworded(ks : List[(Parser[DefForm],Required)]) : Parser[List[Option[DefForm]]] =
  {
    def fill(l : List[(Option[DefForm], Int)]) : List[(Option[DefForm], Int)] = {
      var lp = l
      for (ki <- ks.indices){ if (!lp.map(_._2).contains(ki)) {lp = lp ::: List((None, ki))} } ; lp
    }

    val ks0 = ks.map(p => fullParser(p._1))
    val ks1 = ks0.map(r => r.map(d => (Some(d),ks0.indexOf(r))))

    rep(anyOf(ks1)).map(l => fill(l).sortWith((x,y) => x._2 < y._2).map(_._1))
  }

  /**
    * Parser Combinator that composes a parser for a large def-form from parsers for its parts.
    *
    * Note: This is not a fullparser, because while we _do_ want SourceRefs, we also want for
    *       potentially following comments to stand on their own.
    *
    * @param name The name of the Def-Form, e.g. "def-atomic-sort".
    * @param pos Parsers for all positional arguments (all required and in this order).
    * @param mod Parsers for all modifier arguments (all optional, any order).
    *            These are essentially treated exactly like keyword arguments.
    * @param key Parsers for all keyword arguments (might be optional, can come in any order) and
    *            value indicating if they're optional or required.
    * @param x Companion object containing the applicable build() method.
    * @tparam T The type of the def-form, needs to implement trait DefForm.
    * @return A parser that parses the entire def-form.
    */
  def composeParser[T <: DefForm](name : String,
                                   pos : List[Parser[DefForm]],
                                   mod : List[Parser[DefForm]],
                                   key : List[(Parser[DefForm], Required)],
                                     x : Comp[T]) : Parser[T] = {
    new ParserWithSourcePosition((("(" + name) ~> positional(pos) ~ keyworded(mod.map(k => (k,O)) ::: key) <~ ")") ^^
      { case p ~ k => for (ky <- key.indices) { if (key(ky)._2) { assert(k(ky).isDefined) } }
                      x.build(p ::: k) })
  }
}