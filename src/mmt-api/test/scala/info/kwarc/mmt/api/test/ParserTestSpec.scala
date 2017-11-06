package info.kwarc.mmt.api.test

import org.scalatest.FlatSpec
import info.kwarc.mmt.api.utils.CompRegexParsers

class ParserTestSpec extends FlatSpec {


  //
  // STRING
  //

  "parsing strings" should "parse correctly iff the literal is there" in {
    assert(TestParser.parse(TestParser.StringParser, "").isEmpty)
    assert(TestParser.parse(TestParser.StringParser, "string").get === "string")
    assert(TestParser.parse(TestParser.StringParser, "string+").isEmpty)
    assert(TestParser.parse(TestParser.StringParser, "strip").isEmpty)

    assert(TestParser.parse(TestParser.FancyStringParser, "").isEmpty)
    assert(TestParser.parse(TestParser.FancyStringParser, "string").get === "string")
    assert(TestParser.parse(TestParser.FancyStringParser, "string+").isEmpty)
    assert(TestParser.parse(TestParser.FancyStringParser, "strip").isEmpty)

    assert(TestParser.parse(TestParser.PlusStringParser, "").isEmpty)
    assert(TestParser.parse(TestParser.PlusStringParser, "string").get === "string")
    assert(TestParser.parse(TestParser.PlusStringParser, "string+").isEmpty)
    assert(TestParser.parse(TestParser.PlusStringParser, "strip").isEmpty)
  }

  "default string completion" should "work with substrings" in {
    val stringCompletion = List("string".toList)
    assert(TestParser.complete(TestParser.StringParser, "").results === stringCompletion)
    assert(TestParser.complete(TestParser.StringParser, "s").results === stringCompletion)
    assert(TestParser.complete(TestParser.StringParser, "st").results === stringCompletion)
    assert(TestParser.complete(TestParser.StringParser, "str").results === stringCompletion)
    assert(TestParser.complete(TestParser.StringParser, "stri").results === stringCompletion)
    assert(TestParser.complete(TestParser.StringParser, "strin").results === stringCompletion)
    assert(TestParser.complete(TestParser.StringParser, "string").results === stringCompletion)
    assert(TestParser.complete(TestParser.StringParser, "string+").isEmpty)
  }

  "fancy string completion" should "append fancy completion results on substrings" in {
    val FancyStringCompletion = List("string".toList, "string fancy".toList)
    assert(TestParser.complete(TestParser.FancyStringParser, "").results === FancyStringCompletion)
    assert(TestParser.complete(TestParser.FancyStringParser, "s").results === FancyStringCompletion)
    assert(TestParser.complete(TestParser.FancyStringParser, "st").results === FancyStringCompletion)
    assert(TestParser.complete(TestParser.FancyStringParser, "str").results === FancyStringCompletion)
    assert(TestParser.complete(TestParser.FancyStringParser, "stri").results === FancyStringCompletion)
    assert(TestParser.complete(TestParser.FancyStringParser, "strin").results === FancyStringCompletion)
    assert(TestParser.complete(TestParser.FancyStringParser, "string").results === FancyStringCompletion)
    assert(TestParser.complete(TestParser.FancyStringParser, "string+").isEmpty)
  }

  "plus string completion" should "append + completion results on substrings" in {
    val PlusStringCompletion = "string".toList
    assert(TestParser.complete(TestParser.PlusStringParser, "").results === PlusStringCompletion :: List("+".toList))
    assert(TestParser.complete(TestParser.PlusStringParser, "s").results === PlusStringCompletion :: List("s+".toList))
    assert(TestParser.complete(TestParser.PlusStringParser, "st").results === PlusStringCompletion :: List("st+".toList))
    assert(TestParser.complete(TestParser.PlusStringParser, "str").results === PlusStringCompletion :: List("str+".toList))
    assert(TestParser.complete(TestParser.PlusStringParser, "stri").results === PlusStringCompletion :: List("stri+".toList))
    assert(TestParser.complete(TestParser.PlusStringParser, "string").results === PlusStringCompletion :: List("string+".toList))
    assert(TestParser.complete(TestParser.PlusStringParser, "string+").isEmpty)
  }

  //
  // REGEX
  //

  "regex" should "parse valid pattern m" in {
    assert(TestParser.parse(TestParser.RegexParser, "").isEmpty)
    assert(TestParser.parse(TestParser.RegexParser, "a").get === "a")
    assert(TestParser.parse(TestParser.RegexParser, "aa").get === "aa")
    assert(TestParser.parse(TestParser.RegexParser, "aaa").isEmpty)

    assert(TestParser.parse(TestParser.FancyRegexParser, "").isEmpty)
    assert(TestParser.parse(TestParser.FancyRegexParser, "a").get === "a")
    assert(TestParser.parse(TestParser.FancyRegexParser, "aa").get === "aa")
    assert(TestParser.parse(TestParser.FancyRegexParser, "aaa").isEmpty)

    assert(TestParser.parse(TestParser.FancyRegexParser, "").isEmpty)
    assert(TestParser.parse(TestParser.PlusRegexParser, "a").get === "a")
    assert(TestParser.parse(TestParser.PlusRegexParser, "aa").get === "aa")
    assert(TestParser.parse(TestParser.PlusRegexParser, "aaa").isEmpty)
  }

  "default regex completion" should "match if the expression matches" in {
    assert(TestParser.complete(TestParser.RegexParser, "").isEmpty)
    assert(TestParser.complete(TestParser.RegexParser, "a").results === List("a".toList))
    assert(TestParser.complete(TestParser.RegexParser, "aa").results === List("aa".toList))
    assert(TestParser.complete(TestParser.RegexParser, "aaa").isEmpty)
  }

  //
  // SEQUENTIAL PARSER
  //

  "sequential parser" should "parse completed terms only" in {
    assert(TestParser.parse(TestParser.hello_then_world, "").isEmpty)
    assert(TestParser.parse(TestParser.hello_then_world, "helo").isEmpty)
    assert(TestParser.parse(TestParser.hello_then_world, "helo halo").get === TestParser.~("helo", "halo"))
    assert(TestParser.parse(TestParser.hello_then_world, "helo helo").isEmpty)
    assert(TestParser.parse(TestParser.hello_then_world, "halo halo").isEmpty)
    assert(TestParser.parse(TestParser.hello_then_world, "halo helo").isEmpty)
    assert(TestParser.parse(TestParser.hello_then_world, "helo halo helo").isEmpty)
  }

  "sequential parser" should "tab complete the first word normally" in {
    val HeloStringCompletion = List("helo".toList, "helo_completion".toList)
    val HeloHaloStringCompletion = List("helo halo".toList, "helo halo_completion".toList)
    assert(TestParser.complete(TestParser.hello_then_world, "").results === HeloStringCompletion)
    assert(TestParser.complete(TestParser.hello_then_world, "h").results === HeloStringCompletion)
    assert(TestParser.complete(TestParser.hello_then_world, "he").results === HeloStringCompletion)
    assert(TestParser.complete(TestParser.hello_then_world, "helo").results === HeloHaloStringCompletion)
  }

  "sequential parser" should "tab complete the second word normally" in {
    val HeloHaloStringCompletion = List("helo halo".toList, "helo halo_completion".toList)
    assert(TestParser.complete(TestParser.hello_then_world, "helo h").results === HeloHaloStringCompletion)
    assert(TestParser.complete(TestParser.hello_then_world, "helo ha").results === HeloHaloStringCompletion)
    assert(TestParser.complete(TestParser.hello_then_world, "helo hal").results === HeloHaloStringCompletion)
    assert(TestParser.complete(TestParser.hello_then_world, "helo halo").results === HeloHaloStringCompletion)
    assert(TestParser.complete(TestParser.hello_then_world, "helo halo helo").isEmpty)
  }

  //
  // BRANCHING PARSER
  //

  "branching parser" should "should tab complete both words as long as they match substrings" in {
    val HeloStringCompletion = List("helo".toList, "helo_completion".toList)
    val HaloStringCompletion = List("halo".toList, "halo_completion".toList)


    assert(TestParser.complete(TestParser.hello_or_world, "h").results === HeloStringCompletion ::: HaloStringCompletion)

    assert(TestParser.complete(TestParser.hello_or_world, "he").results === HeloStringCompletion)
    assert(TestParser.complete(TestParser.hello_or_world, "hel").results === HeloStringCompletion)
    assert(TestParser.complete(TestParser.hello_or_world, "helo").results === HeloStringCompletion)
    assert(TestParser.complete(TestParser.hello_or_world, "helo helo").isEmpty)

    assert(TestParser.complete(TestParser.hello_or_world, "ha").results === HaloStringCompletion)
    assert(TestParser.complete(TestParser.hello_or_world, "hal").results === HaloStringCompletion)
    assert(TestParser.complete(TestParser.hello_or_world, "halo").results === HaloStringCompletion)
    assert(TestParser.complete(TestParser.hello_or_world, "halo halo").isEmpty)
  }
}

object TestParser extends CompRegexParsers {

  // string parser
  val StringParser : Parser[String] = literal("string")
  val FancyStringParser : Parser[String] = StringParser @@ List("string fancy")
  val PlusStringParser : Parser[String] = StringParser @@ { s => List(s + "+")}

  // regex parser
  val RegexParser : Parser[String] = regex("a{1,2}".r)
  val FancyRegexParser : Parser[String] = RegexParser @@ List("ab")
  val PlusRegexParser : Parser[String] = RegexParser @@ { s => List(s + "b")}

  //combining parsers
  val hello : Parser[String] = literal("helo") @@ List("helo_completion")
  val world : Parser[String] = literal("halo") @@ List("halo_completion")

  val hello_then_world : Parser[String ~ String] = hello ~ world
  val hello_or_world : Parser[String] = hello | world
}