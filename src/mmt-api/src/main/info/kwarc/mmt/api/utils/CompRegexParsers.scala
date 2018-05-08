/**
  * This file contained a customized and heavily cut down version of scala-parser-combinators, which can be found at:
  *   https://github.com/scala/scala-parser-combinators
  * Furthermore, some parts of the code have been adapted from:
  *   http://marcus-schulte.blogspot.no/2012/04/autocompletion-support-for-scalas.html
  *
  *
  * scala-parser-combinators is licensed as follows:
  *
  * Copyright (c) 2002-2013 EPFL
  * Copyright (c) 2011-2013 Typesafe, Inc.
  *
  * All rights reserved.
  *
  * Redistribution and use in source and binary forms, with or without modification,
  * are permitted provided that the following conditions are met:
  *
  *   * Redistributions of source code must retain the above copyright notice,
  * this list of conditions and the following disclaimer.
  *   * Redistributions in binary form must reproduce the above copyright notice,
  * this list of conditions and the following disclaimer in the documentation
  * and/or other materials provided with the distribution.
  *   * Neither the name of the EPFL nor the names of its contributors
  * may be used to endorse or promote products derived from this software
  * without specific prior written permission.
  *
  * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
  * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
  * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
  * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
  * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
  * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
  * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  *
  */

package info.kwarc.mmt.api.utils

import scala.util.parsing.input._
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec
import scala.util.parsing.input
import scala.util.DynamicVariable
import scala.util.matching.Regex

trait CompParser {

  // element and input types
  type Elem
  protected val separator: Elem
  type Input = Reader[Elem]

  protected def handleSeparators(source: java.lang.CharSequence, offset: Int): Int = offset

  //
  // =============================
  // PARSE RESULTS
  // =============================
  //
  sealed abstract class ParseResult[+T](val next: Input, val successful: Boolean) {

    // mapping this result
    def map[U](f: T => U): ParseResult[U]
    def flatMapWithNext[U](f: T => Input => ParseResult[U]): ParseResult[U]

    // return a new result implementing the sequential composition of this parser with another parser
    def append[U >: T](a: => ParseResult[U]): ParseResult[U]

    // check if the result is empty
    def isEmpty: Boolean = !successful
    def get: T
    def getOrElse[B >: T](default: => B): B =
      if (isEmpty) default else this.get
  }

  case class Success[+T](result: T, override val next: Input) extends ParseResult[T](next, true) {
    // apply a function to the successfull result
    def map[U](f: T => U) = Success(f(result), next)
    def flatMapWithNext[U](f: T => Input => ParseResult[U]): ParseResult[U] = f(result)(next)

    // append a second result
    def append[U >: T](a: => ParseResult[U]): ParseResult[U] = this

    // get an element
    def get: T = result

    // toString
    override def toString: String = "["+next.pos+"] parsed: "+result
  }

  private lazy val lastNoSuccessVar = new DynamicVariable[Option[Option[NoSuccess]]](None)

  sealed abstract class NoSuccess(val msg: String, override val next: Input) extends ParseResult[Nothing](next, false) { // when we don't care about the difference between Failure and Error
    if (lastNoSuccessVar.value exists (_ forall (v => !(next.pos < v.next.pos))))
      lastNoSuccessVar.value = Some(Some(this))

    // mapping a result on failure => do nothing
    def map[U](f: Nothing => U) : ParseResult[U] = this
    def flatMapWithNext[U](f: Nothing => Input => ParseResult[U]): ParseResult[U] = this

    // get a result on failure => Nothing
    def get: Nothing = scala.sys.error("No result when parsing failed")
  }

  object NoSuccess {
    def unapply[T](x: ParseResult[T]): Option[(String, Input)] = x match {
      case Failure(msg, next)   => Some((msg, next))
      case Error(msg, next)     => Some((msg, next))
      case _                    => None
    }
  }

  case class Failure(override val msg: String, override val next: Input) extends NoSuccess(msg, next) {
    def append[U >: Nothing](a: => ParseResult[U]): ParseResult[U] = {
      val alt = a
      alt match {
        case Success(_, _) => alt
        case ns: NoSuccess => if (alt.next.pos < next.pos) this else alt
      }
    }
    override def toString: String = "[" + next.pos + "] failure: "+msg+"\n\n"+next.pos.longString
  }

  case class Error(override val msg: String, override val next: Input) extends NoSuccess(msg, next) {
    def append[U >: Nothing](a: => ParseResult[U]): ParseResult[U] = this
    override def toString: String = "[" + next.pos + "] error: " + msg + "\n\n" + next.pos.longString
  }

  //
  // =============================
  //  COMPLETION RESULTS
  // =============================
  //
  case class CompletionResult(results : List[List[Elem]]) {
    def ::: (other: CompletionResult) = CompletionResult((results ::: other.results).distinct)
    def filter(p : List[Elem] => Boolean) : CompletionResult = CompletionResult(results.filter(p))

    def prefixesOf(input : Input, skipSeparators: Boolean) : CompletionResult = filter(s => {
      val offset = input.offset
      val source = input.source
      val start = handleSeparators(source, offset)

      var i = 0
      var j = start
      while (i < s.length && j < source.length && s(i) == source.charAt(j)) {
        i += 1
        j += 1
      }

      j == source.length
    })

    def map(f: List[Elem] => List[Elem]) : CompletionResult = CompletionResult(results.map(f))

    def isEmpty : Boolean = results.isEmpty
    def nonEmpty: Boolean = results.nonEmpty

    private[CompParser] def prefixSeparatorsIfNeeded(needsSeparation: Boolean) : CompletionResult = {
      if(needsSeparation){
        map(l => separator :: l)
      } else this
    }
  }

  object CompletionResult {
    def empty() : CompletionResult = CompletionResult(List())
  }

  //
  // =============================
  // PARSING
  // =============================
  //
  def Parser[T](f: Input => ParseResult[T], g: (Input, Boolean) => CompletionResult): Parser[T] = new Parser[T]{
    def apply(in: Input) = f(in)
    def complete(in: Input, ns: Boolean) = g(in, ns)
  }

  abstract class Parser[+T] extends (Input => ParseResult[T]) {
    private var name: String = ""
    def named(n: String): this.type = {name=n; this}
    override def toString(): String = "Parser (" + name + ")"

    def apply(in: Input): ParseResult[T]
    def complete(in: Input, needsSeparation: Boolean): CompletionResult

    def withCompletions(f : (Input, Boolean) => CompletionResult): Parser[T] = Parser({ in => apply(in)}, (in, ns) => {
      val completions = complete(in, ns)
      if(completions.nonEmpty) f(in, ns).prefixSeparatorsIfNeeded(ns) ::: completions else completions
    }).named(" @ ")
    def withCompletions(suggestions : CompletionResult): Parser[T] = withCompletions((in, ns) => suggestions.prefixesOf(in, ns))

    def map[U](f: T => U): Parser[U] = Parser({ in => this(in) map f}, complete)
    def flatMap[U](f: T => Parser[U]): Parser[U] = Parser({ in => this(in) flatMapWithNext f}, {(in, ns) => {
      // try to complete f on the output
      val out = this.apply(in)

      if(out.successful) {
        // get the prefix to add to the list
        var prefixList : List[Elem] = Nil
        var sourceR = in
        for( _ <- in.offset until out.next.offset){
          prefixList = sourceR.first :: prefixList
          sourceR = sourceR.rest
        }
        prefixList = prefixList.reverse

        // and complete the next parse
        f(out.get).complete(out.next, needsSeparation=true).map(l => prefixList ::: l)
      } else {
        Parser.this.complete(in, ns)
      }
    }})

    def append[U >: T](p0: => Parser[U]): Parser[U] = { lazy val p = p0 // lazy argument
      Parser({in => this(in) append p(in)}, (in, ns) => { p.complete(in, ns) ::: complete(in, ns) })
    }

    def ~[U](q: => Parser[U]): Parser[~[T, U]] = {
      lazy val p = q // lazy argument
      (for(a <- this; b <- p) yield new ~(a,b)).named("~")
    }

    def ~>[U](q: => Parser[U]): Parser[U] = {
      lazy val p = q // lazy argument
      (for(a <- this; b <- p) yield b).named("~>")
    }

    def <~[U](q: => Parser[U]): Parser[T] = {
      lazy val p = q // lazy argument
      (for(a <- this; b <- p) yield a).named("<~")
    }

    def |[U >: T](q: => Parser[U]): Parser[U] = append(q).named("|")
    def ^^[U](f: T => U): Parser[U] = map(f).named(toString + "^^")

    def ^^^[U](v: => U): Parser[U] =  new Parser[U] {
      private lazy val v0 = v // lazy argument
      def apply(in: Input) : ParseResult[U] = Parser.this(in) map (x => v0)
      def complete(in: Input, ns: Boolean) : CompletionResult = Parser.this.complete(in, ns)
    }.named(toString + "^^^")

    def * : Parser[List[T]] = rep(this)
    def *[U >: T](sep: => Parser[(U, U) => U]) : Parser[U]  = chainl1(this, sep)
    def + : Parser[List[T]] = rep1(this)
    def ? : Parser[Option[T]] = opt(this)
  }

  def elem(kind: String, p: Elem => Boolean): Parser[Elem] = acceptIf(p)(inEl => kind + " expected")

  def elem(e: Elem): Parser[Elem] = accept(e)

  implicit def accept(e: Elem): Parser[Elem] = acceptIf(_ == e)("'" + e + "' expected but " + _ + " found")

  def accept[U](expected: String, f: PartialFunction[Elem, U]): Parser[U] = acceptMatch(expected, f)
  def acceptIf(p: Elem => Boolean)(err: Elem => String): Parser[Elem] = Parser({ in =>
    if (in.atEnd) Failure("end of input", in)
    else if (p(in.first)) Success(in.first, in.rest)
    else Failure(err(in.first), in)
  }, { (in, ns) => CompletionResult.empty() })
  def acceptMatch[U](expected: String, f: PartialFunction[Elem, U]): Parser[U] = Parser({ in =>
    if (in.atEnd) Failure("end of input", in)
    else if (f.isDefinedAt(in.first)) Success(f(in.first), in.rest)
    else Failure(expected+" expected", in)
  }, (in, ns) => CompletionResult.empty())

  def failure[T](msg: String) : Parser[T] = Parser({ in => Failure(msg, in) }, (in, c) => CompletionResult.empty())
  def err[T](msg: String) : Parser[T]  = Parser({ in => Error(msg, in) }, (in, c) => CompletionResult.empty())
  def success[T](v: T) : Parser[T] = Parser({ in => Success(v, in) }, (in, c) => CompletionResult.empty())

  def rep[T](p: => Parser[T]): Parser[List[T]] = rep1(p) | success(List())

  def repsep[T](p: => Parser[T], q: => Parser[Any]): Parser[List[T]] =
    rep1sep(p, q) | success(List())

  def rep1[T](p: => Parser[T]): Parser[List[T]] = rep1(p, p)

  def rep1[T](first: => Parser[T], p0: => Parser[T]): Parser[List[T]] = Parser({ in =>
    lazy val p = p0 // lazy argument
    val elems = new ListBuffer[T]

    def continue(in: Input): ParseResult[List[T]] = {
      val p0 = p    // avoid repeatedly re-evaluating by-name parser
      @tailrec def applyp(in0: Input): ParseResult[List[T]] = p0(in0) match {
        case Success(x, rest) => elems += x ; applyp(rest)
        case e @ Error(_, _)  => e  // still have to propagate error
        case _                => Success(elems.toList, in0)
      }

      applyp(in)
    }

    first(in) match {
      case Success(x, rest) => elems += x ; continue(rest)
      case ns: NoSuccess    => ns
    }
  }, { (in, ns) => first.complete(in, ns) }) // just apply completion first

  def rep1sep[T](p : => Parser[T], q : => Parser[Any]): Parser[List[T]] =
    p ~ rep(q ~> p) ^^ {case x~y => x::y}

  def chainl1[T](p: => Parser[T], q: => Parser[(T, T) => T]): Parser[T]
  = chainl1(p, p, q)

  def chainl1[T, U](first: => Parser[T], p: => Parser[U], q: => Parser[(T, U) => T]): Parser[T]
  = first ~ rep(q ~ p) ^^ {
    case x ~ xs => xs.foldLeft(x: T){case (a, f ~ b) => f(a, b)} // x's type annotation is needed to deal with changed type inference due to SI-5189
  }

  def chainr1[T, U](p: => Parser[T], q: => Parser[(T, U) => U], combine: (T, U) => U, first: U): Parser[U]
  = p ~ rep(q ~ p) ^^ {
    case x ~ xs => (new ~(combine, x) :: xs).foldRight(first){case (f ~ a, b) => f(a, b)}
  }

  def opt[T](p: => Parser[T]): Parser[Option[T]] =
    p ^^ (x => Some(x)) | success(None)

  def phrase[T](p: Parser[T]) : Parser[T] = new Parser[T]{
    def apply(in: Input) : ParseResult[T] = lastNoSuccessVar.withValue(Some(None)) {
      p(in) match {
        case s @ Success(out, in1) =>
          if (in1.atEnd)
            s
          else
            lastNoSuccessVar.value flatMap (_ filterNot { _.next.pos < in1.pos }) getOrElse Failure("end of input expected", in1)
        case ns => lastNoSuccessVar.value.flatten.getOrElse(ns)
      }
    }
    def complete(in : Input, ns: Boolean) : CompletionResult = p.complete(in, ns)
  }

  case class ~[+a, +b](_1: a, _2: b) {
    override def toString : String = "(" + _1 + "~" + _2 + ")"
  }
}


trait CompRegexParsers extends CompParser {

  // this parser always uses chars
  override type Elem = Char


  // OVERRIDE THE PARSER

  implicit def SuperToMe[T](p : super.Parser[T]) : Parser[T] = new Parser[T] {
    def apply(in: Input) = p.apply(in)
    def complete(in : Input, needsSeparation : Boolean) : CompletionResult = p.complete(in, needsSeparation)
  }

  implicit def CompletionResultFromStringList(lst : List[String]) : CompletionResult = CompletionResult(lst.map(_.toList))

  abstract class Parser[+T] extends super.Parser[T] {
    def @@(suggestions : List[String]) : Parser[T] = withCompletions(suggestions)
    def @@(f : String => List[String]) : Parser[T] = withCompletions((in, ns) => {
      val str = in.source.subSequence(in.offset, in.source.length).toString
      f(str)
    })
  }

  // white space handling
  protected val separator : Elem = ' '
  protected val whiteSpace : Regex = """\s+""".r
  def skipWhitespace : Boolean = whiteSpace.toString.length > 0

  // skip whitespace if applicable
  override protected def handleSeparators(source: java.lang.CharSequence, offset: Int): Int =
    if (skipWhitespace)
      whiteSpace findPrefixMatchOf source.subSequence(offset, source.length) match {
        case Some(matched) => offset + matched.end
        case None => offset
      }
    else
      offset

  protected def handleWhiteSpaceCompletion(source: java.lang.CharSequence, offset: Int, needsSeparation: Boolean): (Int, String) = {
    val newOffset = handleSeparators(source, offset)
    val prefix = source.subSequence(offset, newOffset).toString

    (newOffset, if((needsSeparation && skipWhitespace) && prefix.isEmpty) separator.toString else prefix)
  }

  // trying to match a literal
  implicit def literal(s: String): Parser[String] = new Parser[String] {
    def apply(in: Input) : ParseResult[String] = {

      // setup input and handle white space
      val source = in.source
      val offset = in.offset
      val start = handleSeparators(source, offset)

      // compare strings char by char and see how far we match
      var i = 0
      var j = start
      while (i < s.length && j < source.length && s.charAt(i) == source.charAt(j)) {
        i += 1
        j += 1
      }


      if (i == s.length) // we found all the source
        Success(source.subSequence(start, j).toString, in.drop(j - offset))

      else { // we did not find anything => there is an error
        val found = if (start == source.length()) "end of source" else "`" + source.charAt(start) + "'"
        Failure("`" + s + "' expected but " + found + " found", in.drop(start - offset))
      }
    }

    def complete(input : Input, ns: Boolean): CompletionResult = {
      val source = input.source
      val offset = input.offset
      val (start, prefix) = handleWhiteSpaceCompletion(source, offset, ns)


      // compare strings char by char and see how far we match
      var i = 0
      var j = start
      while (i < s.length && j < source.length && s.charAt(i) == source.charAt(j)) {
        i += 1
        j += 1
      }

      if(j == source.length()){// if we were able to reach the end of the source
        CompletionResult(List((prefix + s).toList))
      } else {
        CompletionResult.empty()
      }
    }
  }

  // trying to match a regular expression
  implicit def regex(r: Regex): Parser[String] = new Parser[String] {
    def apply(in: Input) : ParseResult[String] = {

      // setup input and find white space
      val source = in.source
      val offset = in.offset
      val start = handleSeparators(source, offset)

      // find a match of the string
      r findPrefixMatchOf source.subSequence(start, source.length) match {
        case Some(matched) => // we found a match
          Success(source.subSequence(start, start + matched.end).toString,
            in.drop(start + matched.end - offset))

        case None => // we did not find a match
          // TODO: Check if we can make suggestions somehow
          val found = if (start == source.length()) "end of source" else "`" + source.charAt(start) + "'"
          Failure("string matching regex `" + r + "' expected but " + found + " found", in.drop(start - offset))
      }
    }

    def complete(in : Input, ns: Boolean) : CompletionResult = {
      // setup input and find white space
      val source = in.source
      val offset = in.offset
      val (start, prefix) = handleWhiteSpaceCompletion(source, offset, ns)

      r findPrefixMatchOf source.subSequence(start, source.length) match {
        case Some(matched) if start + matched.end == source.length => // we matched the entire string
          CompletionResult(List((prefix + source.subSequence(start, start + matched.end).toString).toList))
        case _ => CompletionResult.empty()
      }
    }
  }

  // parse parts of the input
  private def parseI[T](p: Parser[T], in: Reader[Char]): ParseResult[T] = p(in)
  private def parseI[T](p: Parser[T], in: java.lang.CharSequence): ParseResult[T] = p(new CharSequenceReader(in))
  private def parseI[T](p: Parser[T], in: java.io.Reader): ParseResult[T] = p(new PagedSeqReader(PagedSeq.fromReader(in)))

  // parse everything
  def parse[T](p: Parser[T], in: Reader[Char]): ParseResult[T] = parseI(phrase(p), in)
  def parse[T](p: Parser[T], in: java.io.Reader): ParseResult[T] = parseI(phrase(p), in)
  def parse[T](p: Parser[T], in: java.lang.CharSequence): ParseResult[T] = parseI(phrase(p), in)

  // complete
  def complete[T](p: Parser[T], in : Reader[Char]) : CompletionResult = p.complete(in, needsSeparation=false)
  def complete[T](p: Parser[T], in : java.lang.CharSequence) : CompletionResult = p.complete(new CharSequenceReader(in, 0), needsSeparation=false)
}
