package info.kwarc.mmt.repl.CompletionEngine

import scala.language.implicitConversions
import scala.language.postfixOps

sealed case class Builder(grammar : Grammar) {

  private def tokenize(input : String) : List[Token] = {
    val tokenizer = Tokenizer(input)
    if(tokenizer.lastOption.exists(_.isWhiteSpace)){
      tokenizer ::: List(Token(""))
    } else {
      tokenizer
    }
  }
  def apply(input : String) : List[String] = apply(tokenize(input)).map(ps => ps._1.map(_.word).mkString + ps._2.word)
  def apply(input : List[Token]) : List[(List[Token], Token)] = grammar.complete(input)

  def + : Builder = Builder(grammar+)

  def + (right : Builder) : Builder = Builder(grammar + right.grammar)

  def + (right : String) : Builder = this + Builder(right)
  def + (right : Node) : Builder = this + Builder(right)
  def + (right : Unit => List[String]) : Builder = this + Builder(right)

  def * : Builder = Builder(grammar.*)
  def ? : Builder = Builder(grammar.?)

  def ~ (right : Builder) : Builder = this + WhiteSpaceNode() + right
  def ~ (right : String) : Builder = this ~ Builder(right)
  def ~ (right : Node) : Builder = this ~ Builder(right)
  def ~ (right : Unit => List[String]) : Builder = this ~ Builder(right)

  def | (right : Grammar) : Builder = Builder(OrG(List(grammar, right)))
  def | (right : Builder) : Builder = this | right.grammar
  def | (right : String) : Builder = this | Builder(right)
  def | (right : Node) : Builder = this | Builder(right)

  def ! : Builder = Builder(grammar.!)
}


object Builder {
  /** A Linear Parser from a list of tokens */
  def apply(values : List[Token]) : Builder = Builder(values.foldRight[Grammar](Empty())((t: Token, g: Grammar) => SeqG(ConstantStringValue(t), g)))

  /** builds a simple Tokenizer from a String */
  def apply(value: String) : Builder = apply(Tokenizer(value))

  /** builds a builder from a Node */
  def apply(node : Node) : Builder = Builder(Grammar.fromList(List(node)))

  /** builds a builder from a builder */
  def apply(builder : Builder): Builder = builder

  /** builds a contextual completer from a function */
  def apply(f :  Unit => List[String]) : Builder = Builder(ContextualStringValue(f))

  /** builds an empty builder */
  def apply() : Builder = Builder(Empty())
}

object BuilderConversions {
  implicit def NodeToBuilder(n : Node) : Builder = Builder(n)
  implicit def StringToBuilder(s: String) : Builder = Builder(s)
  implicit def GrammarToBuilder(g: Grammar) : Builder = Builder(g)
  implicit def FuncToBuilder(f :  Unit => List[String]) : Builder = Builder(ContextualStringValue(f))
}