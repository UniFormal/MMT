package info.kwarc.mmt.repl.CompletionEngine

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

  def + (right : Grammar) : Builder = Builder(grammar + right)
  def + (right : Builder) : Builder = this + right.grammar
  def + (right : String) : Builder = this + Builder(right)
  def + (right : Node) : Builder = this + Builder(right)

  def * : Builder = Builder(grammar*)
  def ? : Builder = Builder(grammar?)

  def ~ (right : Grammar) : Builder = this + WhiteSpaceNode() + right
  def ~ (right : Builder) : Builder = this + WhiteSpaceNode() + right
  def ~ (right : String) : Builder = this + WhiteSpaceNode() + right
  def ~ (right : Node) : Builder = this + WhiteSpaceNode() + right

  def ~> (right : Grammar) : Builder = this ~ right
  def ~> (right : Builder) : Builder = this ~ right
  def ~> (right : String) : Builder = this ~ right
  def ~> (right : Node) : Builder = this ~ right

  def <~ (right : Grammar) : Builder = this ~ right
  def <~ (right : Builder) : Builder = this ~ right
  def <~ (right : String) : Builder = this ~ right
  def <~ (right : Node) : Builder = this ~ right

  def | (right : Grammar) : Builder = Builder(OrG(List(grammar, right)))
  def | (right : Builder) : Builder = this | right.grammar
  def | (right : String) : Builder = this | Builder(right)
  def | (right : Node) : Builder = this | Builder(right)
}


object Builder {
  /** A Linear Parser from a list of tokens */
  def apply(values : List[Token]) : Builder = Builder(values.foldRight[LinearGrammar](Empty())((t: Token, g: Grammar) => SeqG(ConstantStringValue(t), g)))

  /** builds a simple Tokenizer from a String */
  def apply(value: String) : Builder = apply(Tokenizer(value))

  /** builds a builder from a Node */
  def apply(node : Node) : Builder = Builder(LinearGrammar.fromList(List(node)))

  /** builds a builder from a builder */
  def apply(builder : Builder): Builder = builder

  /** builds an empty builder */
  def apply() : Builder = Builder(Empty())
}

object BuilderConversions {
  implicit def NodeToBuilder(n : Node) : Builder = Builder(n)
  implicit def StringToBuilder(s: String) : Builder = Builder(s)
  implicit def GrammarToBuilder(g: Grammar) : Builder = Builder(g)
}