package info.kwarc.mmt.repl.CompletionEngine

sealed abstract class Node {
  def applicable(token : Token) : Boolean
  def suggest(token : Token) : List[Token]

  def g = SeqG(this, Empty())

  def + (right : Node) = SeqG(this, SeqG(right, Empty()))
  def + (right : Grammar) = SeqG(this, right)

  def | (right : Node) = OrG(List(g, right.g))
  def | (right : Grammar) : OrG = right match {
    case OrG(b) => OrG(right :: b)
    case _ =>   OrG(List(g, right))
  }
}

trait SpecialNode extends Node {
  val node : Node
  def applicable(token : Token) : Boolean = node.applicable(token)
  def suggest(token : Token) : List[Token] = node.suggest(token)
}

case class RepeatingNode(node : Node) extends SpecialNode
case class OptionalNode(node : Node) extends SpecialNode

trait StaticSuggestions {
  protected val suggestions : List[Token]
  def suggest(token : Token) : List[Token] = suggestions.filter(tk => tk.word.startsWith(token.word))
}

case class StringValue(examples : List[String]) extends Node with StaticSuggestions {
  protected val suggestions : List[Token] = examples.map(Token)
  def applicable(token : Token) : Boolean = token.word.length > 0
}

case class IntegerValue(examples : List[Int]) extends Node with StaticSuggestions {
  protected val suggestions : List[Token] = examples.map(ex => Token(ex.toString))
  def applicable(token : Token) : Boolean = token.word.forall(_.isDigit)
}


case class WhiteSpaceNode() extends Node {
  def applicable(token : Token) : Boolean  = token.isWhiteSpace
  def suggest(token : Token) : List[Token] = applicable(token) match {
    case true => List(token)
    case false => token.word.length == 0 match {
      case true => List(Token(" "))
      case false => Nil
    }
  }
}

case class ConstantStringValue(constant : Token)  extends Node {
  def applicable(token : Token) = token == constant
  def suggest(token : Token) = if(constant.word.startsWith(token.word)) List(constant) else Nil
}