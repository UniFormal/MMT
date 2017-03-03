package info.kwarc.mmt.repl.CompletionEngine

import jdk.nashorn.internal.runtime.QuotedStringTokenizer

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Tokenizer {
  def apply(input : String) : List[Token] = {
    // list of tokens we have at the end
    val tokens = ListBuffer[Token]()

    // the string we are working with
    var str = mutable.Queue[Char](input : _*)

    // current word and state
    val word = mutable.Queue[Char]()
    var state = TokenizerState.Init

    while(str.nonEmpty){
      val char = str.dequeue()
      val isWS = isWhiteSpace(char)
      val isQ = isQuote(char)

      state match {
        case TokenizerState.Init => if (isWS){
          state = TokenizerState.Space
        } else if(isQ) {
          state  = TokenizerState.QuotedWord
        } else {
          state = TokenizerState.Word
        }

        case TokenizerState.Space => if (!isWS){
          tokens += Token(word.dequeueAll(_ => true).toList.mkString)
          if(isQ){
            state = TokenizerState.QuotedWord
          } else {
            state = TokenizerState.Word
          }
        }
        case TokenizerState.Word => if (isWS){
          tokens += Token(word.dequeueAll(_ => true).toList.mkString)
          state = TokenizerState.Space
        } else if(isQ) {
          tokens += Token(word.dequeueAll(_ => true).toList.mkString)
          state = TokenizerState.QuotedWord
        }

        case TokenizerState.QuotedWord => if(isQ){
          word.enqueue(char)
          tokens += Token(word.dequeueAll(_ => true).toList.mkString)
          state = TokenizerState.Init
        }
      }

      if(state != TokenizerState.Init){
        word.enqueue(char)
      }
    }

    tokens += Token(word.dequeueAll(_ => true).toList.mkString)
    tokens.toList
  }

  private def isWhiteSpace(c: Char): Boolean = c.toString.matches("^\\s$")
  private def isQuote(c : Char): Boolean = c == "\""(0)
}

object TokenizerState extends Enumeration {
  type TokenizerState = Value
  val Init,Space,Word,QuotedWord = Value
}

sealed case class Token(word : String) {
  def isWhiteSpace : Boolean = word.matches("^\\s+$")
}
