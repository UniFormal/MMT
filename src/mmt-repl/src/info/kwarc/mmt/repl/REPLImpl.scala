package info.kwarc.mmt.repl

import java.util
import java.util.logging.{Level, Logger}

import org.jline.reader._
import org.jline.reader.impl.history.DefaultHistory
import org.jline.terminal.TerminalBuilder
import org.jline.terminal.impl.DumbTerminal

import scala.collection.JavaConverters._
import scala.util.Try


/**
  * A wrapper around [[org.jline]] to provide a REPL.
  * Not Thread-Safe.
  *
  * @param historyItems List of items that are considered part of the history of this REPL
  */
abstract class REPLImpl(historyItems: List[String] = Nil) {

  //========================================
  // Functions to be implemented by subclass
  //========================================

  /**
    * Evaluates a single line of code and returns a boolean indicating if the prompt should quit
    *
    * @param line Line of code to complete
    */
  def eval(line: String): Boolean

  /**
    * Given a line, returns a list of suggestions
    *
    * @param line Line of code to suggest completions for
    * @return list of possible suggestions building on the given line
    */
  def suggestions(line: String): List[String]

  /**
    * @return optional prompt to be shown on the left side of the REPL
    */
  def promptLeft: Option[String]

  /**
    * @return optimal prompt to be shown on the right hand side of the REPL
    */
  def promptRight: Option[String]

  //========================================
  // Initialisation Code
  //========================================

  /** the underlying LineReader implementation used by this jLineREPL */
  lazy private val reader: LineReader = {
    val logger = Logger.getLogger("org.jline") // need to hold a reference to the logger to avoid settings to be overridden
    logger.setLevel(Level.OFF) // turns of logging

    // create a new terminal
    val terminal = TerminalBuilder.builder.system(true).build()

    // we use a trivial parser for now, that is one which doesn't parse at all
    // it is required to have the entire line as one 'word', otherwise the completer
    // below will not work as expected
    val parser = new Parser {
      def parse(content: String, index: Int, context: Parser.ParseContext): ParsedLine = {
        new ParsedLine {
          override def cursor(): Int = index
          override def wordCursor(): Int = index

          override def line(): String = content
          override def word(): String = content

          override def words(): java.util.List[String] = List(content).asJava
          override def wordIndex(): Int = 0
        }
      }
    }

    // our completer will use the suggestions method above
    val completer = new Completer {
      override def complete(lineReader: LineReader, parsedLine: ParsedLine, list: util.List[Candidate]): Unit = {
        list.addAll(suggestions(parsedLine.line()).map(new Candidate(_)).asJavaCollection)
      }
    }

    // our history can not be loaded or saved
    val history = new DefaultHistory {
      override def load(): Unit = {}
      override def save(): Unit = {}
    }

    // finally put it all together and make the LineReader
    LineReaderBuilder.builder()
      .terminal(terminal)
      .parser(parser)
      .completer(completer)
      .history(history)
      .build()
  }

  //========================================
  // Private Methods
  //========================================

  /** prompts the user for a single input */
  private def prompt(): Option[String] = Try(reader.readLine(promptLeft.orNull, promptRight.orNull, null:org.jline.reader.MaskingCallback, "")).toOption

  /** checks if this Terminal is dumb */
  protected def isDumb() : Boolean = {
    reader.getTerminal().isInstanceOf[DumbTerminal]
  }

  //========================================
  // Public Methods
  //========================================

  /** gets a list of items that were used inside this prompt */
  def history() : List[String] = reader.getHistory.iterator.asScala.map(_.line).toList

  /** starts a loop of input and output */
  def run(): Unit ={
    var shouldQuit = false

    while (!shouldQuit) {
      shouldQuit = prompt().forall(eval)
    }
  }
}

