package info.kwarc.mmt.odk.SCSCP.Lowlevel.Readers

import java.io.{InputStream, InputStreamReader}

import info.kwarc.mmt.api.ImplementationError

import scala.collection.mutable

/**
  * Represents a reader that can read lines both in a blocking and non-blocking way.
  *
  * @param stream   InputStream to read / write characters from
  * @param encoding Encoding to use. Defaults to UTF-8.
  */
class BufferedLineReader(stream: InputStream, val encoding: String = "UTF-8") {


  private final val CARRIAGE_RETURN_CHAR: Char = '\r'
  private final val NEWLINE_CHAR: Char = '\n'


  // the reader for the input stream
  private val reader: InputStreamReader = new InputStreamReader(stream, encoding)

  // a list of characters contained in the input queue
  private val chars: mutable.Queue[Char] = mutable.Queue()

  /**
    * Enqueues the next character received from the socket.
    * Blocks in case no character is currently available
    */
  private def enQueue(): Unit = {
    chars.enqueue(reader.read().asInstanceOf[Char])
  }

  /**
    * Fills up the InQueue with all characters already available
    */
  private def fillQueue(): Unit = {
    while (reader.ready()) {
      enQueue()
    }
  }

  /**
    * Resets this BufferedLineReader to the first char it read.
    */
  def reset(): Unit = stream.reset()

  /**
    * Checks if this BufferedSocketReader has a character that is ready to be read
    *
    * @return
    */
  def hasChar: Boolean = chars.nonEmpty || reader.ready()

  /**
    * Reads the next character in this BufferedLineReader.
    * Does not block, i.e. returns None if no character is currently available.
    *
    * @return
    */
  def readChar: Option[Char] = {

    // Read all available characters
    fillQueue()

    // and return if it is empty or not
    if (chars.isEmpty) {
      None
    } else {
      Some(chars.dequeue())
    }
  }

  /** Reads the next character in this BufferedLineReader.
    * Blocks, i.e. waits until a character is available.
    *
    * @return
    */
  def readCharBlock: Char = {
    // if we have no characters, wait for the next one
    if (chars.isEmpty) {
      enQueue()
    }

    // and then get the first item
    chars.dequeue()
  }

  /**
    * Convenience method that reads the next character in this BufferedLineReader in a blocking or non-blocking way.
    *
    * @param block Boolean indicating if we should block until the next character is available
    * @return
    */
  def readChar(block: Boolean = false): Option[Char] = if (block) Some(readCharBlock) else readChar

  /**
    * Checks if a line ends at a given position
    *
    * @param pos Position to check end of line at
    * @return
    */
  private def isEndOfLine(pos: Int): Boolean = {
    // get a function for the options
    val clift = chars.lift

    (clift(pos), clift(pos + 1)) match {
      // we need either \n
      case (Some(NEWLINE_CHAR), _) => true
      // or \r\n
      case (Some(CARRIAGE_RETURN_CHAR), Some(NEWLINE_CHAR)) => true

      // else we have nothing
      case _ => false
    }
  }

  /**
    * Removes the end of line character from the current Quenue
    */
  private def deQueueEnd(): Unit = {
    val clift = chars.lift

    (clift(0), clift(1)) match {
      // just a \n
      case (Some(NEWLINE_CHAR), _) =>
        chars.dequeue()

      // or \r\n
      case (Some(CARRIAGE_RETURN_CHAR), Some(NEWLINE_CHAR)) =>
        chars.dequeue()
        chars.dequeue()
      case _ => throw ImplementationError("line must end with \\n or \\r\\n")
    }
  }

  /**
    * Gets the length of the current line in the buffer or None, in case no line is available
    *
    * @return
    */
  def getLineLength: Option[Int] = {
    // fill up
    fillQueue()

    // counter and length variable
    var i = 0
    val l = chars.length - 1

    // iterate through the Q
    while (i <= l) {

      // if it is the end of the line congrats
      if (isEndOfLine(i)) {
        return Some(i)
      }

      i += 1
    }

    // else return None
    None
  }

  /**
    * Checks if this BufferedSocketReader has a line that is ready to be read
    *
    * @return
    */
  def hasLine: Boolean = getLineLength.isDefined

  /**
    * Reads the next line in this BufferedLineReader.
    * Does not block, i.e. returns None if no line is currently available.
    */
  def readLine: Option[String] = {
    val lines = getLineLength.map(ln => {

      // get a line
      val line = (1 to ln).map(i => chars.dequeue()).mkString

      // and discard the end
      deQueueEnd()

      // and return the line
      line
    })

    lines
  }

  /**
    * Reads the next line in this BufferedLineReader.
    * Blocks, i.e. waits until a new line is available if it currently isn't
    */
  def readLineBlock: String = {
    // the current line
    val line = new StringBuilder

    // indicator for a carriage return character
    var hasCR = false

    while (true) {
      // read the next character
      val char = readCharBlock

      // if we have a newline, return
      if (char == NEWLINE_CHAR) {
        return line.toString
      }

      // if we had a cr, but no newline add it back
      if (hasCR) {
        line += CARRIAGE_RETURN_CHAR
        hasCR = false
      }

      // if we had a CR, indicate it for the next iteration
      if (char == CARRIAGE_RETURN_CHAR) {
        hasCR = true

        // else just append the char to the buffer
      } else {
        line += char
      }
    }

    throw new IllegalStateException("exited a non-breaking while(true) loop. ")
  }

  /**
    * Convenience method that reads the next line in this BufferedLineReader in a blocking or non-blocking way.
    *
    * @param block Boolean indicating if we should block until the next line is available
    * @return
    */
  def readLine(block: Boolean): Option[String] = if (block) Some(readLineBlock) else readLine
}
