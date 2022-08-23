package info.kwarc.mmt.api.utils

import java.io._

object StreamReading {
  /** a typical (buffered, UTF-8) reader for a stream
    */
  def Reader(in: InputStream): BufferedReader = {
    new BufferedReader(new InputStreamReader(in, java.nio.charset.Charset.forName("UTF-8")))
  }

  /**
    * read a stream into a string
    *
    * @param in the source stream
    * @return s the stream content (line terminators are \n)
    */
  def read(f: InputStream): String = {
    val s = new StringBuilder
    ReadLineWise(f) {l => s.append(l + "\n")}
    s.result
  }

  /** read a stream line by line
    * @param in the stream
    * @param proc a function applied to every line (without line terminator)
    */
  def ReadLineWise(in: InputStream)(proc: String => Unit): Unit = {
    val r = Reader(in)
    var line: Option[String] = None
    try {
      while ({line = Option(r.readLine); line.isDefined}) {
        proc(line.get)
      }
    } finally {
      r.close
    }
  }
}
