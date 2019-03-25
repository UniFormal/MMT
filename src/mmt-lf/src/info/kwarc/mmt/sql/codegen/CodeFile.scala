package info.kwarc.mmt.sql.codegen

import java.io.PrintWriter

case class CodeFile(replacements: Map[String, String], templatePath: String, destinationPath: Option[String] = None) {

  def writeToFile(write: Boolean): Unit = {
    // TODO should it be line by line?
    if (write) {
      val template = scala.io.Source.fromFile(templatePath).mkString
      val result = replacements.foldLeft(template)((str, mapItem) => {
        str.replaceAllLiterally(mapItem._1, mapItem._2)
      })

      val actualDestination = destinationPath.getOrElse(templatePath)
      val pw = new PrintWriter(new java.io.File(actualDestination))
      try pw.write(result) finally pw.close()
    }
  }

}