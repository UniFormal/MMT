package info.kwarc.mmt.lsp

import info.kwarc.mmt.api.utils.{File, URI}

object Utils {
  def vscodeURIToFile(uri: URI): File = {
    if (uri.scheme.contains("file")) {
      val filePath = (uri.path match {
        case driveLetter :: xs if driveLetter.matches("[a-zA-Z](:|%3A)") =>
          (driveLetter.charAt(0).toUpper + ":") :: xs
        case x => x
      }).mkString(java.io.File.separator)

      File(filePath)
    } else { // fallback
      File(uri.toString)
    }
  }
}
