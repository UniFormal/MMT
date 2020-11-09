package info.kwarc.mmt.mizar.newxml

import info.kwarc.mmt.api.utils._

object Main {
  def makeParser = new info.kwarc.mmt.api.utils.XMLToScala("info.kwarc.mmt.mizar.newxml.syntax")

  def main(args: Array[String]) {
    val parser = makeParser
    val files= args.map(arg=>File(arg))
    val parsedFiles=files.map(f => parser(f).asInstanceOf[syntax.Text_Proper])
    parsedFiles.foreach {parsedArt => println(parsedArt.toString)}
    //val f = File(args(0))
    //val a = parser(f).asInstanceOf[syntax.Text_Proper]
    //println(a.toString)
  }
}