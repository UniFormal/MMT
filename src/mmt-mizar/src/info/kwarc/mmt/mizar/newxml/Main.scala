package info.kwarc.mmt.mizar.newxml

import info.kwarc.mmt.api.utils._

object Main {
  def makeParser = new info.kwarc.mmt.api.utils.XMLToScala("info.kwarc.mmt.mizar.newxml.syntax")

  def main(args: Array[String]) {
    val parser = makeParser
    val f = File(args(0))
    val a = parser(f).asInstanceOf[syntax.Article]
    println(a.toString)
  }
}