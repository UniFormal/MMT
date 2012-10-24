package info.kwarc.mmt.api.parser

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.api.utils.mmt
import info.kwarc.mmt.api.objects._
//import info.kwarc.mmt.lf._

/*
 * Structuring
 * {a}{b} [(F a), (F b)] -> type.
 */
object Test {
 def main(args : Array[String]) = {
   val in = new java.io.BufferedReader(new java.io.InputStreamReader(System.in))
   val input = in.readLine()
   println(input)
   //val LFParser = new Parser(LFGrammar.grammar)
   //val tkdecl = LFParser.parse(OMSemiFormal(List(Text("mmt",input))))
   //println(tkdecl)
 }
}