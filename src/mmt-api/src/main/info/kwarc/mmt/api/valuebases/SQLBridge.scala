package info.kwarc.mmt.api.valuebases

import info.kwarc.mmt.api._
import frontend._
import modules._
import symbols._
import objects._

case class Table()

case class Column()

case class Filter()

class SQLBridge(controller: Controller) {
   def theoryToTable(t: Theory): Table = {
     val cols = t.getConstants map {
       case c: Constant => constantToColumn(c)
     }
     Table()
   }
   def constantToColumn(t: Constant): Column = {
     ???
   }
   def termToFilter(t: Term): Filter = {
     ???
   }
}