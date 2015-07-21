package info.kwarc.mmt.leo.AgentSystem.AndOrSystem.DataTreeSystem

import info.kwarc.mmt.leo.AgentSystem.AndOrSystem.AndOrSection

/** Class for the section specific to the DataTree[D] class*/
class DataTreeSection[D](g:DataTree[D]) extends AndOrSection {
  type ObjectType = DataTree[D]
  var data = g
}
