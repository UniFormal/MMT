package info.kwarc.mmt.leo.AgentSystem.AndOrSystem.DataTreeSystem

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.leo.AgentSystem.AndOrSystem.{AndOrBlackboard, AndOrSection}

/** Class for the section specific to the DataTree[D] class*/
case class DataTreeSection[D](blackboard:AndOrBlackboard[DataTree[D]],g:DataTree[D])(implicit c: Controller,oLP:String) extends AndOrSection(blackboard,g) {}
