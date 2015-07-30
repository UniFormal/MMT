package info.kwarc.mmt.leo.AgentSystem.AndOrSystem.DataTreeSystem.PartitionImpl

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.leo.AgentSystem.AndOrSystem.AndOrBlackboard
import info.kwarc.mmt.leo.AgentSystem.AndOrSystem.DataTreeSystem.{DataTree}

/**
 * Created by Mark on 7/21/2015.
 */
class IntBlackboard(g:DataTree[Int])(implicit c: Controller,oLP:String) extends AndOrBlackboard[DataTree[Int]](g) {}
