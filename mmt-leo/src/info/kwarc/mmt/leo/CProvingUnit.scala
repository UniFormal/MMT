package info.kwarc.mmt.leo
import _root_.leo.agents.impl._
import _root_.leo.datastructures.blackboard.Blackboard
import _root_.leo.datastructures.blackboard.scheduler.Scheduler
import _root_.leo.datastructures.context.Context
import _root_.leo.datastructures.impl._
import _root_.leo.modules._
import _root_.leo.modules.Utility._
import _root_.leo.modules.output._
import _root_.leo.modules.Phase._

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.proving._


/**
 * represents a proof obligation
 * @param context the left-hand side/antecedent
 * @param tp the type to be inhabited, i.e., the right-hand side/succedent
 *
 * Created by mark on 6/23/15.
 */
abstract class CProvingUnit(context: objects.Context, tp: Term, logPrefix: String)  extends ProvingUnit(context, tp, logPrefix) {

  //def clausify: Clause = {
  // context.getDomain foreach {v -> Clause}
  //}
  val test = new ClausificationAgent()
  val sig = Signature.empty

  //sig.addBaseType("prop")
  //sig.add
}
