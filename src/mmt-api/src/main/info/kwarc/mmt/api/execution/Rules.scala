package info.kwarc.mmt.api.execution

import info.kwarc.mmt.api._
import objects._

trait PureExecutionCallback {
  //def pureExecute(prog: Term): Term
}

trait ExecutionCallback extends PureExecutionCallback {
  def execute(prog: Term): Term
}

abstract class ExecutionRule(val head: GlobalName, val under: List[GlobalName] = Nil) extends SyntaxDrivenRule with checking.ApplicableUnder {
  protected def headTerm = OMS(head)
  def apply(callback: ExecutionCallback, env: RuntimeEnvironment, prog: Term): Term
}
