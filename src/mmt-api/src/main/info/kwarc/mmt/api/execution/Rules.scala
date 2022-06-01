package info.kwarc.mmt.api.execution

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.symbols.Constant
import objects._

trait PureExecutionCallback {
  //def pureExecute(prog: Term): Term
}

trait ExecutionCallback extends PureExecutionCallback {
  def execute(prog: Term): Term
}

abstract class ExecutionRule(val head: GlobalName) extends SyntaxDrivenRule with checking.ApplicableUnder {
  protected def headTerm = OMS(head)
  def apply(controller: Controller, callback: ExecutionCallback, env: RuntimeEnvironment, prog: Term): Term
}

abstract class RulePreprocessor(val head: GlobalName) extends SyntaxDrivenRule with checking.ApplicableUnder {
  protected def headTerm = OMS(head)
  def apply(const : Constant) : ExecutionRule
}

object DefinedRun extends RulePreprocessor(???){
  override def under: List[GlobalName]= ???
  def apply(const : Constant) : ExecutionRule = {
    const.tp match {
      case Some(_) => {
        ???
      }
      // case _=> None
    }
  }
}