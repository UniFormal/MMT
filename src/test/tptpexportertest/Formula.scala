package tptpexportertest

import tptpexportertest.Commons._

sealed abstract class Formula {
  def function_symbols : Set[String]
}
case class Logical(formula: LogicFormula) extends Formula {
  override def toString = formula.toString

  override val function_symbols: Set[String] = formula.function_symbols
}
case class Sequent(tuple1: Seq[LogicFormula], tuple2: Seq[LogicFormula]) extends Formula {
  override def toString = "[" + tuple1.mkString(",") +"]" + " --> " + "[" + tuple2.mkString(",") + "]"

  override val function_symbols: Set[String] = tuple1.toSet[LogicFormula].flatMap(_.function_symbols) union tuple2.toSet[LogicFormula].flatMap(_.function_symbols)
}

sealed abstract class LogicFormula {
  def function_symbols : Set[String]
}
case class Binary(left: LogicFormula, connective: BinaryConnective, right: LogicFormula) extends LogicFormula  {
  override def toString = "(" + left.toString + ") " + connective.toString + " (" + right.toString + ")"

  override val function_symbols: Set[String] = left.function_symbols union right.function_symbols
}
case class Unary(connective: UnaryConnective, formula: LogicFormula) extends LogicFormula {
  override def toString = connective.toString + " (" + formula.toString + ")"

  override val function_symbols: Set[String] = formula.function_symbols
}
case class Quantified(quantifier: Quantifier, varList: Seq[Variable], matrix: LogicFormula) extends LogicFormula {
  override def toString = quantifier.toString + " [" + varList.mkString(",") + "] : (" + matrix.toString + ")"

  override val function_symbols: Set[String] = matrix.function_symbols -- varList
}
case class Atomic(formula: AtomicFormula) extends LogicFormula {
  override def toString = formula.toString

  override val function_symbols: Set[String] = formula.function_symbols
}
case class Inequality(left: Term, right: Term) extends LogicFormula {
  override def toString = left.toString + " != " + right.toString

  override val function_symbols: Set[String] = left.function_symbols union right.function_symbols
}


sealed abstract class BinaryConnective
case object <=> extends BinaryConnective
case object Impl extends BinaryConnective {
  override def toString = "=>"
}
case object <= extends BinaryConnective

// XOR
case object <~> extends BinaryConnective

// NOR
case object ~| extends BinaryConnective

// NAND
case object ~& extends BinaryConnective
case object | extends BinaryConnective
case object & extends BinaryConnective

sealed abstract class UnaryConnective
case object Not extends UnaryConnective  {
  override def toString = "~"
}

sealed abstract class Quantifier
case object ! extends Quantifier
case object ? extends Quantifier