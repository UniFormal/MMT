package info.kwarc.mmt.sequences

import info.kwarc.mmt.api._
import uom._
import objects._

import Nat._

import SynOpType._

import SemanticOperator._

object NatRules {
  private val n = OMS(nat)
  private val N = StandardNat
  object NatLit extends RepresentedRealizedType(n,N)
  object Succ extends RealizedOperator(succ.path, n =>: n, Arithmetic.Succ, N =>: N)
  object Zero extends RealizedOperator(zero, n, Arithmetic.Zero, N)
  object One extends RealizedOperator(one, n, Arithmetic.One, N)
}
