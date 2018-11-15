package info.kwarc.mmt.sequences

import info.kwarc.mmt.api._
import uom._
import objects._

import Nat._

import info.kwarc.mmt.lf._

object NatRules {
  private val n = OMS(nat)
  private val N = StandardNat
  object NatLit extends RepresentedRealizedType(n,N)
  object Succ extends RealizedOperator(succ.path, SynOpType(List(Apply.path),List(n),n), Arithmetic.Succ, N =>: N)
  object Zero extends RealizedValue(zero, n, Arithmetic.Zero)
  object One extends RealizedValue(one, n, Arithmetic.One)
}
