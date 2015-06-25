package info.kwarc.mmt.api.proving

import info.kwarc.mmt.api._
import objects._

case class Clause(context: Context, literals: List[Literal]) {
  def add(l : Literal): List[Literal] = List(l):::this.literals
  def add(l : List[Literal]): List[Literal]  = l:::this.literals
}

abstract class Literal
case class Postive(t: Term) extends Literal
case class Negative(t: Term) extends Literal