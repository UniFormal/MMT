package info.kwarc.mmt.mizar.mmt.objects

import info.kwarc.mmt._
import api._
import api.objects._
import sequences._

/** abstractions for creating sequence-related terms */
object MizSeq {
  val Index = Sequences.index
  val Rep = Sequences.rep
  def SeqMap(body: Term, index: LocalName, to: Term) = Sequences.ellipsis(to,index,body)
  val Sequence = Sequences.flatseq
  
  def OMI(i: Int) = NatRules.NatLit(BigInt(i))
}