package info.kwarc.mmt.mizar.newxml.mmtwrapper

import info.kwarc.mmt._
import api._
import api.objects._
import sequences._

/** abstractions for creating sequence-related terms */
object MizSeq {
  val Index = Sequences.index
  val Rep = Sequences.rep
  val Ellipsis = Sequences.ellipsis
  val Sequence = Sequences.flatseq
  //def SeqMap(body: Term, index: LocalName, to: Term) = Sequences.ellipsis(to,index,body)

  def OMI(i: Int) = NatRules.NatLit(i)
  def nTerms(n:Int) = Rep(Mizar.any, OMI(n))
}
