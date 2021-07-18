package info.kwarc.mmt.mizar.mmtwrapper

import info.kwarc.mmt._
import api._
import api.objects._
import info.kwarc.mmt.lf.Univ
import sequences._

/** abstractions for creating sequence-related terms */
object MizSeq {
  val Index = Sequences.index
  val Rep = Sequences.rep
  val Ellipsis = Sequences.ellipsis
  object Sequence {
    def apply = Sequences.flatseq
    def apply(tms: List[Term]): Term = apply(tms:_*)
    def unapply(tm: Term): Option[List[Term]] = tm match {
      case OMA(Sequences.flatseq.term, tms) => Some(tms)
      case _ => None
    }
  }

  //def SeqMap(body: Term, index: LocalName, to: Term) = Sequences.ellipsis(to,index,body)

  object OMI {
    def apply(i: Int) = NatRules.NatLit(i)
    def unapply(tm: Term) = tm match {
      case NatRules.NatLit(i) => Some(i.toInt)
      case _ => None
    }
  }
  def nTerms(n:Int) = Rep(MizarPrimitiveConcepts.any, OMI(n))
  def nTypes(n:Int) = Rep(Univ(1), OMI(n))
}