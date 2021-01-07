package info.kwarc.mmt.mizar.newxml.mmtwrapper

import info.kwarc.mmt._
import api._
import api.objects._
import info.kwarc.mmt.api.uom.StandardNat
import sequences._

/** abstractions for creating sequence-related terms */
object MizSeq {
  val Index = Sequences.index
  val Rep = Sequences.rep
  val Ellipsis = Sequences.ellipsis
  object Sequence {
    def apply = Sequences.flatseq
    def unapply(tm: Term): Option[List[Term]] = tm match {
      case Sequences.flatseq(tms: Seq[Term]) => Some(tms.toList)
      case _ => None
    }
  }

  //def SeqMap(body: Term, index: LocalName, to: Term) = Sequences.ellipsis(to,index,body)

  object OMI {
    def apply(i: Int) = NatRules.NatLit(i)
    def unapply(tm: Term) = tm match {
      case OMLIT(value: Int, rt) if (rt.semType == StandardNat) => Some(value)
      case _ => None
    }
  }
  def nTerms(n:Int) = Rep(Mizar.any, OMI(n))
}
