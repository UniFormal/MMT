package info.kwarc.mmt.odk.codecs

import info.kwarc.mmt.api.DPath
import info.kwarc.mmt.api.objects.{OMS, Term}
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.api.valuebases.Coder
import info.kwarc.mmt.lf.ApplySpine

object Codecs {
  val odkpath = DPath(URI("http","www.opendreamkit.org"))
  val path = odkpath ? "Codecs"

  val standardInt = path ? "standardInt"
  val standardNat = path ? "standardNat"
  val standardPos = path ? "standardPos"
  val standardString = path ? "standardString"
  val standardBool = path ? "standardBool"
  val boolAsString = path ? "boolAsString"
  val boolAsInt = path ? "boolAsInt"
  val standardList = path ? "standardList"
  val standardVector = path ? "standardVector"
  val standardMatrix = path ? "standardMatrix"
  val integerPolynomial = path ? "rationalPolynomial"
}

object LMFDBCoder extends Coder(List(TMInt,TMString,BoolAsInt,StandardBool,TMNat,TMPos), List(TMList,StandardVector,StandardMatrix)) {
   override def destruct(t: Term) = t match {
     case ApplySpine(OMS(op), pars) => Some((op, pars))
     case _ => None
   }
}
