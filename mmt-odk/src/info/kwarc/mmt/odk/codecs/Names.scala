package info.kwarc.mmt.odk.codecs

import info.kwarc.mmt.api.objects.{OMS, Term}
import info.kwarc.mmt.api.valuebases.Coder
import info.kwarc.mmt.lf.ApplySpine
import info.kwarc.mmt.odk.ODK

object Codecs {
  val path = ODK.path ? "Codecs"

  val standardInt = path ? "standardInt"
  val standardString = path ? "standardString"
  val standardBool = path ? "standardBool"
  val boolAsInt = path ? "boolAsInt"
  val standardList = path ? "standardList"
  val standardVector = path ? "standardVector"
  val standardMatrix = path ? "standardMatrix"
}

object LMFDBCoder extends Coder(List(TMInt,TMString,BoolAsInt,StandardBool), List(TMList,StandardVector,StandardMatrix)) {
   override def destruct(t: Term) = t match {
     case ApplySpine(OMS(op), pars) => Some((op, pars))
     case _ => None
   }
}