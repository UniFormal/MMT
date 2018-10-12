package info.kwarc.mmt.odk.Sage

import info.kwarc.mmt.MitM.VRESystem.VREWithAlignmentAndSCSCP
import info.kwarc.mmt.MitM.{MitMSystems, MitM}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.lf.ApplySpine
import info.kwarc.mmt.odk.OpenMath.OMSymbol
import info.kwarc.mmt.odk.StringLiterals
import info.kwarc.mmt.sequences.Sequences

class SageSystem extends VREWithAlignmentAndSCSCP("Sage",MitMSystems.sagesym,OMSymbol("MitM_Evaluate", "scscp_transient_1", None, None), "ODK/Sage") {
  object NonTrivials extends StatelessTraverser {
    val nf = Sage.docpath ? """sage.rings.number_field.number_field""" ? "NumberField"
    override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
      case ApplySpine(OMS(MitM.polycons),List(_,r,_,lst)) =>
        val ls = Sequences.flatseq.unapplySeq(lst).getOrElse(return t)
        OMA(r,ls.toList)
      case OMA(OMS(MitM.polycons),List(_,r,_,lst)) =>
        val ls = Sequences.flatseq.unapplySeq(lst).getOrElse(return t)
        OMA(r,ls.toList)
      case ApplySpine(OMS(`nf`),a :: Nil) =>
        OMA(OMS(`nf`),List(a,StringLiterals("x")))
      case OMA(OMS(`nf`),a :: Nil) =>
        OMA(OMS(`nf`),List(a,StringLiterals("x")))
      case _ => Traverser(this,t)
    }
  }

  override def translateToSystem(t: Term): Term = NonTrivials(super.translateToSystem(t),Context.empty)
}
