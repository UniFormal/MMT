package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._

/** see TorsoForm for explanation on the intended use */
case class Appendage(head: GlobalName, extremities: List[Term])

/** The torso form of a term is named akin to the head normal forms.
 * 
 *  For example, OMA(h1, OMA(h2, tr, ext2), ext1) is in torso form with tr as the torso and
 *  (h1,ext1) and (h2,ext2) as Appendages that are attached to the torso.
 * 
 *  Its torso normal form is TorsoNormalForm(c, Appendage(h1, ext1) :: Appendage(h2, ext2) :: Nil)
 *
 *  The point of the torso normal form is understood by considering a type theory, whose constructor are paired into introduction and elimination operators. 
 *  Then the torso form is a useful representation for a series h1, h2, ... of eliminators applied to a term (the torso).
 *  The torso is either atomic or a sequence of introductors.
 *  In the latter case, a reduction rule (e.g., beta) can be applied, which eventually yields an atomic torso.
 *  
 *  For example, OMA(@,OMA(pi1,c),a) arises from the constant c (of product type) by first projection out a component (a function) and then applying it to a.
 */
case class TorsoForm(torso: Term, apps: List[Appendage]) extends utils.HashEquality[TorsoForm] {
   /** transforms a TorsoForm into the usual form */
   def toHeadForm : Term = apps.foldRight(torso) {case (Appendage(h,ext), t) => OMA(OMS(h), t::ext)}
   /** only the heads */
   def heads = apps.map(_.head)
}

//TODO there should be an analogous representation of introduction sequences

object TorsoForm {
   def fromHeadForm(tm: Term, unknowns : List[LocalName]) : TorsoForm = tm match {
      case OMA(OMS(head), torso :: extremities) =>
         torso match {
            case OMV(name) if unknowns contains name => // changed
               TorsoForm(tm, Nil)
            case _ =>
               fromHeadForm(torso, unknowns) match {
                  case TorsoForm(t, apps) => TorsoForm(t, Appendage(head, extremities) :: apps)
               }
         }
      case _ => TorsoForm(tm, Nil)
   }
}

/** In the torso normal form, the torso cannot be further decomposed anymore, typically it is a variable or constant.
 *  See TorsoForm for further explanations.
 */ 
case class TorsoNormalForm(unknowns:List[LocalName]) {
   def unapply(tm: Term): Option[(Term,List[Appendage])] = {
      val TorsoForm(t,apps) = TorsoForm.fromHeadForm(tm,unknowns)
      Some((t,apps))
   }
}