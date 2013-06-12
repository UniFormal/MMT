package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._

/** see TorsoForm for explanation on the intended use */
case class Appendages(head: GlobalName, extremities: List[Term])

/** The torso form of a term is named akin to the head normal forms.
 * 
 *  For example, OMA(h1, OMA(h2, tr, ext2), ext1) is in torso form with tr as the torso and
 *  (h1,ext1) and (h2,ext2) as Appendages that are attached to the torso.
 * 
 *  Its torso normal form is TorsoNormalForm(c, Appendages(h1, ext1) :: Appendages(h2, ext2) :: Nil)
 *
 *  The torso form is relevant when h1, h2, ... are repeated applications of elimination operations to an atomic torso.
 *  
 *  These usually extract parts of the information stored in the torso, e.g., chained method invocation on a torso representing an object (in the OO sense).
 *  
 *  For example, OMA(@,OMA(pi1,c),a) arises from the constant c (of product type) by first projection out a component (a function) and then applying it to a.
 */  
case class TorsoForm(torso: Term, apps: List[Appendages]) {
   /** transforms a TorsoForm into the usual form */
   def toHeadForm = apps.foldRight(torso) {case (Appendages(h,ext), t) => OMA(OMS(h), t::ext)}
   /** only the heads */
   def heads = apps.map(_.head)
}

object TorsoForm {
   def fromHeadForm(tm: Term) : TorsoForm = tm match {
      case OMA(OMS(head), torso :: extremities) => fromHeadForm(torso) match {
         case TorsoForm(t, apps) => TorsoForm(t, Appendages(head, extremities) :: apps)
      }
      case _ => TorsoForm(tm, Nil)
   }
}

/** In the torso normal form, the torso cannot be further decomposed anymore, typically it is a variable or constant.
 *  See TorsoForm for further explanations.
 */ 
object TorsoNormalForm {
   def unapply(tm: Term): Option[(Term,List[Appendages])] = {
      val TorsoForm(t,apps) = TorsoForm.fromHeadForm(tm)
      Some((t,apps))
   }
}