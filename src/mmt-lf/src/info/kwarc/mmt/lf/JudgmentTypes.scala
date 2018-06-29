package info.kwarc.mmt.lf

import info.kwarc.mmt.api._
import libraries._
import objects._

object JudgmentTypes {
     /**
    * Determines whether a type is a judgment type
    * @param tp the argument type of a constant
    * @return true if tp is atomic and formed from a symbol with role "Judgment" or a function type returning such a judgment type
    *
    * Other extensions may want to override this to consider more types as judgment types
    * (e.g., Sigma types formed from judgment types).
    */
   def isJudgment(tp: Term)(implicit lup: Lookup): Boolean = tp match {
      case FunType(_, ApplySpine(OMS(s),_)) =>
         //this can throw errors if the implicit graph is not fully loaded
         try {
            lup.getConstant(s).rl.contains("Judgment")
         } catch {case e: Error =>
            false
         }
      case _ => false
   }
}