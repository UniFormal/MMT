package info.kwarc.mmt.leo.provers.lfprover

import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.proving.{Atom, Facts}
import info.kwarc.mmt.leo.datastructures._

/**
 * Created by Mark on 7/15/2015.
 */

/*
class LFProofData(val contextVar: Context, var concVar: Term, conjunctiveVar: Boolean, isSatisfiableVar: Option[Boolean] = None )
  extends ProofData((contextVar,concVar),conjunctiveVar,isSatisfiableVar) {
  var context = contextVar
  var conc = concVar
  /** sets a new goal, can be used by the prover to simplify goals in place */
  def setConc(newConc: Term)= {concVar = newConc}

}

class LFAndOrTreeTree(dataVar: LFProofData) extends AndOr(dataVar) {

  var context = dataVar.context
  var conc = dataVar.conc

  def setConc(newConc: Term) = {
    dataVar.concVar = newConc
  }

  /** the complete context/antecedent (i.e., including the parent's context) of this sequent */
 // lazy val fullContext: Context = parent.map(_.fullContext).getOrElse(Context()) ++ context

  /** the local context of this goal seen as a list of atomic facts that rules can make use of */
  lazy val varAtoms: List[Atom] = context.flatMap {
    case IncludeVarDecl(_,_) => Nil
    case StructureVarDecl(_,_,_) => Nil
    case VarDecl(n,Some(t),_,_) => List(Atom(OMV(n), t, None))
    case VarDecl(_, None,_,_) => Nil
  }
  /** the complete context of this goal seen as a list of atomic facts that rules can make use of */
  //lazy val fullVarAtoms: List[Atom] = parent.map(_.fullVarAtoms).getOrElse(Nil) ::: varAtoms


}
*/
