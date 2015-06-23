package info.kwarc.mmt.leo

import info.kwarc.mmt.api._
import checking._
import proving._
import objects._
import frontend._

/**
 * represents a proof obligation
 * @param context the left-hand side/antecedent
 * @param tp the type to be inhabited, i.e., the right-hand side/succedent
 *
 * Created by mark on 6/23/15.
 */
abstract class CProvingUnit(context: Context, tp: Term, logPrefix: String)  extends ProvingUnit(context, tp, logPrefix) {

  //def clausify: Clause {  }


}
