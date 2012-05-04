package info.kwarc.mmt.api.patterns

import info.kwarc.mmt.api._
import libraries._
import modules._
import frontend._
import symbols._
import objects._
import objects.Conversions._
import utils._
import utils.MyList._
import scala.io.Source

/** elaborates Instance declarations
 * this is also called the pragmatic-to-strict translation
 */
class PatternChecker(controller: Controller) extends Elaborator {
  def getPatterns(home : Term)(n : Int) : List[Pattern] = {     
     home match {
       case OMMOD(p) => 
         val thy = controller.globalLookup(p)
         thy match {
           case d : DeclaredTheory => d.meta match {
             case Some(m) => 
               val cmeta = controller.globalLookup(m)
               cmeta match {
                 case mthy : DeclaredTheory => 
                   val decls = mthy.valueList
                   decls.mapPartial{
                     case p : Pattern => 
                       if (p.body.variables.toList.length == n) { 
                         Some(p)
                       } else None
                     case _ => None}
                 case _ => Nil //TODO
               }
             case None => Nil
            }
           case _ => Nil //TODO
          }
        case _ => Nil //TODO
     }
  }
  def patternCheck(constants : List[Constant], pattern : Pattern) : Option[Substitution] = {    
    val bodyList = pattern.body.toList    
    if (constants.length == bodyList.length) {
      val mat = new Matcher(controller,pattern.body)
      constants.zip(bodyList).forall {
        case (con,decl) => mat(con.tp,decl.tp,Context()) && mat(con.df,decl.df,Context())                      
      }
      mat.metaContext.toSubstitution
    } else None //Fail: Wrong number of declarations in pattern or number of constants               
  }  
  def apply(e: ContentElement) : Unit = e match {
     case c: Constant =>
       val patts = getPatterns(c.home)(1)
       patts.mapPartial(p => patternCheck(List(c),p))
     case _ => 
   }
}

class Matcher(controller : Controller, var metaContext : Context) {
  def apply(dterm : Term, pterm : Term, con : Context = Context()) : Boolean = true //TODO
  def apply(dterm : Option[Term], pterm : Option[Term], con : Context) : Boolean = true //TODO
}
