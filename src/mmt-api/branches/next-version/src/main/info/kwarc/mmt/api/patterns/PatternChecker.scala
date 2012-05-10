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
    if (constants.length == pattern.body.length) {
      val mat = new Matcher(controller,pattern.body)
      var sub = Substitution()
      constants.zip(pattern.body).forall {
        case (con,decl) =>
          val dtype = decl.tp.map(t => t ^ sub)
          val ddef = decl.df.map(d => d ^ sub)
          sub ++ Sub(con.name,decl.name)
          mat(con.tp,dtype,Context()) && mat(con.df,ddef,Context())          
      }
      mat.metaContext.toSubstitution 
    } else None //Fail: Wrong number of declarations in pattern or number of constants               
  }  
  def apply(e: StructuralElement)(implicit cont: StructuralElement => Unit) : Unit = e match {
     case c: Constant =>
       val patts = getPatterns(c.home)(1)
       patts.mapPartial(p => patternCheck(List(c),p))
     case _ => 
   }
}

class Matcher(controller : Controller, var metaContext : Context) {
  def apply(dterm : Term, pterm : Term, con : Context = Context()) : Boolean = {
    
    /* list of OM terms to check:
     * 		+ OMV vars
     * 		+ OMA application
     *     	+ OMBIND binding (w/out condition) 
     *     	+ OMI integers
     *     OMS symbols
     *     
    */
    
    (dterm,pterm) match {
      
      case (OMI(i),OMI(j)) => i == j                   
      case (OMV(v),OMV(w)) => con.isDeclared(v) && con.isDeclared(w) && v == w
 
      
      case (OMA(f, argsf : List[Term]),OMA(h, argsh : List[Term])) => 
        apply(f, h, con) && 
        argsf.zip(argsh).forall{ 
            case (x,y) => apply(x ,y , con) 
        }
        
      case (OMBIND(b1, ctx1, bod1), OMBIND(b2, ctx2, bod2)) => 
         apply(b1,b2,con) && apply(bod1,bod2, con ++ ctx1 ++ ctx2)
         
      case (OMS(a),OMS(b)) => apply(OMID(a),OMID(b),con)
            
      case (OMID(a), OMID(b)) => a.toString() == b.toString()
         
      case (_,_) => false      
    }
    
  true //TODO  
  
  }
    
  def apply(dterm : Option[Term], pterm : Option[Term], con : Context) : Boolean = {
    (dterm,pterm) match {
      case (Some(d),Some(p)) => apply(d,p,con)
      case (None,None) => true
      case (_,_) => false
    }
  }
}

object Test  {
  
  // just a random test file with THF theory
  val testfile = "/home/aivaras/TPTP/tptp/compiled/Problems/AGT/AGT031^2.omdoc"
    
    
    
  
    //TODO 
    /* parse omdoc file
     * should get a list of constants
     * check constants one by one - thf can only have one declaration anyway
     * 
     * check a parsed constant immediatelly against all patterns OR get list of constants and then check
     *  
     */
    
  var reader = new java.io.
    
    

  val tptpbase = DPath(URI("http://latin.omdoc.org/logics/tptp"))
  val baseType = new Pattern(OMID(tptpbase ? "thf"), LocalName("baseType"),Context(), OMV("t") % OMS(tptpbase ? "Types" ? "$tType"))
  val typedCon = new Pattern(OMID(tptpbase ? "thf"), LocalName("typedCon"), OMV("A") % OMS(tptpbase ? "Types" ? "$tType") , OMV("c") % OMA(OMS(tptpbase ? "Types" ? "$tm"), List(OMV("A"))) )
  val axiom = new Pattern(OMID(tptpbase ? "thf"), LocalName("axiom"), OMV("F") % OMA(OMS(tptpbase ? "Types" ? "$tm"),List(OMS(tptpbase ? "THF0" ? "$o"))) , OMV("c") % OMA(OMS(tptpbase ? "Types" ? "$tm"), List(OMV("A"))) )
  val controller = new Controller
  controller.handleLine("file pattern-test.mmt")
  controller.add(baseType)
  controller.add(typedCon)
  controller.add(axiom)
  
  def main(args : Array[String]) {
    
    val thrName : String = args.first
    
    val pc = new PatternChecker(controller)
    pc.patternCheck()
    
//    log("works!")
    
  }
    
}
























