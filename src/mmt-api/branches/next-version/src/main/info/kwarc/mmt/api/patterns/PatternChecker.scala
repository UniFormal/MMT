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
      val mat = new PatternMatcher(controller,pattern.params)
      var sub = Substitution()
      constants.zip(pattern.body).forall {
        case (con,decl) =>
          val dtype = decl.tp.map(t => t ^ sub)
          val ddef = decl.df.map(d => d ^ sub)
          sub ++ Sub(con.name,decl.name)      
//          println("matching " + con.tp.toString + con.df.toString +  " with " + dtype.toString + ddef.toString)
          val res1 = mat(con.tp,dtype,Context())
          val res2 = mat(con.df,ddef,Context())
          val res = res1 && res2
          println(res1.toString + "  " + res2.toString)// just to get a prompt in the shell          
          res
        
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


class PatternMatcher(controller : Controller, var metaContext : Context) {  
  def apply(dterm : Term, pterm : Term, con : Context = Context()) : Boolean = {// why do we bother with context here?    
    //if (lengthChecker(dterm,pterm)) {          
//    println("matching " + dterm.toString() + " with " + pterm.toString())    
        (dterm,pterm) match {
        	case (OMID(a), OMID(b)) => a.toString == b.toString
        	case (OMI(i),OMI(j)) => i == j                   
            case (OMV(v),OMV(w)) if (v == w) => con.isDeclared(v) && con.isDeclared(w) 
            case (OMA(f1,args1),OMA(f2,args2)) => 
               apply(f1,f2,con) && args1.zip(args2).forall { 
                  case (x,y) => apply(x,y,con) 
               }
            case (OMS(a),OMS(b)) => apply(OMID(a),OMID(b), con)                  
            // OMBIND case gone, due Florian??
            case (OMBINDC(b1, ctx1, cond1, bod1), OMBINDC(b2,ctx2,cond2,bod2)) => apply(b1,b2,con) && apply(cond1,cond2,con ++ ctx1) && apply(bod1,bod2,con ++ ctx1)
// a missing case:  ??
            case (OMV(v), _) => metaContext.isDeclared(v)
//            							metaContext.++() // add v = anyT as definien to the metaContext, also true
//            						else false
            // check if constant and variable types are the same                        
            case (OMS(s),OMV(v)) => {
              val const = controller.globalLookup.getConstant(s)
              val vardecs = metaContext.components
              val vardec = vardecs.find(x => x.name == v)
              vardec match {                
                case Some(v) => (v.tp,const.tp) match {
                  	case (None,None) => true 
                  	case (Some(a),Some(b)) => a == b
                  	case (_,_) => false
                }
                case _ => false
              }              
            }
            // missing:  OMA, OMBIND, OMI ...   to OMV
            case (t, OMV(v)) => {                
              metaContext.isDeclared(v)
            }
            case (_,_) => false      
        }
    //}
  }
    
  def apply(dterm : Option[Term], pterm : Option[Term], con : Context) : Boolean = {
    (dterm,pterm) match {
      case (Some(d),Some(p)) => apply(d,p,con)
      case (None,None) => true
      case (_,_) => false
    }
  }
  
  def lengthChecker(term1 : Term, term2 : Term) : Boolean = {
    true
     //val len1N = normalize(length(term1))
     //val len2N = normalize(length(term2))
    /*
     (term1,len2N) match {
       case (OMI(m),OMI(n)) if (m == n) => true
       case (_,_) => 
         freeVars(len1N)
         freeVars(len1N)
         lengthSolver()
     }
     */
  }
}


// a test run with THF (THF0) patterns
object PatternTest  {  
  //TODO 
     // read omdoc file content --> register archive and call content command    
     // should get a list of constants
     // check constants one by one - thf can only have one declaration anyway     
     // check a parsed constant immediatelly against all patterns OR get list of constants and then check      
     
  val tptpbase = DPath(URI("http://latin.omdoc.org/logics/tptp"))
  val pbbase = DPath(URI("http://oaff.omdoc.org/tptp/problems"))// problem base

  val baseType = new Pattern(OMID(tptpbase ? "THF0"), LocalName("baseType"),Context(), OMV("t") % OMS(tptpbase ? "Types" ? "$tType"))
  val typedCon = new Pattern(OMID(tptpbase ? "THF0"), LocalName("typedCon"), OMV("A") % OMS(tptpbase ? "Types" ? "$tType") , OMV("c") % OMA(OMS(tptpbase ? "Types" ? "$tm"), List(OMV("A"))) )
  val axiom = new Pattern(OMID(tptpbase ? "THF0"), LocalName("axiom"), OMV("F") % OMA(OMS(tptpbase ? "Types" ? "$tm"),List(OMS(tptpbase ? "THF0" ? "$o"))) , OMV("c") % OMA(OMS(tptpbase ? "Types" ? "$istrue"), List(OMV("F"))) )
  val typedConDef = new Pattern(OMID(tptpbase ? "THF0"), LocalName("typedConDef"), OMV("A") % OMS(tptpbase ? "Types" ? "$tType") ++ OMV("D") % OMA(OMS(tptpbase ? "Types" ? "$tm"), List(OMV("A"))), VarDecl(LocalName("c"),Some(OMA(OMS(tptpbase ? "Types" ? "$tm"),List(OMV("A")))),Some(OMV("D"))))
  val theorem = new Pattern(OMID(tptpbase ? "THF0"), LocalName("theorem"), OMV("F") % OMA(OMS(tptpbase ? "Types" ? "$tm"),List(OMS(tptpbase ? "THF0" ? "$o"))) ++ OMV("D") % OMA(OMS(tptpbase ? "Types" ? "$tm"),List(OMS(tptpbase ? "THF0" ? "$o"))), VarDecl(LocalName("c"),Some(OMA(OMS(tptpbase ? "Types" ? "$istrue"), List(OMV("F")))),Some(OMV("D"))))
  val controller = new Controller
  controller.handleLine("file pattern-test.mmt")// run what's written in this file first - add logs, archives etc.
  controller.add(baseType)
  controller.add(typedCon)
  controller.add(axiom)
  controller.add(typedConDef)
  controller.add(theorem)
    
  case class Error(msg : String) extends java.lang.Throwable(msg)  
  
  def main(args : Array[String]) {  
    val pc = new PatternChecker(controller)        														
//    val testget = controller.globalLookup.getStructure(pbbase / "SomeProblem.omdoc" ? "SomeProblem")
    														// file name ? theory name ? constant name 
    
    // get list of constant declarations
//    val conMu = try { 
//      controller.globalLookup.getConstant(pbbase  ? "AGT027^1" ? "mu")
//    } catch {
////      case GetError(m) => GetError//TODO : deal with the error  
//      case e => e.printStackTrace()
//    }
    val constTheory = controller.globalLookup.getTheory(pbbase  ? "AGT027^1") match {
      case c : DeclaredTheory => c
      case _ => throw Error("no constants in " + pbbase + "?" + "AGT027^1")
    }
    val constList : List[Constant] = constTheory.valueListNG mapPartial {
      case p : Constant => Some(p)
      case _ => None
    }
  
    
    //     get list of patterns from controller
    val pp = try {
      controller.globalLookup.getPattern(tptpbase ? "THF0" ? "baseType")
    } catch {
//      case GetError(m) => throw GetError(m)
      case e => e.printStackTrace()
    }
    val pattTheory = controller.globalLookup.getTheory(tptpbase ? "THF0") match {
      case t : DeclaredTheory => t
      case _ => throw Error("no patterns in " + tptpbase + "?" + "THF0")      
    }        
    val pattList : List[Pattern] = pattTheory.valueListNG mapPartial {
      case p : Pattern => Some(p)
      case _ => None
    }
        
    
    //     <------------------------ pattern checking happens here  ------------------------------->
    // for each constant declaration check with each pattern declaration
    val matches = constList.foreach {
        a => println(a.name.toString) 
          pattList.foreach {
          p => pc.patternCheck(List(a),p) 
        }
      }
    
//      val testCon = controller.globalLookup.getConstant(pbbase  ? "SomeProblem" ? "meq_ind")
//    
//      pc.patternCheck(List(testCon), typedConDef)
//      pc.patternCheck(List(testCon), theorem)
//      pc.patternCheck(List(testCon), typedCon) 
//      pc.patternCheck(List(testCon), axiom) 
    
  }
  
    
}
























