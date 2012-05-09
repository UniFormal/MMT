package info.kwarc.mmt.api.patterns

import info.kwarc.mmt.api._
import libraries._
import frontend._
import symbols._
import modules._
import objects._
import objects.Conversions._
import utils._
import scala.io.Source

/** elaborates Instance declarations
 * this is also called the pragmatic-to-strict translation
 */
class InstanceElaborator(controller: Controller) extends Elaborator {
   val report = controller.report
   /**
   * returns the elaboration of an instance
   */
   def apply(e: StructuralElement)(implicit cont: StructuralElement => Unit) {e match {
     case inst : Instance => 
     	val pt : Pattern = controller.globalLookup.getPattern(inst.pattern)
     	val lpair = pt.body.map {d => (d.name,d.name / OMID(inst.home % (inst.name / d.name)))} //TODO Check c.c1
     	val names = lpair.unzip._1
     	val subs = lpair.unzip._2  
        def auxSub(x : Term) = {
     		x ^ (inst.matches ++ Substitution(subs : _*))  
     	}
     	pt.body.map {
     		case VarDecl(n,tp,df,at @ _*) =>
     			val nname = inst.name / n
     			report("elaboration", "generating constant " + nname)
     			val c = new Constant(inst.home,nname,tp.map(auxSub),df.map(auxSub),None,None)
     			c.setOrigin(InstanceElaboration(inst.path)) //TODO Check InstanceElaboration
     			cont(c)
     	} 
     case _ => ()
   }
  }
  
  /**
   * elaborates all instances in a theory and inserts the elaborated constants into the containing theory
   */
  def elaborate(thy: DeclaredTheory) {
     thy.valueList foreach {
        case i : Instance =>
           i.setOrigin(Elaborated)
           apply(i) {
              case s: Symbol => thy.add(s)
              case _ => //does not occur
           }
        case c @ _ => c 
     }
  }
}