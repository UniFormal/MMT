package info.kwarc.mmt.api.patterns

import info.kwarc.mmt.api._
import libraries._
import modules._
import objects._
import objects.Conversions._
import symbols._
import presentation._
import frontend._
import utils._


class PatternAssignment(val home : Term, val name : LocalName, val target : PatternExpression) extends Assignment {
   def toNode = <pattern name={name.toPath}>{getMetaDataNode}{target.toOBJNode}</pattern>
   override def toString = name + " |-> " + target.toString 
   def components = List(StringLiteral(name.toPath)) //TODO give target as argument 
   def role = info.kwarc.mmt.api.Role_PatAss
}

object Functor {
  def applyFunctor(view : View, theo : DeclaredTheory, controller : Controller): DeclaredTheory = {       
    val theoExp = OMA(OMS(mmt.functorapplication),List(OMMOD(view.path),OMMOD(theo.path)))
    val mp = theoExp.toMPath
    val outputTheory =  new DeclaredTheory(mp.parent,mp.name,None)
    //controller.add(Include(OMMOD(mp),view.to)) TODO: fix
    controller.add(outputTheory)
    //val decls = lib.get(view.path)
    val decls = theo.getPrimitiveDeclarations
    //TODO the meta-theory of theo has to be the domain of view
    decls.map {
    //case p : Pattern => TODO
    //case c : Constant => TODO
    case inst : Instance => 
       val pat = controller.globalLookup.getPattern(inst.pattern)
       controller.globalLookup.getO(view.path ? pat.name) match {
         case Some(a : PatternAssignment) =>
           val subsT = inst.matches.map {
             case t => OMM(t,OMMOD(view.path))            
           }
           a.target match {
             case PatternSym(p) => 
                  //val iTE = new InstanceElaborator(controller)
                  val pt = controller.globalLookup.getPattern(p)         
                  val lpair = pt.body.map {d => (d.name,d.name / OMID(inst.home % (inst.name / d.name)))} //TODO Check c.c1
        	       val names = lpair.unzip._1
        	       val subs = lpair.unzip._2  
                  def auxSub(x : Term) = {
        		      x ^ (pt.getSubstitution(inst) ++ Substitution(subs : _*))  
                  }
        	       pt.body.map {
     		   case VarDecl(n,tp,df,at @ _*) =>
        		   val nname = inst.name / n     			
        		   val c = Constant(theoExp,nname,None,tp.map(auxSub),df.map(auxSub),None,None)
        		   controller.add(c)     		   
                  /*	
        			c.setOrigin(InstanceElaboration(inst.path)) //TODO Check InstanceElaboration
        			cont(c)
        	        */
        	
     	       }        	       
           }       
         //case Some(t) => null//TODO Error
         //case None => null//TODO Error           
       }
    }
    outputTheory
  }

  def transform(view : View, theo : Theory, lib : Lookup): View = {
    null
  }
}