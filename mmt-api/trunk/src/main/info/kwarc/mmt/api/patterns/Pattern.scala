package info.kwarc.mmt.api.patterns
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.objects.Conversions._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.utils._
import scala.io.Source


class Pattern(val home: TheoryObj, val name : LocalName, val params: Context, val con : Context) extends Symbol {
   def toNode =
     <pattern name={name.flat}>
   		{if (! params.isEmpty)
   		   <parameters>{params.toNode}</parameters>
   		else Nil}
   	   <declarations>{con.toNode}</declarations>
     </pattern>    
   def role = info.kwarc.mmt.api.Role_Pattern
   def components = Nil
   override def toString = 
     "Pattern for " + name.flat + " " + params.toString + " " + con.toString
}

class Instance(val home : TheoryObj, val name : LocalName, val pattern : GlobalName, val matches : Substitution) extends Symbol {
   def toNode = 
     <instance name={name.flat} pattern={pattern.toPath}>
     {matches.toNode}
     </instance>
   def role = info.kwarc.mmt.api.Role_Instance
   def components = Nil
   override def toString = 
     "Instance " + name.flat + " of pattern " + pattern.toString  
}

object Instance {
  /**
   * returns the elaboration of an instance
   */
  def elaborate(inst: Instance, normalize: Boolean = false)(implicit lup: Lookup): List[Constant] = {  
    	val pt : Pattern = lup.getPattern(inst.pattern)
      pt.con.map {
    	  case TermVarDecl(n,tp,df,at @ _*) =>
              def auxSub(x : Term) = {
        	        val names = pt.con.map(d => d.name)
        	        val subs = pt.con map {d => d.name / OMID(inst.home % (inst.name / d.name))}
         	     val xsub = (x ^ inst.matches) ^ Substitution(subs : _*)
         	     if (normalize) SeqNormalize.normalizeTerm(xsub) else xsub
            }
    	    val c = new Constant(inst.home, inst.name / n, tp.map(auxSub), df.map(auxSub),null)
    	    c.setOrigin(InstanceElaboration(inst.path))
    	    c
        case SeqVarDecl(n,tp,df, at @ _*) => throw ImplementationError("Pattern cannot contain sequence variable declaration")
      }
  }
  
  /**
   * elaborates all instances in a theory and inserts the elaborated constants after the respective instance
   */
  def elaborate(thy: DeclaredTheory)(implicit lup: Lookup) {
     thy.valueList foreach {
        case i: Instance =>
           i.setOrigin(Elaborated)
           thy.replace(i.name, i :: elaborate(i) : _*)
        case _ => 
     }
  }
  
}

case object NoMatch extends java.lang.Throwable 


//val home : TheoryObj, val name : LocalName, val pattern : GlobalName, val matches : Substitution
/*
object Test {
	val ex = DPath(new xml.URI("http", "cds.omdoc.org", "/logics/first-order/syntax/sfol.omdoc", null)) ? "SFOL"
	val fn = Instance(OMMOD(sfol ? " LocalName("binary"))
	val v1 = OMV("x")
	def main(args : Array[String]) {
		print(Pattern.elaborate().toString)
	}
}
*/