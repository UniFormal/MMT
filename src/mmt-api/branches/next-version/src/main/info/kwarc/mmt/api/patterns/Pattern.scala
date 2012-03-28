package info.kwarc.mmt.api.patterns
import info.kwarc.mmt.api._
import libraries._
import modules._
import objects._
import objects.Conversions._
import symbols._
import frontend._
import utils._
import scala.io.Source

              // TODO term
class Pattern(val home: Term, val name : LocalName, val params: Context, val con : Context) extends Symbol {
   def toNode =
     <pattern name={name.flat}>
   		{if (! params.isEmpty)
   		   <parameters>{params.toNode}</parameters>
   		else Nil}
   	   <declarations>{con.toNode}</declarations>
     </pattern>    
   def role = info.kwarc.mmt.api.Role_Pattern
   override def compNames : List[(String,Int)] = List(("paramsBegin",1),("paramsEnd",params.length),("conBegin",params.length + 1)) 
   def components = OMID(path) :: params ::: con
   override def toString = 
     "Pattern for " + name.flat + " " + params.toString + " " + con.toString
}
                     // TODO term
class Instance(val home : Term, val name : LocalName, val pattern : GlobalName, val matches : Substitution) extends Symbol {
   def toNode = 
     <instance name={name.flat} pattern={pattern.toPath}>
     {matches.toNode}
     </instance>
   def role = info.kwarc.mmt.api.Role_Instance
   def components = List(OMID(path), OMID(pattern)) ::: matches
   override def toString = 
     "Instance " + name.flat + " of pattern " + pattern.toString  
}

/*
object Instance {
  /**
   * returns the elaboration of an instance
   */
  def elaborate(inst: Instance, normalize: Boolean)(implicit lup: Lookup, report: Report): List[Constant] = {  
    	val pt : Pattern = lup.getPattern(inst.pattern)
      pt.con.map {
    	  case TermVarDecl(n,tp,df,at @ _*) =>
          def auxSub(x : Term) = {
     	        val names = pt.con.map(d => d.name)
     	        val subs = pt.con map {d => d.name / OMID(inst.home % (inst.name / d.name))}
      	     val xsub = x ^ (inst.matches ++ Substitution(subs : _*))
      	     if (normalize) SeqNormalize.normalizeTerm(xsub) else xsub
          }
          val nname = inst.name / n
          report("elaboration", "generating constant " + nname)
    	    val c = new Constant(inst.home, nname, tp.map(auxSub), df.map(auxSub),None, None)
    	    c.setOrigin(InstanceElaboration(inst.path))
    	    c
        case SeqVarDecl(n,tp,df, at @ _*) => throw ImplementationError("Pattern cannot contain sequence variable declaration")
      }
  }
  
  /**
   * elaborates all instances in a theory and inserts the elaborated constants after the respective instance
   */
  def elaborate(thy: DeclaredTheory, normalize: Boolean = false)(implicit lup: Lookup, report: Report) {
     thy.valueList foreach {
        case i: Instance =>
           i.setOrigin(Elaborated)
           thy.replace(i.name, i :: elaborate(i, true) : _*)
        case _ => 
     }
  }
  
}
*/

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