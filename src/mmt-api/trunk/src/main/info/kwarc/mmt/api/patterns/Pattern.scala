package info.kwarc.mmt.api.patterns
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects.Conversions._
import info.kwarc.mmt.api.utils._
import scala.io.Source


class Pattern(val home: TheoryObj, val name : LocalName, val params: Option[Context], val con : Context) extends Symbol {
   def toNode =
     <pattern name={name.flat}>
   		{params match {
   			case Some(c) => <parameters>{c.toNode}</parameters>
   			case None => Nil}}
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
     matches.toNode
     </instance>
   def role = info.kwarc.mmt.api.Role_Instance
   def components = Nil
   override def toString = 
     "Instance " + name.flat + " of pattern " + pattern.toString  
}

object Instance {
  def validInstance(inst: Instance, lib: Lookup) : Boolean = {
	  val pt : Pattern = lib.getPattern(inst.pattern)
	  pt.params match {
	 	  case Some(p) => if (p.length == inst.matches.length) true else false
	 	  case None => inst.matches match {
	 	 	  case Some(_) => false
	 	 	  case None => true
	 	  }
	  }
  }
  def elaborate(inst: Instance, lib: Lookup): List[Constant] = {  
    if (validInstance(inst,lib))  
    	val pt : Pattern = lib.getPattern(inst.pattern) //TODO There are two getPattern methods now, one should be eliminated.
        pt.con.map {
    	  case TermVarDecl(n,tp,df,at) => 
            def auxSub(x : Term) = {
        	val names = pt.con.map(d => d.name)
        	(x ^ inst.matches) ^ Substitution(names.map(y => TermSub(y,OMID(inst.home % (inst.name / y)))) : _*)
            }
    	    new Constant(inst.home, inst.name / n, tp.map(auxSub), df.map(auxSub),null)
          case SeqVarDecl(n,tp,df) => //TODO  
    }
    else Nil
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