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
class Pattern(val home: Term, val name : LocalName, val params: Context, val body : Context) extends Symbol {
   def toNode =
     <pattern name={name.flat}>
   		{if (! params.isEmpty)
   		   <parameters>{params.toNode}</parameters>
   		else Nil}
   	   <declarations>{body.toNode}</declarations>
     </pattern>    
   def role = info.kwarc.mmt.api.Role_Pattern
   override def compNames : List[(String,Int)] = List(("paramsBegin",1),("paramsEnd",params.length),("conBegin",params.length + 1)) 
   def components = OMID(path) :: params ::: body
   override def toString = 
     "Pattern for " + name.flat + " " + params.toString + " " + body.toString
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
object Test {
	val ex = DPath(new xml.URI("http", "cds.omdoc.org", "/logics/first-order/syntax/sfol.omdoc", null)) ? "SFOL"
	val fn = Instance(OMMOD(sfol ? " LocalName("binary"))
	val v1 = OMV("x")
	def main(args : Array[String]) {
		print(Pattern.elaborate().toString)
	}
}
*/