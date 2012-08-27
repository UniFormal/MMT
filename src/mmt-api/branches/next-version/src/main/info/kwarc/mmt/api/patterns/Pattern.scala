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

/*
 * A Pattern represents a declaration of a "declaration pattern".
 * 
 * @param home   the link to the containing theory
 * @param name   the name of the declaration pattern
 * @param params the parameters of the declaration patterns
 * @param body   the body of the declaration pattern that consists of symbol declarations             
 */
class Pattern(val home: Term, val name : LocalName, val params: Context, val body : Context) extends Symbol {
   def toNode =
     <pattern name={name.toPath}>
   		{if (! params.isEmpty)
   		   <parameters>{params.toNode}</parameters>
   		else Nil}
   	   <declarations>{body.toNode}</declarations>
     </pattern>    
   def role = info.kwarc.mmt.api.Role_Pattern
   override def compNames : List[(String,Int)] = List(("paramsBegin",1),("paramsEnd",params.length),("conBegin",params.length + 1)) 
   def components = OMID(path) :: params ::: body
   override def toString = 
     "Pattern for " + name.toString + {if (params.variables.toList.isEmpty) "" else  {" [ " + params.toString + " ]" }} + " { " + body.toString + " }"
}

sealed abstract class PatternExpression {
  def toOBJNode = toString //TODO case by case in the case classes
}

case class PatternSym(path : GlobalName) extends PatternExpression 
case class PatternAbs(params : Context, body : PatternExpression) extends PatternExpression
case class PatternApp(pattern : PatternExpression, args : Substitution) extends PatternExpression
case class PatternFrag(body : Context) extends PatternExpression


class Instance(val home : Term, val name : LocalName, val pattern : GlobalName, val matches : Substitution) extends Symbol {
   def toNode = 
     <instance name={name.toPath} pattern={pattern.toPath}>
     {matches.toNode}
     </instance>
   def role = info.kwarc.mmt.api.Role_Instance
   def components = List(OMID(path), OMID(pattern)) ::: matches
   override def toString = 
     "Instance " + name.toString + " of pattern " + pattern.toString  
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