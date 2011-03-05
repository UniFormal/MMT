package info.kwarc.mmt.api.objects
import jomdoc._
import jomdoc.utils._
import jomdoc.presentation._

import Context._
import Substitution._

/** represents an MMT variable declaration */
abstract class VarDecl(val name : String) extends Content {
   def ^(sub : Substitution) : VarDecl
   def toOMATTR : Term
   def toNodeID(pos : Position) : scala.xml.Node
   def toNode : scala.xml.Node = toNodeID(Position.None)
}

//TODO: add optional notation
/** represents an MMT term variable declaration
 * @param n name
 * @param tp optional type
 * @param df optional definiens
 * @param attrs list of additional attributions, starting with the innermost
 */
case class TermVarDecl(n : String, tp : Option[Term], df : Option[Term], attrs : (OMS, Term)*) extends VarDecl(n) {
   def ^(sub : Substitution) = TermVarDecl(name, tp.map(_ ^ sub), df.map(_ ^ sub))
   def role = Role_Variable
   def components = List(StringLiteral(n), tp.getOrElse(Omitted), df.getOrElse(Omitted), varToOMATTR)
   def presentation(lpar : LocalParams) =
     ByNotation(NotationKey(None, role), components, lpar)
   /** converts to an OpenMath-style attributed variable using two special keys */
   def varToOMATTR = attrs.toList.foldLeft[Term](OMV(n)) {case (v, (key,value)) => OMATTR(v, key, value)}
   def toOMATTR : Term = {
      (tp, df) match {
         case (None, None) => varToOMATTR
         case (Some(t), None) => OMATTR(varToOMATTR, mmt.mmttype, t)
         case (None, Some(d)) => OMATTR(varToOMATTR, mmt.mmtdef, d)
         case (Some(t), Some(d)) => OMATTR(OMATTR(varToOMATTR, mmt.mmttype, t), mmt.mmtdef, d)
      }
   }
   override def toString = varToOMATTR.toString + tp.map(" : " + _.toString).getOrElse("") + df.map(" = " + _.toString).getOrElse("")  
                           
   def toNodeID(pos : Position) = toOMATTR.toNodeID(pos)
}

/** helper object */
object TermVarDecl {
   /** converts OpenMath-style attributed variable to MMT term variable */
   def fromTerm(t : Term) : TermVarDecl = {
      t match {
         case OMATTR(OMATTR(v, mmt.mmttype, t), mmt.mmtdef, d) => doVar(v, Some(t), Some(d))
         case OMATTR(OMATTR(v, mmt.mmtdef, d), mmt.mmttype, t) => doVar(v, Some(t), Some(d))
         case OMATTR(v, mmt.mmttype, t) => doVar(v, Some(t), None)
         case OMATTR(v, mmt.mmtdef, d) => doVar(v, None, Some(d))
         case v => doVar(v, None, None)
      }
   }
   private def doVar(v : Term, tp : Option[Term], df : Option[Term], attrs : (OMS,Term)*) : TermVarDecl = {
      v match {
         case OMV(n) => TermVarDecl(n, tp, df, attrs : _*)
         case OMATTR(tm, key, value) => doVar(tm, tp, df, (key, value) :: attrs.toList : _*)
         case v => throw new ObjError(v + " is not an MMT variable")
      }
      
   }
}

/** represents an MMT context as a list of variable declarations */
case class Context(variables : VarDecl*) {
   /** add variable at the end */
   def ++(v : VarDecl) : Context = this ++ Context(v)
   /** concatenate contexts */
   def ++(that : Context) : Context = this ::: that
   /** look up a variable by name, throws LookupError if not declared */
   def apply(name : String) : VarDecl = {
      variables.reverse.find(_.name == name).getOrElse(throw LookupError(name))
   }
   def isDeclared(name : String) = this.exists(_.name == name)
   /** returns the de Bruijn index of a variable, starting from 0, throws LookupError if none */
   def index(name: String): Int = variables.lastIndexWhere(_.name == name) match { 
	   case -1 => throw LookupError(name)
	   case i => variables.length - i - 1 
   }
   val id : Substitution = this.map(v => Sub(v.name,OMV(v.name)))  
   override def toString = this.map(_.toString).mkString("",", ","")
   def toNode = toNodeID(Position.None)
   def toNodeID(pos : Position) =
     <om:OMBVAR>{this.zipWithIndex.map({case (v,i) => v.toNodeID(pos + (i+1))})}</om:OMBVAR>
}

/** a case in a substitution */
case class Sub(name: String, target: Term)
/** substitution between two contexts */
case class Substitution(subs : Sub*) {
   def ++(n:String, t:Term) : Substitution = this ++ Sub(n,t)
   def ++(s: Sub) : Substitution = this ++ Substitution(s)
   def ++(that: Substitution) : Substitution = this ::: that
   def ^(sub : Substitution) = this map {case Sub(v,t) => Sub(v, t ^ sub)}
   def apply(v : String) : Term = subs.reverse.find(_.name == v) match {
	   case None => throw SubstitutionUndefined(v)
	   case Some(s) => s.target 
   }
}

/** helper object */
object Context {
   /** converts a list of OpenMath-style attributed variable to a context */
   def fromTerms(l : List[Term]) = Context(l.map(TermVarDecl.fromTerm) : _*)
   /** implicit conversion between a context and a list of variable declarations */
   implicit def list2context(l : List[VarDecl]) : Context = Context(l : _*)
   implicit def context2list(c: Context) : List[VarDecl] = c.variables.toList
}
object Substitution {
   /** implicit conversion between a substitution and a list of maps */
   implicit def string2OMV(s: String) : OMV = OMV(s)
   implicit def list2substitution(l : List[Sub]) : Substitution = Substitution(l:_*)
   implicit def substitution2list(s: Substitution) : List[Sub] = s.subs.toList
}