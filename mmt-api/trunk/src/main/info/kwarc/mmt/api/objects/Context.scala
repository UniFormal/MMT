package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
import utils._
import presentation._
import Conversions._

/** represents an MMT variable declaration */
sealed abstract class VarDecl extends Content {
   val name : String
   val tp : Option[Obj]
   val df : Option[Obj]
   def ^(sub : Substitution) : VarDecl
   /** converts to an OMV for OpenMath export */
   def toOpenMath : Term
   def toNode : scala.xml.Node = toNodeID(Position.None)
   def toNodeID(pos : Position) = toOpenMath.toNodeID(pos)
   def role : Role
   def components = List(StringLiteral(name), tp.getOrElse(Omitted), df.getOrElse(Omitted))
   def presentation(lpar : LocalParams) =
	   ByNotation(NotationKey(None, role), components, lpar)
   override def toString = name.toString + tp.map(" : " + _.toString).getOrElse("") + df.map(" = " + _.toString).getOrElse("")  
}

//TODO: add optional notation
/** represents an MMT term variable declaration
 * @param n name
 * @param tp optional type
 * @param df optional definiens
 * @param attrs OpenMath-style attributions (will be ignored)
 */
case class TermVarDecl(name : String, tp : Option[Term], df : Option[Term], attrs: (OMID,Term)*) extends VarDecl {
   def ^(sub : Substitution) = TermVarDecl(name, tp.map(_ ^ sub), df.map(_ ^ sub))
   /** converts to an OpenMath-style attributed variable using two special keys */
   def toOpenMath : Term = {
	  val varToOMATTR = OMV(name) // attrs.toList.foldLeft[Term](toOpenMath) {(v,a) => OMATTR(v, a._1, a._2)}
      (tp, df) match {
         case (None, None) => varToOMATTR
         case (Some(t), None) => OMATTR(varToOMATTR, OMID(mmt.mmttype), t)
         case (None, Some(d)) => OMATTR(varToOMATTR, OMID(mmt.mmtdef), d)
         case (Some(t), Some(d)) => OMATTR(OMATTR(varToOMATTR, OMID(mmt.mmttype), t), OMID(mmt.mmtdef), d)
      }
   }
   def role = Role_Variable
}

case class SeqVarDecl(name : String, tp : Option[Sequence], df : Option[Sequence]) extends VarDecl {
   def ^(sub : Substitution) = SeqVarDecl(name, tp.map(_ ^ sub), df.map(_ ^ sub))
   /** converts to an OpenMath-style attributed variable using two special keys */
   def toOpenMath : Term = {
	  val varToOMATTR = OMV(name) // attrs.toList.foldLeft[Term](toOpenMath) {(v,a) => OMATTR(v, a._1, a._2)}
      (tp, df) match {
         case (None, None) => varToOMATTR
         case (Some(t), None) => OMATTR(varToOMATTR, OMID(mmt.mmttype), t.toOpenMath)
         case (None, Some(d)) => OMATTR(varToOMATTR, OMID(mmt.mmtdef), d.toOpenMath)
         case (Some(t), Some(d)) => OMATTR(OMATTR(varToOMATTR, OMID(mmt.mmttype), t.toOpenMath), OMID(mmt.mmtdef), d.toOpenMath)
      }
   }
   def role = Role_SeqVariable
}

/** helper object */
object TermVarDecl {
   /** converts OpenMath-style attributed variable to MMT term variable */
   def fromTerm(t : Term) : TermVarDecl = {
      t match {
         case OMATTR(OMATTR(v, OMID(mmt.mmttype), t), OMID(mmt.mmtdef), d) => doVar(v, Some(t), Some(d))
         case OMATTR(OMATTR(v, OMID(mmt.mmtdef), d), OMID(mmt.mmttype), t) => doVar(v, Some(t), Some(d))
         case OMATTR(v, OMID(mmt.mmttype), t) => doVar(v, Some(t), None)
         case OMATTR(v, OMID(mmt.mmtdef), d) => doVar(v, None, Some(d))
         case v => doVar(v, None, None)
      }
   }
   // ignoring attributions
   private def doVar(v : Term, tp : Option[Term], df : Option[Term], attrs : (OMID,Term)*) : TermVarDecl = {
      v match {
         case OMV(n) => TermVarDecl(n, tp, df)
         case OMATTR(tm, key, value) => doVar(tm, tp, df)
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
   def id : Substitution = this map {
	   case TermVarDecl(n, _, _, _*) => TermSub(n,OMV(n))
	   case SeqVarDecl(n, _, _) => SeqSub(n,SeqVar(n))
   }
   /** substitutes in all variable declarations except for the previously declared variables
    *  if |- G ++ H  and  |- sub : G -> G'  then  |- G' ++ (H ^ sub)
    */  
   def ^(sub : Substitution) : Context = {
      val id = this.id // precompute value
      // sub ++ id.take(i) represents sub, x_1/x_1, ..., x_{i-1}/x_{i-1} 
      val newvars = variables.zipWithIndex map {case (vd, i) => vd ^ (sub ++ id.take(i))}
      Context(newvars :_*)
   }

   override def toString = this.map(_.toString).mkString("",", ","")
   def toNode = toNodeID(Position.None)
   def toNodeID(pos : Position) =
     <om:OMBVAR>{this.zipWithIndex.map({case (v,i) => v.toNodeID(pos + (i+1))})}</om:OMBVAR>
}

/** a case in a substitution */
sealed abstract class Sub {
	val name: String
	val target: Obj
	def toOMATTR : Term
	def toNodeID(pos: Position ) = toOMATTR.toNodeID(pos)
	override def toString = toOMATTR.toString
}

case class TermSub(name : String, target : Term) extends Sub {
	def toOMATTR = OMATTR(OMV(name), OMID(mmt.mmtdef), target)
}

case class SeqSub(name : String, target : Sequence) extends Sub {
	def toOMATTR = OMATTR(OMV(name), OMID(mmt.mmtdef), target.toOpenMath)
}

/** helper object */
object Sub {
   /** converts OpenMath-style attributed variable to a Sub */
   def fromTerm(t : Term) : Sub = t match {
      case OMATTR(OMV(n), OMID(mmt.mmtdef), t) => TermSub(n, t)
      case _ => throw ObjError(t + " is not a case in a substitution")
   }
}

/** substitution between two contexts */
case class Substitution(subs : Sub*) {
   def ++(n:String, t:Term) : Substitution = this ++ TermSub(n,t)
   def ++(s: Sub) : Substitution = this ++ Substitution(s)
   def ++(that: Substitution) : Substitution = this ::: that
   def ^(sub : Substitution) = this map {
	   case TermSub(v,t) => TermSub(v, t ^ sub)
	   case SeqSub(v,s) => SeqSub(v, s ^ sub)
   }
   def apply(v : String) : Option[Obj] = subs.reverse.find(_.name == v).map(_.target)
   /** turns a substitution into a context by treating every substitute as a definiens
    *  this permits seeing substitution application as a let-binding
    */
   def asContext = {
      val decls = subs map {
    	  case TermSub(n, t) => TermVarDecl(n, None, Some(t))
    	  case SeqSub(n, t) => SeqVarDecl(n, None, Some(t))
      }
      Context(decls : _*)
   }
   override def toString = this.map(_.toString).mkString("",", ","")
   def toNode = toNodeID(Position.None)
   def toNodeID(pos : Position) = asContext.toNodeID(pos)
}

/** helper object */
object Context {
	/** parsers an OMBVAR into a context */
	def parse(N : scala.xml.Node, base : Path, callback : Path => Unit = x => ()) : Context = N match {
		case <om:OMBVAR>{context @ _*}</om:OMBVAR> => fromTerms(context.map(Obj.parseTerm(_, base, callback)).toList)
        case _ => throw ParseError("not a well-formed context: " + N.toString)
	}
   /** converts a list of OpenMath-style attributed variable to a context */
   def fromTerms(l : List[Term]) = Context(l.map(TermVarDecl.fromTerm) : _*)
}
object Substitution {
	/** parsers an OMBVAR into a substitution */
	def parse(N : scala.xml.Node, base : Path, callback : Path => Unit = x => ()) : Substitution = N match {
		case <om:OMBVAR>{sb @ _*}</om:OMBVAR> => fromTerms(sb.map(Obj.parseTerm(_, base, callback)).toList)
        case _ => throw ParseError("not a well-formed substitution: " + N.toString)
	}
   /** converts a list of OpenMath-style attributed variable to a substitution */
   def fromTerms(l : List[Term]) = Substitution(l.map(Sub.fromTerm) : _*)
}