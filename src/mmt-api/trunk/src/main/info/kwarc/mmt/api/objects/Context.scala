package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
import utils._
import presentation._
import Conversions._

import scala.xml.Node

/** represents an MMT variable declaration */
sealed abstract class VarDecl extends Content {
   val name : String
   val tp : Option[Obj]
   val df : Option[Obj]
   val attrs : List[(GlobalName, Obj)]
   def ^(sub : Substitution) : VarDecl
   def toNode : Node = toNodeID(Position.None)
   def toNodeID(pos : Position) : Node
   def toCML : Node
   def role : Role
   def components = List(StringLiteral(name), tp.getOrElse(Omitted), df.getOrElse(Omitted))
   def presentation(lpar : LocalParams) =
	   ByNotation(NotationKey(None, role), ContentComponents(components), lpar)
   override def toString = name.toString + tp.map(" : " + _.toString).getOrElse("") + df.map(" = " + _.toString).getOrElse("")  
}

//TODO: add optional notation
/** represents an MMT term variable declaration
 * @param n name
 * @param tp optional type
 * @param df optional definiens
 * @param attrs OpenMath-style attributions
 */
case class TermVarDecl(name : String, tp : Option[Term], df : Option[Term], ats: (GlobalName,Term)*) extends VarDecl {
   val attrs = ats.toList
   def ^(sub : Substitution) = TermVarDecl(name, tp.map(_ ^ sub), df.map(_ ^ sub))
   /** converts to an OpenMath-style attributed variable using two special keys */
   def toOpenMath : Term = {
	   val varToOMATTR = attrs.toList.foldLeft[Term](OMV(name)) {(v,a) => OMATTR(v, OMID(a._1), a._2)}
      (tp, df) match {
         case (None, None) => varToOMATTR
         case (Some(t), None) => 
           OMATTR(varToOMATTR, OMID(mmt.mmttype), t)
         case (None, Some(d)) => OMATTR(varToOMATTR, OMID(mmt.mmtdef), d)
         case (Some(t), Some(d)) => OMATTR(OMATTR(varToOMATTR, OMID(mmt.mmttype), t), OMID(mmt.mmtdef), d)
      }
   }
   def toNodeID(pos : Position) = toOpenMath.toNodeID(pos) 
   def toCML = toOpenMath.toCML
   def role = Role_Variable
}

case class SeqVarDecl(name : String, tp : Option[Sequence], df : Option[Sequence], ats: (GlobalName,Term)*) extends VarDecl {
   val attrs = ats.toList
   def ^(sub : Substitution) = SeqVarDecl(name, tp.map(_ ^ sub), df.map(_ ^ sub))
   def toNodeID(pos : Position) = {
      val tpN = tp.map(t => <type>{t.toNodeID(pos + 1)}</type>).getOrElse(Nil)
      val dfN = df.map(t => <definition>{t.toNodeID(pos + 2)}</definition>).getOrElse(Nil)
      val attrsN = attrs map {case (path,tm) => <attribution key={path.toPath}>{tm.toNode}</attribution>}
      <om:OMSV name={name}>{tpN}{dfN}{attrsN}</om:OMSV>
   }
   
   def toCML = null
   def role = Role_SeqVariable
}

/** represents an MMT context as a list of variable declarations */
case class Context(variables : VarDecl*) extends Obj {
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
   def toNodeID(pos : Position) =
     <om:OMBVAR>{this.zipWithIndex.map({case (v,i) => v.toNodeID(pos + i)})}</om:OMBVAR>
   def toCML = 
     <m:bvar>{this.map(v => v.toCML)}</m:bvar>
   def head : Option[Path] = None
   def role : Role = Role_context
   def components = variables.toList 
}

/** a case in a substitution */
sealed abstract class Sub extends Content {
	val name: String 
	val target: Obj
	def toNode : Node = toNodeID(Position.None)
	def toNodeID(pos: Position): Node
	override def toString = name + "/" + target.toString
	def role : Role
    def components = List(StringLiteral(name), target)
    def presentation(lpar : LocalParams) =
	   ByNotation(NotationKey(None, role), ContentComponents(components), lpar)
}
case class TermSub(name : String, target : Term) extends Sub {
   def toNodeID(pos: Position): Node = <om:OMV name={name}>{target.toNodeID(pos + 1)}</om:OMV>
   def role : Role = Role_termsub
}
case class SeqSub(name : String, target : Sequence) extends Sub {
   def toNodeID(pos: Position): Node = <om:OMSV name={name}>{target.toNodeID(pos + 1)}</om:OMSV>
   def role : Role = Role_seqsub
}

/** substitution between two contexts */
case class Substitution(subs : Sub*) extends Obj {
   def ++(n:String, t:Term) : Substitution = this ++ TermSub(n,t)
   def ++(s: Sub) : Substitution = this ++ Substitution(s)
   def ++(that: Substitution) : Substitution = this ::: that
   def ^(sub : Substitution) = this map {
	   case TermSub(v,t) => TermSub(v, t ^ sub)
	   case SeqSub(v,s) => SeqSub(v, s ^ sub)
   }
   def apply(v : String) : Option[Obj] = subs.reverse.find(_.name == v).map(_.target)
   def isIdentity : Boolean = subs forall {
      case TermSub(n, OMV(m)) => m == n
      case SeqSub(n, SeqVar(m)) => m == n
      case _ => false 
   }
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
   def toNodeID(pos : Position) =
      <om:OMBVAR>{subs.zipWithIndex.map(x => x._1.toNodeID(pos + x._2))}</om:OMBVAR>
   def toCML = asContext.toCML
   def head : Option[Path] = None
   def role : Role = Role_substitution
   def components = subs.toList
}

/** helper object */
object Context {
	/** parsers an OMBVAR into a context */
	def parse(N : scala.xml.Node, base : Path) : Context = N match {
	  case <om:OMBVAR>{decls @ _*}</om:OMBVAR> =>  decls.toList.map(VarDecl.parse(_, base))
      case _ => throw ParseError("not a well-formed context: " + N.toString)
	}
}
/** helper object */
object VarDecl {
   def parseAttrs[T <: Obj, D <: Obj](N: Seq[Node], base: Path, parseType: Node => T, parseDef : Node => D, parseOther : Node => Term) :
                                                    (Option[T], Option[D], List[(GlobalName,Term)]) = {
      var tp : Option[T] = None
      var df : Option[D] = None
      var attrs : List[(GlobalName, Term)] = Nil
      var left = N.toList
      while (left != Nil) {
         left.head match {
            case <type>{t}</type> => tp = Some(parseType(t))
            case <definition>{t}</definition> => df = Some(parseDef(t))
            case a @ <attribution>{t}</attribution> =>
               val key = Path.parseS(xml.attr(a, "key"), base)
               val value = parseOther(t)
               attrs = (key, value) :: attrs
            case k @ <OMS/> =>
               val key = Obj.parseOMS(k, base) match {
                  case g: GlobalName => g
                  case _ => throw ParseError("key must be symbol in " + N.toString)
               }
               left = left.tail
               if (left == Nil) throw ParseError("missing attribution value: " + N)
               val vl = left.head
               key match {
                  case mmt.mmttype => tp = Some(parseType(vl))
                  case mmt.mmtdef => df = Some(parseDef(vl))
                  case _ =>
                     val value = parseOther(vl)
                     attrs = (key, value) :: attrs
               }
         }
         left = left.tail
      }
      (tp, df, attrs.reverse)
   }
   def parse(N: Node, base: Path) : VarDecl = {
      val pTerm = (x:Node) => Obj.parseTerm(x, base)
      val pSeq = (x:Node) => Obj.parseSequence(x, base)
      N match {      
         case <OMATTR><OMATP>{ats @ _*}</OMATP>{v}</OMATTR> =>
            val name = xml.attr(v, "name")
            val (tp, df, attrs) = parseAttrs(ats, base, pTerm, pTerm, pTerm)
            TermVarDecl(name, tp, df, attrs : _*)
         case <OMV>{ats @ _*}</OMV> =>
            val name = xml.attr(N, "name")
            val (tp, df, attrs) = parseAttrs(ats, base, pTerm, pTerm, pTerm)
            TermVarDecl(name, tp, df, attrs : _*)
         case <OMSV>{ats @ _*}</OMSV> =>
            val name = xml.attr(N, "name")
            val (tp, df, attrs) = parseAttrs(ats, base, pSeq, pSeq, pTerm)
            SeqVarDecl(name, tp, df, attrs : _*)
         case <seqvar>{ats @ _*}</seqvar> =>//TODO remove this case
            val name = xml.attr(N, "name")
            val (tp, df, attrs) = parseAttrs(ats, base, pSeq, pSeq, pTerm)
            SeqVarDecl(name, tp, df, attrs : _*)
         case _ => throw ParseError("not a well-formed variable declaration: " + N.toString)
      }
   }
}
/** helper object */
object Substitution {
	/** parsers an OMBVAR into a substitution */
	def parse(N : scala.xml.Node, base : Path) : Substitution = N match {
		case <om:OMBVAR>{sbs @ _*}</om:OMBVAR> => sbs.toList.map(Sub.parse(_, base))
      case _ => throw ParseError("not a well-formed substitution: " + N.toString)
	}
}
/** helper object */
object Sub {
   def parse(N: Node, base: Path) = N match {
      case <OMV>{e}</OMV> => TermSub(xml.attr(N, "name"), Obj.parseTerm(e, base))
      case <OMSV>{e}</OMSV> => SeqSub(xml.attr(N, "name"), Obj.parseSequence(e, base))
      case <seqvar>{e}</seqvar> => SeqSub(xml.attr(N, "name"), Obj.parseSequence(e, base)) //TODO remove
      case _ => throw ParseError("not a well-formed case in a substitution: " + N.toString)
   }
}

object ++ {
   def unapply(c: Context) : Option[(Context,VarDecl)] =
      if (c.isEmpty) None
      else Some((c.init, c.last))
   def unapply(s: Substitution) : Option[(Substitution,Sub)] =
      if (s.isEmpty) None
      else Some((s.init, s.last))
}