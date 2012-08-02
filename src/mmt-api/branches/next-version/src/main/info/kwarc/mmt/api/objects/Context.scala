package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
import utils._
import utils.MyList._
import presentation._
import Conversions._

import scala.xml.Node

//TODO: add optional notation
/** represents an MMT term variable declaration
 * @param name name
 * @param tp optional type
 * @param df optional definiens
 * @param ats OpenMath-style attributions
 */
case class VarDecl(name : LocalName, tp : Option[Term], df : Option[Term], ats: (GlobalName,Term)*) extends Obj {
   val attrs = ats.toList
   /** self-written copy method that does not allow changing attributions
    * (because sequence arguments and copy do not work together) */
   def copy(name : LocalName = this.name, tp : Option[Term] = this.tp, df : Option[Term] = this.df) =
      VarDecl(name, tp, df, ats:_*)
   def ^(sub : Substitution) = VarDecl(name, tp.map(_ ^ sub), df.map(_ ^ sub))
   private[objects] def freeVars_ = (tp map {_.freeVars_}).getOrElse(Nil) ::: (df map {_.freeVars_}).getOrElse(Nil) 
   /** converts to an OpenMath-style attributed variable using two special keys */
   def toOpenMath : Term = {
	   val varToOMATTR = attrs.foldLeft[Term](OMV(name)) {(v,a) => OMATTR(v, OMID(a._1), a._2)}
      (tp, df) match {
         case (None, None) => varToOMATTR
         case (Some(t), None) => 
           OMATTR(varToOMATTR, OMID(mmt.mmttype), t)
         case (None, Some(d)) => OMATTR(varToOMATTR, OMID(mmt.mmtdef), d)
         case (Some(t), Some(d)) => OMATTR(OMATTR(varToOMATTR, OMID(mmt.mmttype), t), OMID(mmt.mmtdef), d)
      }
   }
   def toNodeID(pos : Position) = <om:OMV name={name.toPath}>{tpN(pos)}{dfN(pos)}{attrsN(pos)}</om:OMV> % pos.toIDAttr 
   def toCML = toOpenMath.toCML
   def role = Role_Variable
   def head = None
   def components = List(StringLiteral(name.toString), tp.getOrElse(Omitted), df.getOrElse(Omitted))
   override def toString = name.toString + tp.map(" : " + _.toString).getOrElse("") + df.map(" = " + _.toString).getOrElse("")  
   private def tpN(pos : Position) = tp.map(t => <type>{t.toNodeID(pos + 1)}</type>).getOrElse(Nil)
   private def dfN(pos : Position) = df.map(t => <definition>{t.toNodeID(pos + 2)}</definition>).getOrElse(Nil)
   private def attrsN(pos : Position) = attrs map {case (path,tm) => <attribution key={path.toPath}>{tm.toNode}</attribution>}
}

/** represents an MMT context as a list of variable declarations */
case class Context(variables : VarDecl*) extends Obj {
   /** add variable at the end */
   def ++(v : VarDecl) : Context = this ++ Context(v)
   /** concatenate contexts */
   def ++(that : Context) : Context = this ::: that
   /** look up a variable by name, throws LookupError if not declared */
   def apply(name : LocalName) : VarDecl = {
      variables.reverse.find(_.name == name).getOrElse(throw LookupError(name))
   }
   def isDeclared(name : LocalName) = this.exists(_.name == name)
   /** returns the de Bruijn index of a variable, starting from 0, throws LookupError if none */
   def index(name: LocalName): Int = variables.lastIndexWhere(_.name == name) match { 
	   case -1 => throw LookupError(name)
	   case i => variables.length - i - 1 
   }
   /** the identity substitution of this context */
   def id : Substitution = this map {
	   case VarDecl(n, _, _, _*) => Sub(n,OMV(n))
   }
 
   /** applies a function to the type/definiens of all variables (in the respective context)
    * @return the resulting context
    */
   def mapTerms(f: (Context,Term) => Term): Context = {
      val newvars = variables.zipWithIndex map {case (VarDecl(n,tp,df,attrs @ _*), i) =>
         val con = Context(variables.take(i) : _*)
         VarDecl(n, tp map {f(con,_)}, df map {f(con,_)}, attrs : _*) 
      }
      Context(newvars : _*)
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
   private[objects] def freeVars_ = {
      var except: List[LocalName] = Nil
      this flatMap {vd =>
        val fv = vd.freeVars_.filterNot(except contains _) //remove those variables that are declared previously in this context
        except ::= vd.name
        fv
      }
   }
   /** returns this as a substitutions using those variables that have a definiens */
   def toPartialSubstitution : Substitution = {
     variables.toList mapPartial {
        case VarDecl(n,_,Some(df), _*) => Some(Sub(n,df))
        case _ => None
     }
   }
   /** returns this as a substitution if all variables have a definiens */
   def toSubstitution : Option[Substitution] = {
     val subs = toPartialSubstitution
     if (subs.length == this.length) Some(subs) else None
   }
   override def toString = this.map(_.toString).mkString("",", ","")
   def toNodeID(pos : Position) =
     <om:OMBVAR>{this.zipWithIndex.map({case (v,i) => v.toNodeID(pos + i)})}</om:OMBVAR>
   def toCML = 
     <m:bvar>{this.map(v => v.toCML)}</m:bvar>
   def head = None
   def role : Role = Role_context
   def components = variables.toList 
}

/** a case in a substitution */		
case class Sub(name : LocalName, target : Term) extends Obj {
   def ^(sub : Substitution) = Sub(name, target ^ sub)
   private[objects] def freeVars_ = target.freeVars_
   def role : Role = Role_termsub
   def toNodeID(pos: Position): Node = <om:OMV name={name.toString}>{target.toNodeID(pos + 1)}</om:OMV>
   def toCML : Node = <m:mi name={name.toPath}>{target.toCML}</m:mi>
   def components = List(StringLiteral(name.toString), target)
   override def toString = name + "/" + target.toString
   def head = None
}


/** substitution between two contexts */
case class Substitution(subs : Sub*) extends Obj {
   def ++(n:String, t:Term) : Substitution = this ++ Sub(LocalName(n),t)
   def ++(s: Sub) : Substitution = this ++ Substitution(s)
   def ++(that: Substitution) : Substitution = this ::: that
   def ^(sub : Substitution) = this map {
	   case Sub(v,t) => Sub(v, t ^ sub)
   }
   private[objects] def freeVars_ = (this flatMap {_.freeVars_})
   def maps(n: LocalName): Boolean = this exists {_.name == n}
   def apply(v : LocalName) : Option[Obj] = subs.reverse.find(_.name == v).map(_.target)
   def isIdentity : Boolean = subs forall {
      case Sub(n, OMV(m)) => m == n
      case _ => false 
   }
   /** turns a substitution into a context by treating every substitute as a definiens
    *  this permits seeing substitution application as a let-binding
    */
   def asContext = {
      val decls = subs map {
    	  case Sub(n, t) => VarDecl(n, None, Some(t))
      }
      Context(decls : _*)
   }
   override def toString = this.map(_.toString).mkString("",", ","")
   def toNodeID(pos : Position) =
      <om:OMBVAR>{subs.zipWithIndex.map(x => x._1.toNodeID(pos + x._2))}</om:OMBVAR>
   def toCML = asContext.toCML
   def head = None
   def role : Role = Role_substitution
   def components = subs.toList
}

/** helper object */
object Context {
	/** parses an OMBVAR into a context */
	def parse(N : scala.xml.Node, base : Path) : Context = N match {
	  case <om:OMBVAR>{decls @ _*}</om:OMBVAR> =>  decls.toList.map(VarDecl.parse(_, base))
      case _ => throw ParseError("not a well-formed context: " + N.toString)
	}
	//crude renaming to kind of avoid variable capture
	private def rename(s: LocalName) = s / "_"
	/** returns an alpha-renamed version of con that contains no variable from forbidden, and a substitution that performs the alpha-renaming */
	def makeFresh(con: Context, forbidden: List[LocalName]) : (Context,Substitution) = {
	   var sub = Substitution()
	   val conN = con map {case vd @ VarDecl(x,tp,df,ats @ _*) =>
	      if (forbidden contains x) {
   	      val xn = rename(x)
            val vdn = VarDecl(xn, tp map {_ ^ sub}, df map {_ ^ sub}, ats : _*)
   	      sub = sub ++ OMV(x) / OMV(xn)
   	      vdn
	      } else {
	         sub = sub ++ OMV(x) / OMV(x)
	         vd
	      }
	   }
	   (conN, sub)
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
      N match {      
         case <OMATTR><OMATP>{ats @ _*}</OMATP>{v}</OMATTR> =>
            val name = LocalName.parse(xml.attr(v, "name"))
            val (tp, df, attrs) = parseAttrs(ats, base, pTerm, pTerm, pTerm)
            VarDecl(name, tp, df, attrs : _*)
         case <OMV>{ats @ _*}</OMV> =>
            val name = LocalName.parse(xml.attr(N, "name"))
            val (tp, df, attrs) = parseAttrs(ats, base, pTerm, pTerm, pTerm)
            VarDecl(name, tp, df, attrs : _*)
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
      case <OMV>{e}</OMV> => Sub(LocalName.parse(xml.attr(N, "name")), Obj.parseTerm(e, base))
      case _ => throw ParseError("not a well-formed case in a substitution: " + N.toString)
   }
}

/** Concatenation of contexts and substitutions (extractor object) */
object ++ {
   def unapply(c: Context) : Option[(Context,VarDecl)] =
      if (c.isEmpty) None
      else Some((c.init, c.last))
   def unapply(s: Substitution) : Option[(Substitution,Sub)] =
      if (s.isEmpty) None
      else Some((s.init, s.last))
}