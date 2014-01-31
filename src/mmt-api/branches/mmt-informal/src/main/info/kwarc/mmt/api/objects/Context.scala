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
case class VarDecl(name : LocalName, tp : Option[Term], df : Option[Term]) extends Obj {
   type ThisType = VarDecl
   /** self-written copy method to copy metadata */
   def copy(name : LocalName = this.name, tp : Option[Term] = this.tp, df : Option[Term] = this.df) = {
      val vd = VarDecl(name, tp, df)
      vd.copyFrom(this)
      vd
   }
   def toTerm = OMV(name)
   def substitute(sub : Substitution)(implicit sa: SubstitutionApplier) = {
      val vd = VarDecl(name, tp.map(_ ^^ sub), df.map(_ ^^ sub))
      vd.copyFrom(this)
      vd
   }
   private[objects] def freeVars_ = (tp map {_.freeVars_}).getOrElse(Nil) ::: (df map {_.freeVars_}).getOrElse(Nil) 
   /** converts to an OpenMath-style attributed variable using two special keys */
   def toOpenMath : Term = {
	   val varToOMATTR = OMV(name)
      (tp, df) match {
         case (None, None) => varToOMATTR
         case (Some(t), None) => 
           OMATTR(varToOMATTR, OMID(mmt.mmttype), t)
         case (None, Some(d)) => OMATTR(varToOMATTR, OMID(mmt.mmtdef), d)
         case (Some(t), Some(d)) => OMATTR(OMATTR(varToOMATTR, OMID(mmt.mmttype), t), OMID(mmt.mmtdef), d)
      }
   }
   def toNode = <om:OMV name={name.toPath}>{mdNode}{tpN}{dfN}</om:OMV> 
   def toCMLQVars(implicit qvars: Context) = <m:bvar><m:ci>{name.toPath}</m:ci>{(tp.toList:::df.toList).map(_.toCMLQVars)}</m:bvar>
   def role = Role_Variable
   def head = tp.flatMap(_.head)
   def components =
      List(StringLiteral(name.toString), tp.getOrElse(Omitted), df.getOrElse(Omitted)) :::
      //temporary hack: if any other attribution is present, the type is assumed to be reconstructed
      //and returning a 4th components indicates that
      (if (metadata.getTags contains utils.mmt.inferedTypeTag) List(StringLiteral("")) else Nil)
   override def toString = name.toString + tp.map(" : " + _.toString).getOrElse("") + df.map(" = " + _.toString).getOrElse("")  
   private def tpN = tp.map(t => <type>{t.toNode}</type>).getOrElse(Nil)
   private def dfN = df.map(t => <definition>{t.toNode}</definition>).getOrElse(Nil)
}

object IncludeVarDecl {
   def apply(p: MPath) = VarDecl(LocalName(ComplexStep(p)), Some(OMMOD(p)), None)
   def unapply(vd: VarDecl) = vd match {
      case VarDecl(LocalName(List(ComplexStep(p))), Some(OMMOD(q)), None) if p == q => Some(p)
      case _ => None
   }
}

object StructureVarDecl {
   def apply(n: LocalName, from: Term, df: Option[Term]) =
      VarDecl(n, Some(from), df)
   def unapply(vd: VarDecl) : Option[(LocalName, Term, Option[Term])] =
      vd.tp match {
         case Some(tp @ ComplexTheory(_)) =>
            Some((vd.name, tp, vd.df))
         case _ => None
   }
}

/** represents an MMT context as a list of variable declarations */
case class Context(variables : VarDecl*) extends Obj {
   type ThisType = Context
   /** add variable at the end */
   def ++(v : VarDecl) : Context = this ++ Context(v)
   /** concatenate contexts */
   def ++(that : Context) : Context = this ::: that
   /** look up a variable by name, throws LookupError if not declared */
   def apply(name : LocalName) : VarDecl = {
      variables.reverse.foreach {vd =>
         if (vd.name == name) return vd
      }
      throw LookupError(name, this)
   }
   def isDeclared(name : LocalName) = index(name).isDefined
   /** returns the de Bruijn index of a variable, starting from 0 */
   def index(name: LocalName): Option[Int] = variables.lastIndexWhere(_.name == name) match { 
	   case -1 => None
	   case i => Some(variables.length - i - 1) 
   }
   
   /**
    * @return domain of this context, flattening nested ComplexTheories and ComplexMorphisms
    *
    * Due to ComplexMorphism's, names may erroneously be defined but not declared.
    */
   def getDomain: List[DomainElement] = {
      var des : List[DomainElement] = Nil
      variables foreach {
         case StructureVarDecl(name, tp, df) =>
            val (total, definedAt) : (Boolean, List[LocalName]) = df match {
               case None =>
                  // no definition for any declaration in tp 
                  (false, Nil)
               case Some(ComplexMorphism(subs)) =>
                  // partial morphism, defined at dom
                  //TODO this does not cover the morphism case because tp with be empty
                  val dom = subs.asContext.getDomain.map {de => de.name / name}
                  (false, dom)
               case Some(morph) =>
                  // everything else is a total morphism from tp, i.e., everything is defined
                  (true, Nil) // second component is irrelevant
            }
            tp match {
               case OMMOD(p) =>
                  des ::= DomainElement(name, total, Some((p, definedAt)))
               case ComplexTheory(body) =>
                  val bodyDes = body.getDomain
                  bodyDes.foreach {case DomainElement(n, defined, subdOpt) =>
                     val subdOptNew = subdOpt map {case (p,ds) => (p, ds ::: definedAt)}
                     des ::= DomainElement(name / n, defined, subdOptNew)
                  }
            }
         case VarDecl(n, _, df) =>
            des ::= DomainElement(n, df.isDefined, None)
      }
      des.reverse
   }
   
   /** the identity substitution of this context */
   def id : Substitution = this map {
	   case VarDecl(n, _, _) => Sub(n,OMV(n))
   }
 
   /** applies a function to the type/definiens of all variables (in the respective context)
    * @return the resulting context
    */
   def mapTerms(f: (Context,Term) => Term): Context = {
      val newvars = variables.zipWithIndex map {case (VarDecl(n,tp,df), i) =>
         val con = Context(variables.take(i) : _*)
         VarDecl(n, tp map {f(con,_)}, df map {f(con,_)}) 
      }
      Context(newvars : _*)
   }
   /** applies a function to each VarDecl, each time in the respective context
    * @return the list of results
    */
   def mapVarDecls[A](f: (Context,VarDecl) => A): List[A] = {
      variables.zipWithIndex.toList map {case (vd, i) =>
         val con = Context(variables.take(i) : _*)
         f(con, vd)
      }
   }
   
   /**
    * if c1 and c2 have the same length, then (c1 alpha c2 : c1 -> c2) is the substitution mapping corresponding variables
    * 
    *  This substitution can be used to alpha-rename c1-objects to c2-objects. 
    */
   def alpha(that: Context): Option[Substitution] = 
      if (this.length == that.length) {
         val subs = (this zip that).map {case (vd1,vd2) => vd1.name / OMV(vd2.name)}
         Some(Substitution(subs:_*))
      } else
         None
   
   /** substitutes in all variable declarations except for the previously declared variables
    *  if |- G ++ H  and  |- sub : G -> G'  then  |- G' ++ (H ^ sub)
    */
   def substitute(sub : Substitution)(implicit sa: SubstitutionApplier) : Context = {
      val id = this.id // precompute value
      // sub ++ id.take(i) represents sub, x_1/x_1, ..., x_{i-1}/x_{i-1} 
      val newvars = variables.zipWithIndex map {case (vd, i) => vd ^^ (sub ++ id.take(i))}
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
        case VarDecl(n,_,Some(df)) => Some(Sub(n,df))
        case _ => None
     }
   }
   /** returns this as a substitution if all variables have a definiens */
   def toSubstitution : Option[Substitution] = {
     val subs = toPartialSubstitution
     if (subs.length == this.length) Some(subs) else None
   }
   override def toString = this.map(_.toString).mkString("",", ","")
   def toNode =
     <om:OMBVAR>{mdNode}{this.zipWithIndex.map({case (v,i) => v.toNode})}</om:OMBVAR>
   def toCMLQVars(implicit qvars: Context) = 
     <m:apply>{this.map(v => v.toCMLQVars)}</m:apply>
   def head = None
   def role : Role = Role_context
   def components = variables.toList 
}

/** a case in a substitution */		
case class Sub(name : LocalName, target : Term) extends Obj {
   type ThisType = Sub
   def substitute(sub : Substitution)(implicit sa: SubstitutionApplier) = {
     val s = Sub(name, target ^^ sub)
     s.copyFrom(this)
     s
   }
   private[objects] def freeVars_ = target.freeVars_
   def role : Role = Role_termsub
   def toNode: Node = <om:OMV name={name.toString}>{mdNode}{target.toNode}</om:OMV>
   def toCMLQVars(implicit qvars: Context) : Node = <m:mi name={name.toPath}>{target.toCMLQVars}</m:mi>
   def components = List(StringLiteral(name.toString), target)
   override def toString = name + ":=" + target.toString
   def head = None
}


/** substitution between two contexts */
case class Substitution(subs : Sub*) extends Obj {
   type ThisType = Substitution
   def ++(n:String, t:Term) : Substitution = this ++ Sub(LocalName(n),t)
   def ++(s: Sub) : Substitution = this ++ Substitution(s)
   def ++(that: Substitution) : Substitution = this ::: that
   def substitute(sub : Substitution)(implicit sa: SubstitutionApplier) = this map {s => s ^^ sub}
   private[objects] def freeVars_ = (this flatMap {_.freeVars_})
   def maps(n: LocalName): Boolean = this exists {_.name == n}
   def apply(v : LocalName) : Option[Term] = subs.reverse.find(_.name == v).map(_.target)
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
   def toNode =
      <om:OMBVAR>{mdNode}{subs.zipWithIndex.map(x => x._1.toNode)}</om:OMBVAR>
   def toCMLQVars(implicit qvars: Context) = asContext.toCMLQVars
   def head = None
   def role : Role = Role_substitution
   def components = subs.toList
   def isEmpty = components.isEmpty
}

/** helper object */
object Context {
	/** parses an OMBVAR into a context */
	def parse(Nmd : scala.xml.Node, base : Path) : Context = {
	   val (n,mdOpt) = metadata.MetaData.parseMetaDataChild(Nmd, base)
	   val c = n match {
	      case <om:OMBVAR>{decls @ _*}</om:OMBVAR> =>  decls.toList.map(VarDecl.parse(_, base))
         case _ => throw ParseError("not a well-formed context: " + n.toString)
	   }
      mdOpt.foreach {md => c.metadata = md}
      c
	}
	/** generate new variable name similar to x */
   private def rename(x: LocalName) = x / ""
   /** picks a variable name that is fresh for context, preferably x */
   def pickFresh(context: Context, x1: LocalName): (LocalName,Substitution) = {
      var x = x1
      while (context.isDeclared(x)) {
         x = rename(x)
      }
      (x, x1 / OMV(x))
   } 
	/** returns an alpha-renamed version of con that contains no variable from forbidden, and a substitution that performs the alpha-renaming */
	def makeFresh(con: Context, forbidden: List[LocalName]) : (Context,Substitution) = {
	   var sub = Substitution()
	   val conN = con map {case vd @ VarDecl(x,tp,df) =>
	      var xn = x
	      while (forbidden contains xn) {xn = rename(xn)}
	      val vdn = if (x == xn && sub.isIdentity)
	         vd
	      else
            vd.copy(name = xn, tp = tp map {_ ^? sub}, df = df map {_ ^? sub})
	      sub = sub ++ OMV(x) / OMV(xn)
	      vdn
	   }
	   (conN, sub)
	}
}

/** helper object */
object VarDecl {
   private def parseAttrs(N: Seq[Node], base: Path) : (Option[Term], Option[Term], Boolean) = {
      var tp : Option[Term] = None
      var df : Option[Term] = None
      var attrs = false
      N.toList.foreach {
            case <type>{t}</type> => tp = Some(Obj.parseTerm(t, base))
            case <definition>{t}</definition> => df = Some(Obj.parseTerm(t, base))
            case _ => attrs = true // for Twelf: all other children mark inferred types
      }
      (tp, df, attrs)
   }
   def parse(Nmd: Node, base: Path) : VarDecl = {
      val (n,mdOpt) = metadata.MetaData.parseMetaDataChild(Nmd, base)
      n match {      
         case <OMV>{body @ _*}</OMV> =>
            val name = LocalName.parse(xml.attr(n, "name"))
            val (tp, df, attrs) = parseAttrs(body, base)
            val vd = VarDecl(name, tp, df)
            mdOpt.foreach {md => vd.metadata = md}
            if (attrs) vd.metadata.add(metadata.Tag(utils.mmt.inferedTypeTag)) // for Twelf export compatibility, TODO remove
            vd
         case _ => throw ParseError("not a well-formed variable declaration: " + n.toString)
      }
   }
}
/** helper object */
object Substitution {
	/** parsers an OMBVAR into a substitution */
	def parse(Nmd : scala.xml.Node, base : Path) : Substitution = {
      val (n,mdOpt) = metadata.MetaData.parseMetaDataChild(Nmd, base)
	   val s = n match {
   		case <om:OMBVAR>{sbs @ _*}</om:OMBVAR> => sbs.toList.map(Sub.parse(_, base))
         case _ => throw ParseError("not a well-formed substitution: " + n.toString)
      }
      mdOpt.foreach {md => s.metadata = md}
      s
   }
}
/** helper object */
object Sub {
   def parse(Nmd: Node, base: Path) = {
      val (n,mdOpt) = metadata.MetaData.parseMetaDataChild(Nmd, base)      
      val s = n match {
         case <OMV>{e}</OMV> => Sub(LocalName.parse(xml.attr(n, "name")), Obj.parseTerm(e, base))
         case _ => throw ParseError("not a well-formed case in a substitution: " + n.toString)
      }
      mdOpt.foreach {md => s.metadata = md}
      s
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