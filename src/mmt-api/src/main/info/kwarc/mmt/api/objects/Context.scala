package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
import symbols._
import notations._
import utils._
import utils.MyList._
import presentation._
import Conversions._

import scala.xml.Node


/** represents an MMT term variable declaration
 * @param name name
 * @param tp optional type
 * @param df optional definiens
 * @param ats OpenMath-style attributions
 */
case class VarDecl(name : LocalName, tp : Option[Term], df : Option[Term], not: Option[TextNotation]) extends Obj with NamedElement {
   type ThisType = VarDecl
   /** self-written copy method to copy metadata */
   def copy(name : LocalName = this.name, tp : Option[Term] = this.tp, df : Option[Term] = this.df,
                                          not: Option[TextNotation] = this.not) = {
      val vd = VarDecl(name, tp, df, not)
      vd.copyFrom(this)
      vd
   }
   def toTerm = OMV(name)
   def substitute(sub : Substitution)(implicit sa: SubstitutionApplier) = {
      val vd = VarDecl(name, tp.map(_ ^^ sub), df.map(_ ^^ sub), not)
      vd.copyFrom(this)
      vd
   }
   private[objects] def freeVars_ = (tp map {_.freeVars_}).getOrElse(Nil) ::: (df map {_.freeVars_}).getOrElse(Nil)
   def subobjects = subobjectsNoContext(tp.toList ::: df.toList)
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
   def toConstant(mp: MPath, con : Context) = {
     val sub = con.map(vd => vd.name / (OMS(mp ? name)))
     symbols.Constant(OMMOD(mp), name, Nil, tp map(_^? sub), df map(_^? sub), None)
   }
   def toNode = <om:OMV name={name.toPath}>{mdNode}{tpN}{dfN}</om:OMV> 
   def toCMLQVars(implicit qvars: Context) = <bvar><ci>{name.toPath}</ci>{(tp.toList:::df.toList).map(_.toCMLQVars)}</bvar>
   def head = tp.flatMap(_.head)
   override def toString = this match {
      case IncludeVarDecl(p, args) => p.toString + args.mkString(" ")
      case _ => name.toString + tp.map(" : " + _.toString).getOrElse("") + df.map(" = " + _.toString).getOrElse("")
   }
   private def tpN = tp.map(t => <type>{t.toNode}</type>).getOrElse(Nil)
   private def dfN = df.map(t => <definition>{t.toNode}</definition>).getOrElse(Nil)
   
   def toDeclaration(home: Term): Declaration = this match {
      case IncludeVarDecl(p,args) =>
         Include(home, p, args)
      case StructureVarDecl(n, from, dfOpt) => dfOpt match {
         case None =>
            DeclaredStructure(home, name, from, false)
         case Some(ComplexMorphism(subs)) =>
            val s = DeclaredStructure(home, name, from, false)
            //TODO add body
            s
         case Some(df) =>
            DefinedStructure(home, name, from, df, false)
      }
      case _ => Constant(home, name, Nil, tp, df, None, NotationContainer(not))
   }
}

object IncludeVarDecl {
   def apply(p: MPath, args: List[Term]) = VarDecl(LocalName(ComplexStep(p)), Some(OMPMOD(p, args)), None, None)
   def unapply(vd: VarDecl): Option[(MPath, List[Term])] = vd match {
      case VarDecl(LocalName(List(ComplexStep(p))), Some(OMPMOD(q, args)), None, _) if p == q => Some((p,args))
      case _ => None
   }
}

/**
 * apply/unapply methods for variable declarations that correspond to [[Structure]]s
 */
object StructureVarDecl {
   /**
    * @param n the name 
    * @param from the domain, must be OMMOD or OMPMOD
    * @param df optional definiens, must be total morphism expresion or partial ComplexMorphism
    */
   def apply(n: LocalName, from: Term, df: Option[Term]) =
      VarDecl(n, Some(from), df, None)
   def unapply(vd: VarDecl) : Option[(LocalName, Term, Option[Term])] =
      vd.tp match {
         case Some(from @ OMPMOD(p, args)) =>
            Some((vd.name, from, vd.df))
         case _ => None
   }
}

/** represents an MMT context as a list of variable declarations */
case class Context(variables : VarDecl*) extends Obj with ElementContainer[VarDecl] with DefaultLookup[VarDecl] {
   type ThisType = Context
   def getDeclarations = variables.toList
   
   /** add a theory inclusion at the end */
   def ++(p : MPath) : Context = this ++ Context(p)
   /** add variable at the end */
   def ++(v : VarDecl) : Context = this ++ Context(v)
   /** concatenate contexts */
   def ++(that : Context) : Context = this ::: that

   /** look up a variable by name, throws LookupError if not declared */
   def apply(name : LocalName) : VarDecl = getO(name).getOrElse {
      throw LookupError(name, this)
   }
   /** @return the de Bruijn index of the variable, starting from 0 */
   def index(name: LocalName): Option[Int] = variables.lastIndexWhere(_.name == name) match { 
	   case -1 => None
	   case i => Some(variables.length - i - 1) 
   }
   /** @return the prefix up to and excluding the variable */
   def before(name: LocalName): Context = {
      index(name) match {
         case None => this 
         case Some(i) => Context(variables.take(variables.length-i-1):_*)
      }
   }
   /** @return the suffix after and excluding the variable */
   def after(name: LocalName): Context = {
      index(name) match {
         case None => Context.empty 
         case Some(i) => Context(variables.drop(variables.length-i):_*)
      }
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
            des ::= DomainElement(name, total, Some((tp, definedAt)))
         case VarDecl(n, _, df, _) =>
            des ::= DomainElement(n, df.isDefined, None)
      }
      des.reverse
   }
   /** all theories directly included into this context */
   def getIncludes: List[MPath] = variables.toList flatMap {
      case IncludeVarDecl(p, args) => List(p)
      case _ => Nil
   }
   
   /** the identity substitution of this context */
   def id : Substitution = this map {
	   case VarDecl(n, _, _, _) => Sub(n,OMV(n))
   }
   /**
    * @return substitution that maps variables according to args, None if lengths do not match
    */
   def /(args: List[Term]): Option[Substitution] = {
      if (variables.length != args.length) None else {
         val s: Substitution = (variables.toList zip args).map {case (vd,a) => Sub(vd.name, a)}
         Some(s)
      }
   }
 
   /** applies a function to the type/definiens of all variables (in the respective context)
    * @return the resulting context
    */
   def mapTerms(f: (Context,Term) => Term): Context = {
      val newvars = variables.zipWithIndex map {case (VarDecl(n,tp,df, not), i) =>
         val con = Context(variables.take(i) : _*)
         VarDecl(n, tp map {f(con,_)}, df map {f(con,_)}, not) 
      }
      Context(newvars : _*)
   }
   
   /** @return an iterator over all declarations in their respective context */
   def declsInContext = new Iterator[(Context,VarDecl)] {
      private var sofar = Context()
      private var todo  = variables
      def hasNext = !todo.isEmpty
      def next = {
         val hd::tl = todo
         val ret = (sofar,hd)
         sofar = sofar ++ hd
         todo = tl
         ret
      }
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
      val ret = Context(newvars :_*)
      ret.copyFrom(this)
      ret
   }
   private[objects] def freeVars_ = {
      var except: List[LocalName] = Nil
      this flatMap {vd =>
        val fv = vd.freeVars_.filterNot(except contains _) //remove those variables that are declared previously in this context
        except ::= vd.name
        fv
      }
   }
   def subobjects = mapVarDecls {case (con, vd) => (con, vd)}
   /** returns this as a substitutions using those variables that have a definiens */
   def toPartialSubstitution : Substitution = {
     variables.toList mapPartial {
        case VarDecl(n,_,Some(df), _) => Some(Sub(n,df))
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
     <apply>{this.map(v => v.toCMLQVars)}</apply>
   def head = None
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
   def subobjects = subobjectsNoContext(List(target))
   def toNode: Node = <om:OMV name={name.toString}>{mdNode}{target.toNode}</om:OMV>
   def toCMLQVars(implicit qvars: Context) : Node = <mi name={name.toPath}>{target.toCMLQVars}</mi>
   override def toString = name + ":=" + target.toString
   def head = None
}

object IncludeSub {
   def apply(p: MPath, mor: Term) = Sub(LocalName(ComplexStep(p)), mor)
   def unapply(s: Sub) = s match {
      case Sub(LocalName(List(ComplexStep(p))), mor) => Some((p, mor))
      case _ => None
   }
}

object StructureSub {
   def apply(n: LocalName, mor: Term) =
      Sub(n, mor)
   def unapply(s: Sub) : Option[(LocalName, Term)] =
      s match {
         case Sub(n,mor) => Some((n,mor))
         case _ => None
   }
}

/** substitution between two contexts */
case class Substitution(subs : Sub*) extends Obj {
   type ThisType = Substitution
   def ++(n:String, t:Term) : Substitution = this ++ Sub(LocalName(n),t)
   def ++(s: Sub) : Substitution = this ++ Substitution(s)
   def ++(that: Substitution) : Substitution = this ::: that
   def substitute(sub : Substitution)(implicit sa: SubstitutionApplier) = {
     val ret = this map {s => s ^^ sub}
     ret.copyFrom(this)
     ret
   }
   private[objects] def freeVars_ = (this flatMap {_.freeVars_})
   def subobjects = subobjectsNoContext(subs.toList)
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
    	  case Sub(n, t) => VarDecl(n, None, Some(t), None)
      }
      Context(decls : _*)
   }
   def mapTerms(f: Term => Term): Substitution = this map {s =>
      Sub(s.name, f(s.target))
   }

   override def toString = this.map(_.toString).mkString("",", ","")
   def toNode =
      <om:OMBVAR>{mdNode}{subs.zipWithIndex.map(x => x._1.toNode)}</om:OMBVAR>
   def toCMLQVars(implicit qvars: Context) = asContext.toCMLQVars
   def head = None
   def isEmpty = subs.isEmpty
}

/** helper object */
object Context {
   /** implicit conversion between a context and a list of variable declarations */
   implicit def list2context(l : List[VarDecl]) : Context = Context(l : _*)
   implicit def context2list(c: Context) : List[VarDecl] = c.variables.toList
   implicit def vardec2context(d: VarDecl) : Context = Context(d)
   /** a context consisting of a single theory */
   def apply(p: MPath): Context = Context((IncludeVarDecl(p,Nil)))
   val empty = Context()
	/** parses an OMBVAR into a context */
	def parse(Nmd : scala.xml.Node, nsMap: NamespaceMap) : Context = {
	   val (n,mdOpt) = metadata.MetaData.parseMetaDataChild(Nmd, nsMap)
	   val c = xml.trimOneLevel(n) match {
	      case <OMBVAR>{decls @ _*}</OMBVAR> =>  decls.toList.map(VarDecl.parse(_, nsMap))
         case _ => throw ParseError("not a well-formed context: " + n.toString)
	   }
      mdOpt.foreach {md => c.metadata = md}
      c
	}
	/** generate new variable name similar to x */
   private def rename(x: LocalName) = {
      if (x.length == 1)
         x / "0"
      else x.last match {
         case SimpleStep(n) if n.matches("[0-9]+") => x.init / ((n.toInt+1).toString)
         case _ => x / "0"
      }
   }
   /** picks a variable name that is fresh for context, preferably x1 */
   def pickFresh(context: Context, x1: LocalName): (LocalName,Substitution) = {
      var x = x1
      while (context.isDeclared(x)) {
         x = rename(x)
      }
      (x, x1 / OMV(x))
   }
	/** returns an alpha-renamed version of con that declares no variable from forbidden, and a substitution that performs the alpha-renaming */
	def makeFresh(con: Context, forbidden: List[LocalName]) : (Context,Substitution) = {
	   if (forbidden.forall(n => !con.isDeclared(n)))
   	    // optimization in case there is nothing to do
	      return (con, con.id)
	   var sub = Substitution()
	   val conN = con map {case vd @ VarDecl(x,tp,df, _) =>
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
   private def parseComponents(N: Seq[Node], nsMap: NamespaceMap) : (Option[Term], Option[Term], Option[TextNotation]) = {
      var tp : Option[Term] = None
      var df : Option[Term] = None
      var not: Option[TextNotation] = None
      N.map(xml.trimOneLevel).foreach {
            case <type>{t}</type> => tp = Some(Obj.parseTerm(t, nsMap))
            case <definition>{t}</definition> => df = Some(Obj.parseTerm(t, nsMap))
            case <notation>{n}</notation> => not = Some(TextNotation.parse(n, nsMap))
            case n => throw ParseError("not a well-formed variable component: " + n.toString)
      }
      (tp, df, not)
   }
   def parse(Nmd: Node, nsMap: NamespaceMap) : VarDecl = {
      val (n,mdOpt) = metadata.MetaData.parseMetaDataChild(Nmd, nsMap)
      xml.trimOneLevel(n) match {      
         case <OMV>{body @ _*}</OMV> =>
            val name = LocalName.parse(xml.attr(n, "name"))
            val (tp, df, not) = parseComponents(body, nsMap)
            val vd = VarDecl(name, tp, df, not)
            mdOpt.foreach {md => vd.metadata = md}
            vd
         case _ => throw ParseError("not a well-formed variable declaration: " + n.toString)
      }
   }
}
/** helper object */
object Substitution {
   /** implicit conversion between a substitution and a list of maps */
   implicit def list2substitution(l : List[Sub]) : Substitution = Substitution(l:_*)
   implicit def substitution2list(s: Substitution) : List[Sub] = s.subs.toList
   implicit def varsub2substitution(s: Sub) : Substitution = Substitution(s)   
   val empty = Substitution()
	/** parsers an OMBVAR into a substitution */
	def parse(Nmd : scala.xml.Node, nsMap: NamespaceMap) : Substitution = {
      val (n,mdOpt) = metadata.MetaData.parseMetaDataChild(Nmd, nsMap)
	   val s = xml.trimOneLevel(n) match {
   		case <OMBVAR>{sbs @ _*}</OMBVAR> => sbs.toList.map(Sub.parse(_, nsMap))
         case _ => throw ParseError("not a well-formed substitution: " + n.toString)
      }
      mdOpt.foreach {md => s.metadata = md}
      s
   }
}
/** helper object */
object Sub {
   def parse(Nmd: Node, nsMap: NamespaceMap) = {
      val (n,mdOpt) = metadata.MetaData.parseMetaDataChild(Nmd, nsMap)      
      val s = xml.trimOneLevel(n) match {
         case <OMV>{e}</OMV> => Sub(LocalName.parse(xml.attr(n, "name")), Obj.parseTerm(e, nsMap))
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