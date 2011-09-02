package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
import utils._
import libraries._
import modules._
import presentation._
import Conversions._

import scala.xml.{Node}

//import info.kwarc.mmt.api.utils.{log}

/**
 * An Obj represents an MMT object. MMT objects are represented by immutable Scala objects.
 */
sealed abstract class Obj extends Content {
   /** prints to OpenMath (with OMOBJ wrapper) */
   def toNodeID(pos : Position) : scala.xml.Node
   def toNode : scala.xml.Node = toNodeID(Position.None)
   def toOBJNode = {
      val om = xml.namespace("om") // inlining this into XML literal does not work
      <OMOBJ xmlns:om={om}>{toNode}</OMOBJ>
   }
   /** applies a substitution to an object (computed immediately) */
   def ^ (sub : Substitution) : Obj
   def head : Option[Path]
   def role : Role
   def nkey = NotationKey(head, role)
   def components : List[Content]
   def presentation(lpar: LocalParams) = ByNotation(nkey, components, lpar)
   def toCML: Node
}

trait MMTObject {
   def args : List[Obj]
   def components = OMID(path) :: args
   def path : GlobalName
   def head = Some(path)
   def role = if (args.isEmpty) Role_ConstantRef else Role_application
   def toNodeID(pos : Position) =
      if (args.isEmpty) OMID(path).toNode
      else
      <om:OMA>{components.zipWithIndex.map({case (m,i) => m.toNodeID(pos+i)})}
      </om:OMA> % pos.toIDAttr
   def toCML = 
     if (args.isEmpty) <m:csymbol>{path.toPath}</m:csymbol>
     else <m:apply>{components.zipWithIndex.map({case (m,i) => m.toCML})}</m:apply> 
}

/**
 * A Term represents an MMT term.
 */
sealed abstract class Term extends SeqItem {
   def strip : Term = this
   def ^(sub : Substitution) : Term
   /** morphism application (written postfix), maps OMHID to OMHID */
   def *(that : Morph) : Term = OMM(this, that)
   /** permits the intuitive f(t_1,...,t_n) syntax for term applications */
   def apply(args : Term*) = OMA(this, args.toList)

}

/**
 * OMHID represents the hidden term.
 */
case object OMHID extends Term with MMTObject {
   def ^(sub : Substitution) = this
   override def *(that: Morph) = this
   def path = mmt.mmtsymbol("hidden")
   def args = Nil
}

case class OMID(gname: GlobalName) extends Term {
   def parent = gname.parent
   def name = gname.name
   def head = Some(gname)
   def ^(sub : Substitution) = this
   def role = gname match {
      case OMMOD(p) % _ => Role_ConstantRef
      case _ => Role_ComplexConstantRef
   }
   def components = gname match {
      case OMMOD(doc ? mod) % ln => List(StringLiteral(doc.toPath), StringLiteral(mod.flat),
                                 StringLiteral(ln.flat), StringLiteral(gname.toPathEscaped))
      case _ => List(parent, StringLiteral(name.flat)) 
   }
   def toNodeID(pos : Position) = parent match {
      case OMMOD(doc ? mod) => <om:OMS base={doc.toPath} module={mod.flat} name={name.flat}/> % pos.toIDAttr
      case par => <om:OMS name={name.flat}>{par.toNodeID(pos + 0)}</om:OMS> % pos.toIDAttr
   }
   
   def toCML = <csymbol>{gname.toPath}</csymbol>
}

/**
 * An OMBINDC represents a binding with condition
 * @param binder the binder
 * @param context the bound variables (from outside to inside)
 * @param condition an optional condition on the variables
 * @param body the scope/body/matrix of the binder
 */
case class OMBINDC(binder : Term, context : Context, condition : Option[Term], body : Term) extends Term  {
   def head = binder.head
   val numVars = context.variables.length
   def components = binder :: context.toList ::: List(body) //condition.getOrElse(Omitted)
   def role = Role_binding
   override def presentation(lpar : LocalParams) = {
      val LocalParams(pos, ip, cont, io) = lpar
      val addedContext = head match {
         case Some(path : GlobalName) => context.zipWithIndex.map {
           case (v, i) => VarData(v.name, path, pos + (i+1))
         }
         case _ => throw PresentationError("binder without a path: " + this)
      }
      ByNotation(nkey, components, LocalParams(pos, ip, cont ::: addedContext, io))
   }
   def toNodeID(pos : Position) = 
      <om:OMBIND>{binder.toNodeID(pos + 0)}
                 {context.toNodeID(pos)}
                 {if (condition.isDefined) condition.get.toNodeID(pos + (numVars + 2))}
                 {body.toNodeID(pos + (numVars + 1))}
      </om:OMBIND> % pos.toIDAttr
   override def toString = "(" + binder + " [" + context + "] " + body + ")"  
   def ^(sub : Substitution) = {
      val subid = sub ++ context.id
      OMBINDC(binder ^ sub, context ^ sub, condition.map(_ ^ subid), body ^ subid)
   }
      
   def toCML = condition match {
     case Some(cond) => <m:apply>{binder.toCML}{context.toCML}<m:condition>{cond.toCML}</m:condition>{body.toCML}</m:apply>
     case None => <m:apply>{binder.toCML}{context.toCML}{body.toCML}</m:apply>
   }
}

/**
 * OMBIND represents a binding without condition
 */
object OMBIND {
	def apply(binder : Term, context : Context, body : Term) = OMBINDC(binder, context, None, body)
	def unapply(t : Term): Option[(Term,Context,Term)] = t match {
		case OMBINDC(b,c,None,s) => Some((b,c,s))
		case _ => None
	}
}

/**
 * An OMM represents a morphism application to a term
 * @param arg the argument term
 * @param via the applied morphism
 */
case class OMM(arg : Term, via : Morph) extends Term with MMTObject {
   def path = mmt.morphismapplication
   def args = List(arg,via)
   def ^ (sub : Substitution) = OMM(arg ^ sub, via ^ sub) 
   
}

/** an explicit substitution behaves like a let-binder
 * consequently, the substitution must be given as Context to introduce information about the bound variables
 * G |- OMSub(arg,via) iff G, via |- arg  
 */
case class OMSub(arg: Term, via: Context) extends Term {
   def head = Some(mmt.substitutionapplication)
   def role = Role_binding
   def ^ (sub : Substitution) = OMSub(arg ^ sub ++ via.id, via ^ sub)
   private def asBinder = OMBIND(OMID(mmt.substitutionapplication), via, arg)
   def toNodeID(pos: Position) = asBinder.toNodeID(pos)
   def components = asBinder.components
   def toCML = <m:apply><csymbol>OMSub</csymbol>{arg.toCML}</m:apply>
}

/**
 * An OMA represents an application of a term to a list of terms
 * @param fun the function term
 * @param args the list of argument terms
 */
case class OMA(fun : Term, args : List[SeqItem]) extends Term {  
   def head = fun.head
   def role = Role_application
   def components = fun :: args
   override def toString = (fun :: args).map(_.toString).mkString("@(", ", ", ")")
   def toNodeID(pos : Position) =
      <om:OMA>{fun.toNodeID(pos + 0)}
              {args.zipWithIndex.map({case (a,i) => a.toNodeID(pos+(i+1))})}
      </om:OMA> % pos.toIDAttr
   def ^ (sub : Substitution) = OMA(fun ^ sub, args.map(_ ^ sub).flatMap(_.items))
  def toCML = <m:apply>{fun.toCML}{args.map(_.toCML)}</m:apply>

}

/**
 * An OMV represents a reference to a variable.
 * @param name the name of the referenced variable
 */
case class OMV(name : String) extends Term {
   def head = None
   def role = Role_VariableRef
   def components = Nil 
   override def presentation(lpar : LocalParams) = {
         lpar.context.reverse.zipWithIndex.find(_._1.name == name) match {
            case Some((VarData(_, binder, pos), i)) =>
               val components = List(StringLiteral(name), StringLiteral(i.toString), StringLiteral(pos.toString)) 
               ByNotation(NotationKey(Some(binder), role), components, lpar)
            case None => throw PresentationError("unbound variable")
         }
   }
   /** the substutition this/s */
   def /(s : Term) = TermSub(name, s)
   /** the declaration this:tp */
   def %(tp : Term) = TermVarDecl(name, Some(tp), None)
   def toNodeID(pos : Position) = <om:OMV name={name}/> % pos.toIDAttr
   override def toString = name
   def ^(sub : Substitution) =
	   sub(name) match {
	  	   case Some(t: Term) => t
	  	   case Some(_) => 
	  	     throw SubstitutionUndefined(name, "substitution is applicable but does not provide a term")
	  	   case None => this
       }
   
   def toCML = <m:ci>{name}</m:ci>
}

/**
 * An OME represents an application of an error-constructing term to a list of terms
 * @param error the error-constructor
 * @param args the list of argument terms
 */
case class OME(error : Term, args : List[Term]) extends Term {
   def head = error.head
   def role = Role_application
   def components = error :: args
   def toNodeID(pos : Position) =
      <om:OMA>{error.toNodeID(pos + 0)}
              {args.zipWithIndex.map({case (a,i) => a.toNodeID(pos+(i+1))})}
      </om:OMA> % pos.toIDAttr
   def ^ (sub : Substitution) = OME(error ^ sub, args.map(_ ^ sub))
   
   def toCML = <m:apply>{error.toCML}{args.map(_.toCML)}</m:apply>
}

/**
 * An OMATTR represents an attributed term.
 * @param arg the term without attribution
 * @param key the key (This must be an OMS in OpenMath.)
 * @param value the value
 */
case class OMATTR(arg : Term, key : OMID, value : Term) extends Term {
   def head = key.head
   def role = Role_attribution
   def components = List(key, arg, value)
   override def strip = arg.strip
   override def toString = "{" + arg + " : " + key + " -> " + value + "}"
   def toNodeID(pos : Position) = 
      <om:OMATTR><om:OMATP>{key.toNodeID(pos+0)}{value.toNodeID(pos+2)}</om:OMATP>
                 {arg.toNodeID(pos+1)}
      </om:OMATTR> % pos.toIDAttr
   def ^ (sub : Substitution) = OMATTR(arg ^ sub, key ^ sub, value ^ sub)
   def toCML = <m:apply><csymbol>OMATTR</csymbol>{arg.toCML}{key.toCML}{value.toCML}</m:apply>

}

/**
 * An OMFOREIGN represents an OpenMath foreign object.
 * @param node the XML element holding the foreign object
 */
case class OMFOREIGN(node : Node) extends Term {
   def head = None
   def role = Role_foreign
   def components = List(XMLLiteral(node))
   def toNodeID(pos : Position) = <om:OMFOREIGN>{node}</om:OMFOREIGN> % pos.toIDAttr
   def ^(sub : Substitution) = this
   def toCML = <m:apply><m:csymbol>OMFOREIGN</m:csymbol>{Node}</m:apply>
}
/*
 * OpenMath values are integers, floats, and strings (we omit byte arrays); we add URIs
 * 
 */
abstract class OMLiteral extends Term {
   def head = None
   def role = Role_value
   def components = List(StringLiteral(value.toString))
   val value : Any
   def tag: String
   override def toString = value.toString
   def toNodeID(pos : Position) = scala.xml.Elem("om", tag, pos.toIDAttr, scala.xml.TopScope, scala.xml.Text(value.toString)) 
   def ^(sub : Substitution) = this   
}
case class OMI(value : BigInt) extends OMLiteral {
   def tag = "OMI"
   def toCML = <m:cn>{value}</m:cn>
}
case class OMF(value : Double) extends OMLiteral {
   def tag = "OMF"
   override def toNodeID(pos : Position) = <om:OMF dec={value.toString}/> % pos.toIDAttr
   def toCML = <m:cn>{value}</m:cn>

}
case class OMSTR(value : String) extends OMLiteral {
   def tag = "OMSTR"
   def toCML = <m:cn>{value}</m:cn>
}
case class OMURI(value: URI) extends OMLiteral {
   def tag = "OMURI"
   def toCML = <m:cn>{value}</m:cn>

}

case class OMSemiFormal(tokens: List[SemiFormalObject]) extends Term {
   def head = None
   def role = Role_value
   def components = tokens
   override def toString = tokens.map(_.toString).mkString("", " ", "")
   def toNodeID(pos : Position) = <semiformal>{tokens.map(_.toNode)}</semiformal> 
   def ^(sub : Substitution) = {
      val newtokens = tokens map {
         case Formal(t) => Formal(t ^ sub)
         case i => i
      }
      OMSemiFormal(newtokens)
   }
   def toCML = <m:apply><csymbol>OMSemiFormal</csymbol>tokens.map(_.toCML)</m:apply>

}

case class Index(seq : Sequence, term : Term) extends Term {
	def head = term.head //TODO Sequence does not have head
	def role = Role_index
	def components = List(seq,term)
	def toNodeID(pos : Position) = <om:OMNTH>{seq.toNodeID(pos + 0)}{term.toNodeID(pos + 1)}</om:OMNTH> //TODO Change order of index and sequence 
	//<index>{seq.toNodeID(pos + 0)}{term.toNodeID(pos + 1)}</index>
	def toCML(pos : Position) = <m:nth>{seq.toNodeID(pos + 0)}{term.toNodeID(pos + 1)}</m:nth> //TODO Change order of index and sequence
	def toCML = null
	def ^(sub : Substitution) = Index(seq ^ sub, term ^ sub)
}

/*
 def toNodeID(pos : Position) : scala.xml.Node
   /** applies a substitution to an object (computed immediately) */
   def ^ (sub : Substitution) : Obj
   def head : Option[Path]
   def role : Role
   def components : List[Content]
 */

sealed abstract class Sequence extends Obj {
	def ^(sub : Substitution) : Sequence
	def items : List[SeqItem]
   def flatMap(f: SeqItem => Sequence) : Sequence = SeqItemList((items map {i => f(i).items}).flatten) 
   def toOpenMath : Term = OMA(OMID(mmt.seq), items)
}

sealed abstract class SeqItem extends Sequence {
   def items = List(this)
}

/*
case class SeqSubst(expr : Term, name : String, seq : Sequence) extends SeqItem {
	def toNodeID(pos : Position)= 		
	    <seqsubst var ={name}>{expr.toNodeID(pos + 0)}{seq.toNodeID(pos + 2)}</seqsubst> % pos.toIDAttr
	def toCML(pos : Position) = 
	    <m:seqsubst var ={name}>{expr.toNodeID(pos + 0)}{seq.toNodeID(pos + 2)}</m:seqsubst> % pos.toIDAttr
	def ^ (sub : Substitution) = {
	    	val subn = sub ++ (name / OMV(name)) 
	    	SeqSubst(expr ^ subn,name,seq ^ sub)  //TODO Variable capture
	    }
	def toCML = null
	def head = expr.head
	def role = Role_seqsubst
	def components = List(expr,StringLiteral(name),seq)
	
}
*/

case class SeqSubst(seq1 : Sequence, name : String, seq2 : Sequence) extends SeqItem {
	def toNodeID(pos : Position)= 		
	    <seqsubst var ={name}>{seq1.toNodeID(pos + 0)}{seq2.toNodeID(pos + 2)}</seqsubst> % pos.toIDAttr
	def toCML = 
	    <m:seqsubst var={name}>{seq1.toCML}{seq2.toCML}</m:seqsubst>
	def ^ (sub : Substitution) = {
	    	val subn = sub ++ (name / OMV(name)) 
	    	SeqSubst(seq1 ^ subn,name,seq2 ^ sub)  //TODO Variable capture
	    }
	def head = seq1.head
	def role = Role_seqsubst
	def components = List(seq1,StringLiteral(name),seq2)
	
}
case class SeqVar(name : String) extends SeqItem {
	def toNodeID(pos : Position) =
		<om:OMSV name ={name}/> % pos.toIDAttr
	def toCML =
		<m:si>{name}</m:si> 
	def /(s : Sequence) = SeqSub(name, s)
	def ^(sub : Substitution) =
	   sub(name) match {
	  	   case Some(t : Sequence) => t
	  	   case Some(_) => throw SubstitutionUndefined(name, "substitution is applicable but does not provide a sequence")
	  	   case None => this
       }
	def head = None
	def role = Role_SeqVariableRef
	def components : List[Content] = List(StringLiteral(name))
}

case class SeqUpTo(num : Term) extends SeqItem {
	def toNodeID(pos : Position) =
		<om:OMNATS>{num.toNodeID(pos + 0)}</om:OMNATS> % pos.toIDAttr
   def toCML =
		<m:nats>{num.toCML}</m:nats>
	def ^(sub : Substitution) = SeqUpTo(num ^ sub)
	def head = num.head
	def role = Role_sequpto
	def components : List[Content] = List(num)
}

case class SeqItemList(items: List[SeqItem]) extends Sequence {
   def toNodeID(pos : Position) =
	   <sequence>{items.zipWithIndex map {x => x._1.toNodeID(pos + x._2)}}</sequence> % pos.toIDAttr
   def toCMLdeID(pos : Position) =
	   <m:seq>{items.zipWithIndex map {x => x._1.toNodeID(pos + x._2)}}</m:seq> % pos.toIDAttr
   def toCML = <m:seq>{items.map(_.toCML)}</m:seq>
   def ^(sub : Substitution) : Sequence = SeqItemList(items.map(_ ^ sub).flatMap(_.items))
   def components :List[Content] = items
   def head = None
   def role = Role_seqitemlist
}

/**
 * A ModuleObj is a composed module level expressions.
 */
sealed trait ModuleObj extends Obj {
	/**
	 * returns Some(p) iff this module object refers to a real declaration with path p
	 */
	def asPath : Option[MPath] = None
	def %(n: LocalName) = GlobalName(this, n)
	/** converts the object into an MPath */
	def toMPath : MPath = utils.mmt.mmtbase ? toString
}

/**
 * A TheoryObj represents a Theory object.
 * Theory objects are to theory modules as terms are to constants: Theory modules introduce named theories,
 * theory objects refer to theory modules or to composed theories.
 * References to theory modules are the only possible theory objects.
 */
sealed trait TheoryObj extends ModuleObj {
   def ^ (sub : Substitution) : TheoryObj = this
   def id = OMIDENT(this)
}

/**
 * A Morph represents a morphism.
 * Morphisms are to links as terms are to constants.
 * Morphisms are formed from links, identity, and composition.
 */
sealed trait Morph extends ModuleObj {
   /**
    * composition in diagram order
    * no nested OMCOMPs are introduced 
    * @param that the "argument" morphism
    * @return the composition <code>that</code> ; <code>this</code>
    */
   def *(that : Morph) : Morph = that match {
      case that : OMCOMP => OMCOMP(this :: that.morphisms)
      case _ => OMCOMP(this, that)
   }
   //morphisms do not contain free variables
   def ^ (sub : Substitution) : Morph = this
}

sealed trait AtomicMorph extends Morph

/**
 * An OMMOD represents a reference to a module (which may be a theory or a morphism expression).
 * @param path the path to the link, structures are referenced by their module level path
 */
case class OMMOD(path : MPath) extends TheoryObj with AtomicMorph {
   //need to override the methods for which two implementations are inherited
   override def ^(sub : Substitution) : OMMOD = this
   override def toMPath = path
   def head = Some(path)
   def role = Role_ModRef
   def components = path.components
   def toNodeID(pos : Position) = <om:OMS cdbase={path.^^.toPath} cd={path.name.flat}/> % pos.toIDAttr
   def toCML = <m:csymbol>{path.toPath}</m:csymbol>
   def links = List(path) 
   override def asPath = Some(path)
   override def toString = path.toPath
}

case class TEmpty(meta: Option[MPath]) extends TheoryObj with MMTObject {
   def args = meta match {case Some(m) => List(OMMOD(m)) case None => Nil}
   def path = mmt.tempty
}

case class TUnion(left: TheoryObj, right: TheoryObj) extends TheoryObj with MMTObject {
   def args = List(left,right)
   def path = mmt.tunion
   override def toString = left.toString + " + " + right.toString
}
object TUnion {
   def apply(thys: List[TheoryObj]) : TheoryObj = thys match {
      case Nil => TEmpty(None)
      case hd :: Nil => hd
      case hd :: tl => TUnion(hd, apply(tl))
   }
}

/*
case class OMPI(push: TheoryObj, along: Morph, wth: TheoryObj) extends TheoryObj with ComposedModuleObject {
   def role = Role_pushout_i
   def components = List(push, along, wth)
   override def toString = push.toString + " + " + along.toString + " with " + wth.toString 
}
*/

case class OMDL(cod: TheoryObj, name: LocalName) extends AtomicMorph {
   def path = cod % name
   def head : Option[Path] = Some(path)
   def role = cod match {
      case OMMOD(_) => Role_StructureRef
      case _ => Role_ComplexConstantRef
   }
   def components = OMID(cod % name).components
   def toNodeID(pos : Position) = OMID(path).toNodeID(pos)
   def toCML = <m:csymbol>{path.toPath}</m:csymbol>
   override def asPath = path match {
      case OMMOD(p) % LocalName(List(NamedStep(n))) => Some(p / n)
      case _ => None
   }
   override def toString = path.toString
}

/**
 * An OMComp represents the associative composition of a list of morphisms. Compositions may be nested.
 * @param morphisms the list of morphisms in diagram order
 */
case class OMCOMP(morphisms: List[Morph]) extends Morph with MMTObject {
   def args = morphisms
   def path = mmt.composition
   override def toString = morphisms.mkString("", " ; ", "")
}
object OMCOMP {
   def apply(ms: Morph*) : OMCOMP = OMCOMP(ms.toList)
}

/**
 * An OMIDENT represents the identity morphism of a theory.
 * @param theory the theory
 */
case class OMIDENT(theory : TheoryObj) extends Morph with MMTObject {
   def args = List(theory)
   def path = mmt.identity
}

case class MEmpty(from: TheoryObj, to: TheoryObj) extends Morph with MMTObject {
   def args = List(from,to)
   def path = mmt.mempty
}

case class MUnion(left: Morph, right: Morph) extends Morph with MMTObject {
   def args = List(left, right)
   def path = mmt.munion
   override def toString = left.toString + " + " + right.toString
}
object MUnion {
   def apply(mors: List[Morph]) : Morph = mors match {
      case Nil => throw ImplementationError("union of 0 morphisms")
      case hd :: Nil => hd
      case hd :: tl => MUnion(hd, apply(tl))
   }
}

/*
case class OMUNIONPIM(push: Morph, along: Morph, wth: Morph) extends Morph with ComposedModuleObject {
   def role = Role_pushout_im
   def components = List(push, along, wth)
   override def toString = push.toString + " + " + along.toString + " with " + wth.toString 
}

case class OMPIW(raise: Morph, to: TheoryObj) extends Morph with ComposedModuleObject {
   def role = Role_pushout_iim
   def components = List(raise, to)
   override def toString = raise.toString + " * " + to.toString
}
*/

/**
 * Obj contains the parsing methods for objects.
 * There is one parsing method for each syntactic class of objects: terms, morphisms, theories.
 */
object Obj {
   //read basattr and compute new base
   private def newBase(N : Node, base : Path) : Path = {
      val b = xml.attr(N,"base")
      if (b == "") base else Path.parse(b, base)
   }
   //base: base reference, may be set by any object
   //lib: optional library against which to validate and parse paths
   /** parses a term relative to a base address */
   def parseTerm(N : Node, base : Path) : Term = {
      //N can set local cdbase attribute; if not, it is copied from the parent
      val nbase = newBase(N, base)
      //this function unifies the two cases for binders in the case distinction below
      def doBinder(binder : Node, context : Node, condition : Option[Node], body : Node) = {
         val bind = parseTerm(binder, nbase)
         val cont = Context.parse(context, base)
         if (cont.isEmpty)
            throw new ParseError("at least one variable required in " + cont.toString)
         val cond = condition.map(parseTerm(_, nbase))
         val bod = parseTerm(body, nbase)
         OMBINDC(bind, cont, cond, bod)
      }
      N match {
         case <OMS/> =>
            parseOMS(N, base) match {
               case p : GlobalName => OMID(p)
               case p => throw new ParseError("Not a term: " + p + N.toString)
            }
         case <OMV/> =>
            OMV(xml.attr(N,"name"))
         case <OMA>{child @ _*}</OMA> if child.length == 3 && parseOMS(child.head, base) == mmt.morphismapplication =>
            val arg = parseTerm(child(1), nbase)
            val morph = parseMorphism(child(2), nbase)
            OMM(arg, morph)
         case <OMA>{child @ _*}</OMA> =>
            if (child.length <= 1)
               throw ParseError("No arguments given: " + N.toString)
            val fun = parseTerm(child.head, base)
            val args = child.tail.toList.map(parseSeqItem(_, nbase))
            OMA(fun, args)
         case <OME>{child @ _*}</OME> =>
            val ch = child.toList.map(parseTerm(_, nbase))
            OME(ch.head,ch.tail)
         case <OMBIND>{binder}{context}{condition}{body}</OMBIND> =>
           	doBinder(binder, context, Some(condition), body)
         case <OMBIND>{binder}{context}{body}</OMBIND> =>
            doBinder(binder, context, None, body)
         case <OMATTR><OMATP>{key}{value}</OMATP>{rest @ _*}</OMATTR> =>
            val k = parseTerm(key, nbase)
            if (! k.isInstanceOf[OMID])
               throw new ParseError("key must be OMS in " + N.toString)
            val v = parseTerm(value, nbase)
            if (rest.length == 0)
               throw ParseError("not a well-formed attribution: " + N.toString)
            val n = if (rest.length == 1)
               rest(0) else <OMATTR>{rest}</OMATTR>
            val t = parseTerm(n, nbase)
            OMATTR(t, k.asInstanceOf[OMID], v)
         case <OMFOREIGN>{_*}</OMFOREIGN> => OMFOREIGN(N)
         case <OMI>{i}</OMI> => OMI(BigInt(i.toString))
         case <OMSTR>{s @ _*}</OMSTR> => OMSTR(s.text)
         case <OMF/> => OMF(xml.attr(N, "dec").toDouble) //TODO hex encoding
         case <OMNTH>{s}{i}</OMNTH> => Index(parseSequence(s, base), parseTerm(i,base))
         case <OMOBJ>{o}</OMOBJ> => parseTerm(o, nbase)
         case _ => throw ParseError("not a well-formed term: " + N.toString)
      }
   }
   def parseSeqItem(N: Node, base: Path) : SeqItem = N match {
      case <seqsubst>{e}{s}</seqsubst> =>
         SeqSubst(parseTerm(e, base), xml.attr(N, "var"), parseSequence(s, base))
      case <OMSV>{c @ _*}</OMSV> => SeqVar(xml.attr(N, "name"))
      case <OMNATS>{e}</OMNATS> => SeqUpTo(parseTerm(e, base))
      case t => parseTerm(t, base)
      // case _ => throw ParseError("not a well-formed sequence item: " + N.toString)
   }
   def parseSequence(N: Seq[Node], base: Path) : Sequence = N match {
      case <sequence>{its @ _*}</sequence> =>
         val items = its.map {parseSeqItem(_, base)}
         SeqItemList(items.toList)
      case i : scala.xml.Elem => parseSeqItem(i,base) //throw ParseError("not a well-formed sequence: " + N.toString)
   }
   
   /** parses a morphism relative to a base address */
   def parseMorphism(N : Node, base : Path) : Morph = {
      //N can set local cdbase attribute; if not, it is copied from the parent
      val nbase = newBase(N, base)
      N match {
         case <OMOBJ>{mor}</OMOBJ> => parseMorphism(mor, base)
         case <OMMOR>{mor}</OMMOR> => parseMorphism(mor, base)
         case <OMS/> =>
            parseOMS(N, base) match {
               case p : MPath => OMMOD(p)
               case (t: TheoryObj) % name => OMDL(t, name)
/*               case doc ? mod ?? str =>
                  throw ParseError("not a well-formed link reference (case was removed): " + N.toString)
                  val p = doc ? (mod / str)
                  callback(p)
                  OMMOD(p) */
               case _ => throw ParseError("not a well-formed link reference: " + N.toString)
            }
         case <OMA>{child @ _*}</OMA> if child.length >= 2 && parseOMS(child.head, base) == mmt.composition =>
            val links = child.toList.tail.map(parseMorphism(_, nbase))
            OMCOMP(links)
         case <OMA>{child @ _*}</OMA> if child.length == 2 && parseOMS(child(0), base) == mmt.identity =>
            val theory = parseTheory(child(1), nbase)
            OMIDENT(theory)
         case <OMA>{child @ _*}</OMA> if child.length >= 2 && parseOMS(child.head, base) == mmt.munion =>
            val mors = child.toList.tail.map(parseMorphism(_, nbase))
            MUnion(mors)
         case _ => throw ParseError("not a well-formed morphism: " + N.toString)
      }
   }

  /** parses a theory object relative to a base address */
  def parseTheory(N : Node, base : Path) : TheoryObj = {
     val nbase = newBase(N, base)
     N match {
        case <OMOBJ>{thy}</OMOBJ> => parseTheory(thy, base)
        case <OMS/> =>
           parseOMS(N, base) match {
               case p : MPath => OMMOD(p)
               case _ => throw ParseError("not a well-formed theory reference: " + N.toString)
           }
        case <OMA>{child @ _*}</OMA> if child.length >= 2 && parseOMS(child.head, base) == mmt.tunion =>
            val thys = child.toList.tail.map(parseTheory(_, nbase))
            TUnion(thys)
        case _ => throw ParseError("not a well-formed theory: " + N.toString)
    }
  }
  
  def parseOMS(N : Node, base : Path) : Path = {
     val doc = URI(xml.attr(N,"base"))
     val mod = xml.attr(N,"module")
     val name = xml.attr(N,"name")
     Path.parse(doc, mod, name, base)
  }
}

object Morph {
	def domain(m : Morph)(implicit lib : Lookup) : TheoryObj = m match {
      case OMIDENT(t) => t
      case OMCOMP(m :: _) => domain(m)
      case OMCOMP(Nil) => throw ImplementationError("cannot infer domain of empty composition")
      case MEmpty(f,_) => f
      case MUnion(l,r) => TUnion(domain(l), domain(r))
      case OMMOD(path) => try {
         lib.get(path) match {case l: Link => l.from}
      } catch {
         case _ => throw Invalid("not a well-formed morphism: " + m)
      }
      case OMDL(h,n) => try {
         lib.getStructure(h % n).from
      } catch {
         case _ => throw Invalid("not a well-formed morphism: " + m)
      }
    }

   def codomain(m : Morph)(implicit lib : Lookup) : TheoryObj = m match {
      case OMIDENT(t) => t
      case OMCOMP(Nil) => throw ImplementationError("cannot infer codomain of empty composition")
      case OMCOMP(l) => codomain(l.last)
      case MEmpty(_,t) => t
      case MUnion(l,r) =>
         val lc = codomain(l)
         val rc = codomain(r)
         if (lc == rc) lc
         else TUnion(lc, rc)
      case OMMOD(path) => try {
         lib.get(path) match {case l: Link => l.to}
      } catch {
         case _ => throw Invalid("not a well-formed morphism: " + m)
      }
      case OMDL(t, _ ) => t
   }
}