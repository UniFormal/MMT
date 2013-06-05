package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
import utils._
import libraries._
import modules._
import metadata._
import presentation._
import Conversions._

import scala.xml.{Node}
import scala.collection.mutable.ListMap

/** a trait to be mixed into Obj 
 * it provides a stateful key-value map for storing arbitrary information
 *
 * This trait introduces state into the stateless Obj case classes and should be used cautiously.
 * For example, x match {case x => x} preserves all client properties while
 * x match {case OMS(p) => OMS(p)} deletes all client properties. 
 * Software components should only access properties they put themselves or
 * that are explicitly guaranteed by other components.
 * 
 * While a bad idea in most situations, client properties are very useful occasionally.
 * Example applications:
 *   - UOM uses a property to remember whether a Term has been simplified already
 *   - UOM uses a property to remember what kind of change a simplification has produced
 */
trait ClientProperties {
   lazy val clientProperty = new ListMap[URI, Any]
}

/**
 * An Obj represents an MMT object. MMT objects are represented by immutable Scala objects.
 */
abstract class Obj extends Content with ontology.BaseType with HasMetaData with ClientProperties {
   /** prints to OpenMath (with OMOBJ wrapper) */
   def toNodeID(pos : Position) : scala.xml.Node
   def toNode : scala.xml.Node = toNodeID(Position.Init)
   def toOBJNode = {
      val om = xml.namespace("om") // inlining this into XML literal does not work
      <om:OMOBJ xmlns:om={om}>{toNode}</om:OMOBJ>
   }
   def toCML: Node
   /** applies a substitution to an object (computed immediately)
    *  capture is avoided by renaming bound variables that are free in sub */
   // TODO use an internal auxiliary method for subsitution that carries along a precomputed list of free variables in sub to avoid recomputing it at every binder
   // alternatively, make freeVars a lazy val in Substitution
   def ^ (sub : Substitution) : Obj
   /** the free variables of this object in any order */ 
   def freeVars : List[LocalName] = freeVars_.distinct
   /** helper function for freeVars that computes the free variables without eliminating repetitions */ 
   private[objects] def freeVars_ : List[LocalName]
   /** returns the subobject at a given position and its context; first index of pos is always 0 and is ignored */
   def subobject(pos: Position) : (Context, Obj) =
     pos.indices.tail.foldLeft((Context(),this)) {
         case ((con,obj), i) =>
            val newContext = obj match {
               case OMBINDC(_,context,_) => con ++ context.take(i)
               case _ => con
            }
            obj.components(i) match {
               case o : Obj => (newContext, o)
               case _ => throw GetError("position " + pos + " not valid in " + this)
            }
      }
   def head : Option[ContentPath]
   /** the governing path required by Content is the head, if any */
   def governingPath = head
   /** replaces metadata of this with those of o
    * 
    * @param o the original object
    * call o2.copyFrom(o1) after transforming o1 into o2 in order to preserve metadata 
    */
   def copyFrom(o: Obj) {
      metadata = o.metadata
   }
}

trait MMTObject {
   def args : List[Obj]
   def components = OMID(path) :: args
   val path : GlobalName
   def head = Some(path)
   def role = if (args.isEmpty) Role_ConstantRef else Role_application(None)
   def toNodeID(pos : Position) =
      if (args.isEmpty) OMID(path).toNode
      else
      <om:OMA>{components.zipWithIndex.map({case (m,i) => m.toNodeID(pos/i)})}
      </om:OMA> % pos.toIDAttr
   def toCML = 
     if (args.isEmpty) <m:csymbol>{path.toPath}</m:csymbol>
     else <m:apply>{components.zipWithIndex.map({case (m,i) => m.toCML})}</m:apply> 
}

/**
 * A Term represents an MMT term.
 */
sealed abstract class Term extends Obj {
   def strip : Term = this
   def ^(sub : Substitution) : Term
   def ^?(sub : Substitution) : Term = if (sub.isIdentity) this else this ^ sub
   /** morphism application (written postfix), maps OMHID to OMHID */
   def *(that : Term) : Term = OMM(this, that)
   /** permits the intuitive f(t_1,...,t_n) syntax for term applications */
   def apply(args : Term*) = OMA(this, args.toList)
   /** the syntactically computed MPath of the term.
     * If the term is module level, this is guaranteed to be correct. */
   def toMPath : MPath = this match {
     case OMMOD(p) => p
     case _ => mmt.mmtbase ? Obj.toPathEncoding(this)
   }
   /** This permits the syntax term % sym for path composition */
   def %(n: LocalName) : GlobalName = GlobalName(this,n)
   def %(n: String) : GlobalName = GlobalName(this,LocalName(n))
   /** applies copyFrom and returns this
    * 
    * @return this object but with the metadata from o
    */
   def from(o: Term): Term = {
      copyFrom(o)
      this
   }
}

/**
 * OMHID represents the hidden term.
 */
case object OMHID extends Term with MMTObject {
   val path = mmt.mmtsymbol("hidden")
   def args = Nil
   def ^(sub : Substitution) = this
   private[objects] def freeVars_ = Nil
   override def *(that: Term) = this
}

/**
 * An OMID represents an MMT reference
 */
case class OMID(path: ContentPath) extends Term {
   def head = Some(path)
   def ^(sub : Substitution) = this
   private[objects] def freeVars_ = Nil
   def role = path match {
      case m: MPath => Role_ModRef
      case OMMOD(p) % _ => Role_ConstantRef
      case thy % name => Role_ComplexConstantRef
   }
   def components = path match {
      case m: MPath => m.components
      case OMMOD(doc ? mod) % ln => List(StringLiteral(doc.toPath), StringLiteral(mod.flat),
                                 StringLiteral(ln.toString), StringLiteral(path.toPathEscaped))
      case thy % name => List(thy, StringLiteral(name.toString))
   }
   override def toString = path match {
      case doc ? mod => doc + "?" + mod.flat
      case OMMOD(mod) % name => mod.name.flat + "?" + name.toString
      case thy % name => "[" + thy.toString + "]?" + name.toString
   }
   def toNodeID(pos : Position) = path match {
      case doc ? mod => <om:OMS base={doc.toPath} module={mod.toPath}/> % pos.toIDAttr
      case OMMOD(doc ? mod) % name => <om:OMS base={doc.toPath} module={mod.flat} name={name.toPath}/> % pos.toIDAttr
      case thy % name => <om:OMS name={name.toPath}>{thy.toNodeID(pos / 0)}</om:OMS> % pos.toIDAttr
   }
   def toCML = <csymbol>{path.toPath}</csymbol>   //TODO ContentMathML syntax for complex identifiers
}

object OMS {
   def apply(p: GlobalName) = OMID(p)
   def unapply(t: Term) : Option[GlobalName] = t match {
     case OMID(p : GlobalName) => Some(p)
     case _ => None
   }
}

/**
 * An OMBINDC represents a binding with condition
 * @param binder the binder
 * @param context the bound variables (from outside to inside)
 * @param scopes the scopes/bodies/matrices of the binder (usually exactly 1)
 */
case class OMBINDC(binder : Term, context : Context, scopes: List[Term]) extends Term  {
   def head = binder.head
   val numVars = context.variables.length
   def components = binder :: context.toList ::: scopes
   def role = Role_binding
   def toNodeID(pos : Position) = 
      <om:OMBIND>{binder.toNodeID(pos / 0)}
                 {context.toNodeID(pos)}
                 {scopes.zipWithIndex.map {
                    case (s,i) => s.toNodeID(pos / (numVars + i + 1))
                 }}
      </om:OMBIND> % pos.toIDAttr
   override def toString = "(" + binder + " [" + context + "] " + scopes.map(_.toString).mkString(" ") + ")"  
   def ^(sub : Substitution) = {
      val (newCon, alpha) = Context.makeFresh(context, sub.freeVars)
      val subN = sub ++ alpha
      OMBINDC(binder ^ sub, newCon ^ sub, scopes.map(_ ^ subN)).from(this)
   }
   private[objects] def freeVars_ = binder.freeVars_ ::: context.freeVars_ ::: scopes.flatMap(_.freeVars_).filterNot(x => context.isDeclared(x))      
   def toCML = <m:apply>{binder.toCML}{context.toCML}{scopes.map(_.toCML)}</m:apply>
}

/**
 * OMBIND represents a binding without condition
 */
object OMBIND {
	def apply(binder : Term, context : Context, body : Term) = OMBINDC(binder, context, List(body))
	def unapply(t : Term): Option[(Term,Context,Term)] = t match {
		case OMBINDC(b,c,List(s)) => Some((b,c,s))
		case _ => None
	}
}

/**
 * An OMM represents a morphism application to a term
 * @param arg the argument term
 * @param via the applied morphism
 */
case class OMM(arg : Term, via : Term) extends Term with MMTObject {
   val path = mmt.morphismapplication
   def args = List(arg,via)
   def ^ (sub : Substitution) = OMM(arg ^ sub, via ^ sub).from(this)
   private[objects] def freeVars_ = arg.freeVars_ ::: via.freeVars_
}

/**
 * An OMA represents an application of a term to a list of terms
 * @param fun the function term
 * @param args the list of argument terms
 */
case class OMA(fun : Term, args : List[Term]) extends Term {
   def head = fun.head
   def role = Role_application(None)
   def components = fun :: args
   override def toString = (fun :: args).map(_.toString).mkString("(", " ", ")")
   def toNodeID(pos : Position) =
      <om:OMA>{fun.toNodeID(pos / 0)}
              {args.zipWithIndex.map({case (a,i) => a.toNodeID(pos/(i+1))})}
      </om:OMA> % pos.toIDAttr
   def ^ (sub : Substitution) = OMA(fun ^ sub, args.map(_ ^ sub)).from(this)
   private[objects] def freeVars_ = fun.freeVars_ ::: args.flatMap(_.freeVars_)
   def toCML = <m:apply>{fun.toCML}{args.map(_.toCML)}</m:apply>
}

/**
 * An OMV represents a reference to a variable.
 * @param name the name of the referenced variable
 */
case class OMV(name : LocalName) extends Term {
   def head = None
   def role = Role_VariableRef
   def components = List(StringLiteral(name.toString)) 
   /** the substutition this/s */
   def /(s : Term) = Sub(name, s)
   /** the declaration this:tp */
   def %(tp : Term) = VarDecl(name, Some(tp), None)
   def toNodeID(pos : Position) = <om:OMV name={name.toPath}/> % pos.toIDAttr
   override def toString = name.toString
   def ^(sub : Substitution) =
	   sub(name) match {
	  	   case Some(t) => t match {
	  	      //substitution introduces structure-sharing if the same variable occurs more than once
	  	      //that is normally useful for efficiency
	  	      //but it the two occurrences carry different metadata (in particular, different source-references), it is undesirable
	  	      //for the most important case of variable renamings, the OMV case below avoids structure sharing and preserves metadata 
	  	      case OMV(x) => OMV(x).from(this)
	  	      case _ => t
	  	   }
	  	   case None => this
       }
   private[objects] def freeVars_ = List(name)
   def toCML = <m:ci>{name.toPath}</m:ci>
}

object OMV {
   def apply(n: String) : OMV = OMV(LocalName(n))
}

/**
 * An OME represents an application of an error-constructing term to a list of terms
 * @param error the error-constructor
 * @param args the list of argument terms
 */
case class OME(error : Term, args : List[Term]) extends Term {
   def head = error.head
   def role = Role_application(None)
   def components = error :: args
   def toNodeID(pos : Position) =
      <om:OMA>{error.toNodeID(pos / 0)}
              {args.zipWithIndex.map({case (a,i) => a.toNodeID(pos/(i+1))})}
      </om:OMA> % pos.toIDAttr
   def ^(sub : Substitution) = OME(error ^ sub, args.map(_ ^ sub)).from(this)
   private[objects] def freeVars_ = error.freeVars_ ::: args.flatMap(_.freeVars_)   
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
      <om:OMATTR><om:OMATP>{key.toNodeID(pos/0)}{value.toNodeID(pos/2)}</om:OMATP>
                 {arg.toNodeID(pos/1)}
      </om:OMATTR> % pos.toIDAttr
   def ^ (sub : Substitution) = OMATTR(arg ^ sub, key, value ^ sub).from(this)
   private[objects] def freeVars_ = arg.freeVars_ ::: value.freeVars_
   def toCML = <m:apply><csymbol>OMATTR</csymbol>{arg.toCML}{key.toCML}{value.toCML}</m:apply>

}

case class OMREF(uri: URI, var value: Option[Term] = None) extends Term {
   def ^(sub: Substitution) = this
   private[objects] def freeVars_ = Nil
   def isDefined = value.isDefined
   def set(t: Term) {value = Some(t)}
   def head = value flatMap {_.head}
   def role = Role_reference
   def components = List(StringLiteral(uri.toString), value.getOrElse(Omitted))
   override def toString = uri.toString
   def toNodeID(pos: Position) = <om:OMREF uri={uri.toString}/>
   def toCML = <m:ref uri={uri.toString}/>
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
   private[objects] def freeVars_ = Nil
   def toCML = <m:apply><m:csymbol>OMFOREIGN</m:csymbol>{Node}</m:apply>
}
/** OpenMath literals
 *  integers, floats, and strings (we omit byte arrays)
 *  we add URIs
 */
abstract class OMLiteral extends Term {
   def head = None
   def role = Role_value
   def components = List(StringLiteral(value.toString))
   val value : Any
   def tag: String
   override def toString = value.toString
   def toNodeID(pos : Position) = scala.xml.Elem("om", tag, pos.toIDAttr, scala.xml.TopScope, true, scala.xml.Text(value.toString)) 
   def ^(sub : Substitution) = this
   private[objects] def freeVars_ = Nil
}

/** An integer literal */
case class OMI(value : BigInt) extends OMLiteral {
   def tag = "OMI"
   def toCML = <m:cn>{value}</m:cn>
}

/** A float literal */
case class OMF(value : Double) extends OMLiteral {
   def tag = "OMF"
   override def toNodeID(pos : Position) = <om:OMF dec={value.toString}/> % pos.toIDAttr
   def toCML = <m:cn>{value}</m:cn>

}

/** A string literal */
case class OMSTR(value : String) extends OMLiteral {
   def tag = "OMSTR"
   def toCML = <m:cn>{value}</m:cn>
}

/** A literal object representing a URI (not part of the OpenMath standard) */
case class OMURI(value: URI) extends OMLiteral {
   def tag = "OMURI"
   def toCML = <m:cn>{value}</m:cn>

}

//TODO: could this be merged with presentation.Literal?
/** An OMSemiFormal represents a mathematical object that mixes formal and informal components */
case class OMSemiFormal(tokens: List[SFObjectElem]) extends Term with SFContentElemList {
   def head = None
   def role = Role_value
   def ^(sub : Substitution) = {
      val newtokens = tokens map {
         case Formal(t) => Formal(t ^ sub)
         case i => i
      }
      OMSemiFormal(newtokens).from(this)
   }
   private[objects] def freeVars_ = tokens.flatMap(_.freeVars)
   def toCML = <m:apply><csymbol>OMSemiFormal</csymbol>tokens.map(_.toCML)</m:apply>
}

object OMSemiFormal {
  def apply(tokens: SFObjectElem*) : OMSemiFormal = OMSemiFormal(tokens.toList)
}

/** provides apply/unapply methods for constructing and pattern-matching a function that is may or may not be applied to arguments
 *  Nil is used as the list of arguments in the latter case
 *  This identifies t with OMA(t,Nil).
 */
object OMAMaybeNil {
   def apply(f: Term, args: List[Term]) = if (args == Nil) f else OMA(f,args)
   def unapply(t: Term) : Option[(Term,List[Term])]= t match {
      case OMA(f, args) => Some((f,args))
      case _ => Some((t, Nil))
   }
}

/**
 * ComplexTerm provides apply/unapply methods to unify OMA and OMBINDC as well as complex binder
 * 
 * It does not subsume the OMID case
 */
object ComplexTerm {
   def apply(p: GlobalName, args: List[Term], con: Context, scopes: List[Term]) =
      if (args.isEmpty && con.isEmpty && scopes.isEmpty) OMS(p)
      else if (con.isEmpty && scopes.isEmpty) OMA(OMS(p), args)
      else
         if (args.isEmpty) OMBINDC(OMS(p), con,scopes)
         else OMBINDC(OMA(OMS(p), args), con, scopes)
   def unapply(t: Term) : Option[(GlobalName, List[Term], Context, List[Term])] = t match {
      //case OMS(p) => Some((p, Nil, Context(), Nil))
      case OMA(OMS(p), args) => Some(p, args, Context(), Nil)
      case OMBIND(OMS(p), con, scope) => Some((p, Nil, con, List(scope)))
      case OMBIND(OMA(OMS(p), args), con, scope) => Some((p, args, con, List(scope)))
      case _ => None
   }
}


/* this may prove useful to pattern-match through OMREF elements 
abstract class TermCompanion[A] {
   def strictUnapply(t:Term): A
   def unapply(t: Term): Option[A] = t match {
      case r: OMREF => r.value map unapply
      case t =>
         try {Some(strictUnapply(t))}
         catch {case e: ClassCastException => None}
   }
}

object OMID extends TermCompanion[ContentPath] {
   def strictUnapply(t: Term) = t.asInstanceOf[OMID].path
}
object OMA extends TermCompanion[(Term,List[Term])] {
   def strictUnapply(t: Term) = {
      val a = t.asInstanceOf[OMA]
      (t.fun,t.args)
   }
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
      def doBinder(binder : Node, context : Node, scopes : List[Node]) = {
         val bind = parseTerm(binder, nbase)
         val cont = Context.parse(context, base)
         if (cont.isEmpty)
            throw new ParseError("at least one variable required in " + cont.toString)
         val scopesP = scopes.map(parseTerm(_, nbase))
         OMBINDC(bind, cont, scopesP)
      }
      N match {
         case <OMS/> =>
            parseOMS(N, base) match {
               case p : ContentPath => OMID(p)
               case p => throw new ParseError("Not a term: " + p + " " + N.toString)
            }
         case <OMV/> =>
            OMV(LocalName.parse(xml.attr(N,"name")))
         case <OMA>{child @ _*}</OMA> if child.length == 3 && child.head.label == "OMS" && parseOMS(child.head, base) == mmt.morphismapplication =>
            val arg = parseTerm(child(1), nbase)
            val morph = parseTerm(child(2), nbase)
            OMM(arg, morph)
         case <OMA>{child @ _*}</OMA> =>
            if (child.length == 0)
               throw ParseError("No operator given: " + N.toString)
            val fun = parseTerm(child.head, base)
            val args = child.tail.toList.map(parseTerm(_, nbase))
            OMA(fun, args)
         case <OME>{child @ _*}</OME> =>
            val ch = child.toList.map(parseTerm(_, nbase))
            OME(ch.head,ch.tail)
         case <OMBIND>{binder}{context}{scopes @ _*}</OMBIND> =>
            doBinder(binder, context, scopes.toList)
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
         case <OMSF>{nodes @ _*}</OMSF> =>
            val sf = nodes.toList.map {
               case node @ <text>{scala.xml.PCData(t)}</text> => Text(xml.attr(node, "format"), t)
               case <node>{n}</node> => XMLNode(n)
               case n => Formal(parseTerm(n, nbase))
            }
            OMSemiFormal(sf)
         case <OMI>{i}</OMI> => OMI(BigInt(i.toString))
         case <OMSTR>{s @ _*}</OMSTR> => OMSTR(s.text)
         case <OMF/> => OMF(xml.attr(N, "dec").toDouble) //TODO hex encoding
         case <OMOBJ>{o}</OMOBJ> => parseTerm(o, nbase)
         case <OMMOR>{o}</OMMOR> => parseTerm(o, nbase) //TODO this should be deprecated
         case <OMTHY>{o}</OMTHY> => parseTerm(o, nbase) //TODO this should be deprecated
         case <OMREL>{o}</OMREL> => parseTerm(o, nbase) //TODO this should be deprecated
         case _ => throw ParseError("not a well-formed term: " + N.toString)
      }
   }
  
  def parseOMS(N : Node, base : Path) : Path = N match {
      case <OMS/> =>
        val doc = URI(xml.attr(N,"base"))
        val mod = xml.attr(N,"module")
        val name = xml.attr(N,"name")
        Path.parse(doc, mod, name, "", base)
      case <OMS>{n}</OMS> =>
        val mod = parseTerm(n, base)
        val name = xml.attr(N,"name")
        mod % name
      case _ => throw ParseError("not a well-formed identifier: " + N.toString)
  }
  
  /** separator string used in fromPathEncoding and toPathEncoding */
  private val sepString = "___"
  /** decodes an expression encoded by toPathEncoding
   * @throws ParseError whenever it does not understand the input
   */
  def fromPathEncoding(s: String) : Term = 
     if (s.startsWith("[") && s.endsWith("]")) 
        fromPathEncodingAux(s.substring(1, s.length - 1))
     else
        throw ParseError("not an MPath-encoded Term: " + s)

  private def fromPathEncodingAux(s: String) : Term =
     try {
        s match {
           case MPathEncodedOMS(p: ContentPath) => OMID(p)
           case MPathEncodedOMA(p: ContentPath, args) => OMA(OMID(p), args map fromPathEncodingAux)
           case _ => throw ParseError("not an MPath-encoded Term: " + s)
        }
     } catch {case e: ParseError => 
       throw ParseError("not an MPath-encoded Term: " + s).setCausedBy(e)
     }
  
  /** encodes a theory or morphism expressions as a String */
  def toPathEncoding(t: Term) = "[" + toPathEncodingAux(t) + "]"
  private def toPathEncodingAux(t: Term): String = t match {
     case OMA(fun, args) => (fun::args).map(toPathEncodingAux).mkString("(", sepString, ")")
     case OMID(p) => p.toPath
     case _ => throw ImplementationError("path encoding only available for theory expressions")
  }
  private object MPathEncodedOMA {
      def unapply(s: String): Option[(Path,List[String])] =
         if (s.startsWith("(") && s.endsWith(")")) {
            val parts = s.substring(1,s.length - 1).split(sepString).toList
            val op = Path.parse(parts.head)
            Some((op, parts.tail))
         }
         else
           None
   }
   private object MPathEncodedOMS {
      def unapply(s: String): Option[Path] =
         if (! s.contains(sepString)) {
            val op = Path.parse(s)
            Some(op)
         }
         else
           None
   }
}



