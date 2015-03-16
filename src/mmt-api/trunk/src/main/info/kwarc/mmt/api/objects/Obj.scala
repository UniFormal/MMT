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

/** convenience to create get and put methods for objects with ClientProperties
 *  @param property the client property for which to generate get and put methods
 */
class TermProperty[A](val property: utils.URI) {
   /** put the client property */
   def put(t: Obj, a:A) {
      t.clientProperty(property) = a
   }
   /** get the client property if defined */
   def get(t: Term): Option[A] = t.clientProperty.get(property) match {
      case Some(a: A) => Some(a) // :A is unchecked but true if put was used to set the property
      case None => None
      case Some(_) =>
         throw ImplementationError("client property has bad type") // impossible if put was used
   }
   def erase(t: Term) {
      t.clientProperty -= property
   }
}

object TermProperty {
   /** removes all term properties */
   def eraseAll(t: Term) {
      t.clientProperty.clear
      t match {
         case ComplexTerm(_, subs, cont, args) =>
            subs.foreach {s => eraseAll(s.target)}
            cont.foreach {v => 
               v.tp foreach eraseAll
               v.df foreach eraseAll
            }
            args foreach eraseAll
         case _ =>
      }
   }
}

/**
 * An Obj represents an MMT object. MMT objects are represented by immutable Scala objects.
 */
abstract class Obj extends Content with ontology.BaseType with HasMetaData with ClientProperties with HashEquality[Obj] {
   /** the type of this instance
    *  
    *  This is needed to provide sharper return types for inductive functions,
    *  e.g., substitution returns Term if applied to Term, Context if applied to Context, etc. 
    */
   type ThisType >: this.type <: Obj
   protected def mdNode = metadata.toNode
   /** prints to OpenMath */
   def toNode : scala.xml.Node
   /** prints to OpenMath (with OMOBJ wrapper) */
   def toOBJNode = {
      val om = xml.namespace("om") // inlining this into XML literal does not work
      <om:OMOBJ xmlns:om={om}>{toNode}</om:OMOBJ>
   }
   def toCMLQVars(implicit qvars: Context): Node
   def toCML = toCMLQVars(Context())
   /**
    * generic version of substitution that does one step and recurses according to a SubstitutionApplier
    *  
    * capture is avoided by renaming bound variables that are free in sub
    * 
    * Individual SubstitutionAppliers can default to this method in order to recurse one level.
    */
   def substitute(sub : Substitution)(implicit sa: SubstitutionApplier) : ThisType
   /**
    * convenience method that applies substitution by relegating to a SubstitutionApplier
    * 
    * This has the effect that o ^^ sub becomes an infix notation for substitution
    * if an implicit SubstitutionApplier is available.
    */
   def ^^(sub : Substitution)(implicit sa: SubstitutionApplier) : ThisType = sa(this, sub)
   /** applies a substitution to an object (computed immediately)
    *  implemented in terms of ^^ and PlainSubstitutionApplier
    */
   def ^(sub : Substitution) : ThisType = ^^(sub)(PlainSubstitutionApplier)
   /** optimized version of substitution defined in terms of SmartSubstitutionApplier */
   def ^?(sub : Substitution) : ThisType = ^^(sub)(SmartSubstitutionApplier)

   /** the free variables of this object in any order */ 
   lazy val freeVars : List[LocalName] = freeVars_.distinct
   /** helper function for freeVars that computes the free variables without eliminating repetitions */ 
   private[objects] def freeVars_ : List[LocalName]
   /** all direct subobjects of this object with their context (excluding any outer context of this object) */
   def subobjects: List[(Context,Obj)]
   /** auxiliary function for subobjects in the absence of binding */
   protected def subobjectsNoContext(os: List[Obj]) = os.map(s => (Context(),s))
   /** returns the subobject at a given position and its context; first index of pos used to be always 0 but not anymore */
   def subobject(pos: Position) : (Context, Obj) =
     pos.indices.foldLeft((Context(),this)) {
         case ((con,obj), i) =>
            obj.subobjects.applyOrElse(i, null) match {
               case null => throw GetError("position " + pos + " not valid in " + this)
               case (newCon, so) => (con ++ newCon, so)
            }
      }
   /* the constructor or constant used to form this term */
   def head : Option[ContentPath]
   /** the governing path required by Content is the head, if any */
   def governingPath = head
   /** replaces metadata of this with pointer to those of o
    * 
    * @param o the original object
    * call o2.copyFrom(o1) after transforming o1 into o2 in order to preserve metadata 
    */
   def copyFrom(o: Obj) {
      metadata = o.metadata
   }
}

/**
 * A Term represents an MMT term.
 */
sealed abstract class Term extends Obj {
   final type ThisType = Term
   def strip : Term = this
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
 * An OMID represents an MMT reference
 */
case class OMID(path: ContentPath) extends Term {
   def head = Some(path)
   def substitute(sub : Substitution)(implicit sa: SubstitutionApplier) = this
   private[objects] def freeVars_ = Nil
   def subobjects = Nil
   override def toString = path match {
      case doc ? mod => doc + "?" + mod.toString
      case OMMOD(mod) % name => mod.name.toString + "?" + name.toString
      case thy % name => "[" + thy.toString + "]?" + name.toString
   }
   def toNode = path match {
      case doc ? mod => <om:OMS base={doc.toPath} module={mod.toPath}>{mdNode}</om:OMS>
      case OMMOD(doc ? mod) % name => <om:OMS base={doc.toPath} module={mod.toPath} name={name.toPath}>{mdNode}</om:OMS>
      case thy % name => <om:OMS name={name.toPath}>{mdNode}{thy.toNode}</om:OMS>
   }
   def toCMLQVars(implicit qvars: Context) = <m:csymbol>{path.toPath}</m:csymbol>
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
   def toNode = 
      <om:OMBIND>{mdNode}
                 {binder.toNode}
                 {context.toNode}
                 {scopes.zipWithIndex.map {
                    case (s,i) => s.toNode
                 }}
      </om:OMBIND>
   override def toString = "(" + binder + " [" + context + "] " + scopes.map(_.toString).mkString(" ") + ")"  
   def substitute(sub : Substitution)(implicit sa: SubstitutionApplier) = {
      val (newCon, alpha) = Context.makeFresh(context, sub.freeVars)
      val subN = sub ++ alpha
      OMBINDC(binder ^^ sub, newCon ^^ sub, scopes.map(_ ^^ subN)).from(this)
   }
   private[objects] lazy val freeVars_ = binder.freeVars_ ::: context.freeVars_ ::: scopes.flatMap(_.freeVars_).filterNot(x => context.isDeclared(x))      
   def subobjects = ComplexTerm.subobjects(this) getOrElse {
     (Context(), binder) :: context.subobjects ::: scopes.map(s => (context, s))
   }
   def toCMLQVars(implicit qvars: Context) = <m:apply>{binder.toCMLQVars}{context.map(_.toCMLQVars)}{scopes.map(_.toCMLQVars)}</m:apply>
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
 * An OMA represents an application of a term to a list of terms
 * @param fun the function term
 * @param args the list of argument terms (may be Nil)
 */
case class OMA(fun : Term, args : List[Term]) extends Term {
   def head = fun.head
   override def toString = (fun :: args).map(_.toString).mkString("(", " ", ")")
   def toNode =
      <om:OMA>{mdNode}
              {fun.toNode}
              {args.zipWithIndex.map({case (a,i) => a.toNode})}
      </om:OMA>
   def substitute(sub : Substitution)(implicit sa: SubstitutionApplier) = OMA(fun ^^ sub, args.map(_ ^^ sub)).from(this)
   private[objects] lazy val freeVars_ = fun.freeVars_ ::: args.flatMap(_.freeVars_)
   def subobjects = ComplexTerm.subobjects(this) getOrElse subobjectsNoContext(fun :: args)
   def toCMLQVars(implicit qvars: Context) = <m:apply>{fun.toCMLQVars}{args.map(_.toCMLQVars)}</m:apply>
}

/**
 * special application that elides empty argument lists:
 * @return f if args is Nil, OMA(f, args) otherwise
 */
object OMAorOMID {
   def apply(f: Term, args: List[Term]) = if (args == Nil) f else OMA(f,args)
   def unapply(t: Term) = t match {
      case OMID(p) => Some((t, Nil))
      case OMA(t, args) => Some((t, args))
      case _ => None
   }
}

/**
 * An OMV represents a reference to a variable.
 * @param name the name of the referenced variable
 */
case class OMV(name : LocalName) extends Term {
   def head = None
   /** the substutition this/s */
   def /(s : Term) = Sub(name, s)
   def ->(s: Term) = Sub(name, s)
   /** the declaration this:tp */
   def %(tp : Term) = VarDecl(name, Some(tp), None, None)
   def toNode = <om:OMV name={name.toPath}>{mdNode}</om:OMV>
   override def toString = name.toString
   def substitute(sub : Substitution)(implicit sa: SubstitutionApplier) = 
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
   def subobjects = Nil
   def toCMLQVars(implicit qvars: Context) =
      if (qvars.isDeclared(name)) <mws:qvar xmlns:mws="http://www.mathweb.org/mws/ns">{name.toPath}</mws:qvar>
      else <m:ci>{name.toPath}</m:ci>
}

/** helper object */
object OMV {
   /** convenience */
   def apply(n: String) : OMV = OMV(LocalName(n))
   /** the name of an anonymous variable (_) */
   val anonymous = LocalName("_")
}

/**
 * An OMATTR represents an attributed term.
 * @param arg the term without attribution
 * @param key the key (This must be an OMS in OpenMath.)
 * @param value the value
 */
case class OMATTR(arg : Term, key : OMID, value : Term) extends Term {
   def head = key.head
   override def strip = arg.strip
   override def toString = "{" + arg + " : " + key + " -> " + value + "}"
   def toNode = 
      <om:OMATTR>{mdNode}<om:OMATP>{key.toNode}{value.toNode}</om:OMATP>
                 {arg.toNode}
      </om:OMATTR>
   def substitute(sub : Substitution)(implicit sa: SubstitutionApplier) = OMATTR(arg ^^ sub, key, value ^^ sub).from(this)
   def subobjects = List(arg, key, value).map(s => (Context(), s))
   private[objects] def freeVars_ = arg.freeVars_ ::: value.freeVars_
   def toCMLQVars(implicit qvars: Context) = <m:apply><m:csymbol>OMATTR</m:csymbol>{arg.toCMLQVars}{key.toCMLQVars}{value.toCMLQVars}</m:apply>
}

/** apply/unapply methods for a list of attributions */
object OMATTRMany {
   def apply(arg: Term, attrs: List[(OMID,Term)]) = attrs.foldLeft(arg){
      case (sofar, (k,v)) => OMATTR(sofar, k, v)
   }
   /** always matches */
   def unapply(t: Term) : Option[(Term, List[(OMID, Term)])] = t match {
      case OMATTR(a, k, v) =>
         val Some((arg, kvs)) = unapply(a)
         Some((arg, kvs:::List((k,v))))
      case t => Some(t, Nil)
   }
}

/** The joint methods of OMLIT and UnknownOMLIT */
sealed trait OMLITTrait extends Term {
   def synType: GlobalName
   def head = None
   def toNode = <om:OMLIT value={toString} type={synType.toPath}/>
   def toCMLQVars(implicit qvars: Context) = <m:lit value={toString} type={synType.toPath}/>
   def substitute(sub : Substitution)(implicit sa: SubstitutionApplier) = this
   private[objects] def freeVars_ = Nil
   def subobjects = Nil
}

/**
 * A literal consists of a RealizedType rt and a value:rt.univ.
 * 
 * OMLIT is abstract due to subtleties of the Scala type system. equals is overridden accordingly. 
 * 
 * Literals can be constructed conveniently using OMLIT.apply or RealizedType.apply 
 *
 * invariant for structurally well-formed literals: the value is valid and normal, i.e,
 *   rt.valid(value) and rt.normalform(value) == value
 */
sealed abstract class OMLIT(val rt: RealizedType) extends Term with OMLITTrait {
   override def equals(that: Any) = that match {
      case l: OMLIT => rt == l.rt && value == l.value
      case _ => false
   }
   val value: rt.univ
   def synType = rt.synType
   override def toString = rt.toString(value)
}

object OMLIT {
   /**
    * the canonical way to construct literals
    * the value is always normalized and checked for validity
    */
   def apply(rt: RealizedType)(v: rt.univ) = {
      new OMLIT(rt) {
         val value = {
            // This always type checks, but the Scala compiler cannot prove it and needs a type cast
            val vN = rt.normalform(v.asInstanceOf[rt.univ])
            if (! rt.valid(vN))
               throw ParseError("invalid literal value")
            vN
         }
      }
   }
}

/** degenerate case of OMLIT when no RealizedType was known to parse a literal
 *  
 *  This class is awkward but necessary to permit a lookup-free parser, which delays parsing of literals to a later phase.
 *  UnknownOMLITs are replaced with OMLITs in the [[libraries.StructureChecker]].
 */
case class UnknownOMLIT(value: String, synType: GlobalName) extends Term with OMLITTrait {
   override def toString = value 
}

/**
 * An OMFOREIGN represents an OpenMath foreign object.
 * @param node the XML element holding the foreign object
 */
case class OMFOREIGN(node : Node) extends Term {
   def head = None
   def toNode = <om:OMFOREIGN>{node}</om:OMFOREIGN>
   def substitute(sub : Substitution)(implicit sa: SubstitutionApplier) = this
   private[objects] def freeVars_ = Nil
   def subobjects = Nil
   def toCMLQVars(implicit qvars: Context) = <m:apply><m:csymbol>OMFOREIGN</m:csymbol>{Node}</m:apply>
}


//TODO: could this be merged with presentation.Literal?
/** An OMSemiFormal represents a mathematical object that mixes formal and informal components */
case class OMSemiFormal(tokens: List[SemiFormalObject]) extends Term with SemiFormalObjectList {
   def head = None
   def substitute(sub : Substitution)(implicit sa: SubstitutionApplier) = {
      val newtokens = tokens map {
         case Formal(t) => Formal(t ^^ sub)
         case i => i
      }
      OMSemiFormal(newtokens).from(this)
   }
   private[objects] def freeVars_ = tokens.flatMap(_.freeVars)
   def subobjects = {
      val terms = tokens.flatMap {
         case Formal(t) => List(t)
         case _ => Nil
      }
      subobjectsNoContext(terms)
   }
   def toCMLQVars(implicit qvars: Context) = <m:apply><m:csymbol>OMSemiFormal</m:csymbol>{tokens.map(_.toCMLQVars)}</m:apply>
}

object OMSemiFormal {
  def apply(tokens: SemiFormalObject*) : OMSemiFormal = OMSemiFormal(tokens.toList)
}

/**
 * ComplexTerm provides apply/unapply methods to unify OMA and OMBINDC as well as named arguments and complex binders
 * 
 * It does not subsume the OMID case
 */
object ComplexTerm {
   object LabelledTerms {
      def apply(sub: Substitution) = sub.map {case Sub(l,t) => (OMS(utils.mmt.label ? l), t)}
      def unapply(ts: List[(OMID,Term)]): Option[Substitution] = ts match {
         case Nil => Some(Nil)
         case (OMS(utils.mmt.label ?? l), t) :: rest =>
            unapply(rest) map {case sub => Sub(l,t) ++ sub}  
      }
   }
   def apply(p: GlobalName, sub: Substitution, con: Context, args: List[Term]) = {
      val psub = OMATTRMany(OMS(p), LabelledTerms(sub))
      if (con.isEmpty && args.isEmpty) psub
      else if (con.isEmpty) OMA(psub, args)
      else OMBINDC(psub, con, args)
   }
   def unapply(t: Term) : Option[(GlobalName, Substitution, Context, List[Term])] = t match {
      //case OMS(p) => Some((p, Nil, Context(), Nil))
      case OMATTRMany(OMS(p), LabelledTerms(sub)) if ! sub.isEmpty => Some((p, sub, Context(), Nil))
      case OMA(OMATTRMany(OMS(p), LabelledTerms(sub)), args) => Some((p, sub, Context(), args))
      case OMBINDC(OMATTRMany(OMS(p), LabelledTerms(sub)), con, scopes) => Some((p, sub, con, scopes))
      case _ => None
   }
   def subobjects(t: Term) : Option[List[(Context, Obj)]] = unapply(t) map {case (op, subs, con, args) =>
      (Context(),OMS(op)) :: subs.subobjects ::: con.subobjects ::: args.map(a => (con,a))
   }
}


/**
 * Obj contains the parsing methods for objects.
 */
object Obj {
   /** parses a term relative to a base address
    *  @param Nmd node to parse (may contain metadata) 
    *  @param base base URI to resolve relative URIs
    *  @return the parsed term
    */
   def parseTerm(Nmd : Node, nm: NamespaceMap) : Term = {
      implicit val nsMap = nm
      Nmd match {
         case <OMOBJ>{o}</OMOBJ> => parseTermRec(o)
         case o => parseTermRec(o)
      }
   }

   /**
    * the recursion for parseTermTop
    */
   private def parseTermRec(Nmd : Node)(implicit nsMap: NamespaceMap) : Term = {
      //this function unifies the two cases for binders in the case distinction below
      def doBinder(binder : Node, context : Node, scopes : List[Node]) = {
         val bind = parseTermRec(binder)
         val cont = Context.parse(context, nsMap)
         if (cont.isEmpty)
            throw new ParseError("at least one variable required in " + cont.toString)
         val scopesP = scopes.map(parseTermRec(_))
         OMBINDC(bind, cont, scopesP)
      }
      val (n,mdOpt) = metadata.MetaData.parseMetaDataChild(Nmd, nsMap)
      val N = n
      val o = N match {
         case <OMS/> =>
            parseOMS(N) match {
               case p : ContentPath => OMID(p)
               case p => throw new ParseError("Not a term: " + p + " " + N.toString)
            }
         case <OMV/> =>
            OMV(LocalName.parse(xml.attr(N,"name")))
         case <OMA>{child @ _*}</OMA> =>
            if (child.length == 0)
               throw ParseError("No operator given: " + N.toString)
            val fun = parseTermRec(child.head)
            val args = child.tail.toList.map(parseTermRec(_))
            OMA(fun, args)
         case <OME>{child @ _*}</OME> =>
            throw ParseError("OME not supported - use OMA: " + N.toString)
         case <OMBIND>{binder}{context}{scopes @ _*}</OMBIND> =>
            doBinder(binder, context, scopes.toList)
         case <OMATTR><OMATP>{key}{value}</OMATP>{rest @ _*}</OMATTR> =>
            val k = parseTermRec(key)
            if (! k.isInstanceOf[OMID])
               throw new ParseError("key must be OMS in " + N.toString)
            val v = parseTermRec(value)
            if (rest.length == 0)
               throw ParseError("not a well-formed attribution: " + N.toString)
            val n = if (rest.length == 1)
               rest(0) else <OMATTR>{rest}</OMATTR>
            val t = parseTermRec(n)
            OMATTR(t, k.asInstanceOf[OMID], v)
         case <OMFOREIGN>{_*}</OMFOREIGN> => OMFOREIGN(N)
         case <OMSF>{nodes @ _*}</OMSF> =>
            val sf = nodes.toList.map {
               case node @ <text>{scala.xml.PCData(t)}</text> => Text(xml.attr(node, "format"), t)
               case <node>{n}</node> => XMLNode(n)
               case n => Formal(parseTermRec(n))
            }
            OMSemiFormal(sf)
         case <OMI>{i}</OMI> => OMI.parse(i.toString)
         case <OMSTR>{s @ _*}</OMSTR> => OMSTR.parse(s.toString)
         case <OMF/> => OMF.parse(xml.attr(N, "dec"))
         case <OMLIT/> =>
            val tp = Path.parseS(xml.attr(N, "type"), nsMap)
            val v = xml.attr(N, "value")
            UnknownOMLIT(v, tp)
         case _ => throw ParseError("not a well-formed term: " + N.toString)
      }
      mdOpt.foreach {md => o.metadata = md}
      o
   }
   private def parseOMS(N : Node)(implicit nsmap: NamespaceMap) : Path = N match {
      case <OMS/> =>
        val doc = xml.attr(N,"base")
        val mod = xml.attr(N,"module")
        val name = xml.attr(N,"name")
        Path.parse(doc, mod, name, "", nsmap)
      case <OMS>{n}</OMS> =>
        val mod = parseTermRec(n)
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
           case MPathEncodedOMA(p: ContentPath, args) => OMA(OMID(p), args map fromPathEncodingAux)
           case MPathEncodedOMS(p: ContentPath) => OMID(p)
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
            // s is of the form (op___(op___arg___arg)___arg) where ___ == sepString
            var left = s.substring(1,s.length-1).split(sepString).toList
            var found : List[String] = Nil
            var level = 0
            var current: List[String] = Nil
            while (! left.isEmpty) {
               val head = left.head
               left = left.tail
               current ::= head
               if (head.startsWith("(")) level += 1
               if (head.endsWith(")")) level -= 1
               if (level == 0) {
                  found ::= current.reverse.mkString(sepString)
                  current = Nil
               }
            }
            found = found.reverse
            // now found is of the form List(op, (op___arg___arg), arg) 
            val op = Path.parse(found.head)
            Some((op, found.tail))
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
