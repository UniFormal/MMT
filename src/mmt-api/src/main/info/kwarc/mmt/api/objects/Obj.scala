package info.kwarc.mmt.api.objects

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.notations.TextNotation
import info.kwarc.mmt.api.objects.Conversions._
import info.kwarc.mmt.api.presentation.{ContentMathMLPresenter, MathMLContext}
import info.kwarc.mmt.api.presentation.HTMLAttributes.position
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.utils.xml.addAttrOrChild

import scala.xml.Node

trait ShortURIPrinter {
   /** configurable string representation
     *  @param shortURIs print OMS without namespace, theory
     */
   def toStr(implicit shortURIs: Boolean): String
   /** defaults to toStr(false) */
   override def toString = toStr(false)
}


/**
 * An Obj represents an MMT object. MMT objects are represented by immutable Scala objects.
 */
abstract class Obj extends Content with ontology.BaseType with ShortURIPrinter with HashEquality[Obj] {
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
   @deprecated("use ContentMathMLPresenter instead")
   def toCMLQVars(implicit qvars: Context): Node = ContentMathMLPresenter.applyContext(this)(MathMLContext.forContent(qvars, None))
   @deprecated("use ContentMathMLPresenter instead")
   def toCML = ContentMathMLPresenter(this)

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
   /** the paths mentioned in this object in any order */
   def paths = paths_.distinct
   /** helper function for paths that does not eliminate repetitions */
   private[objects] def paths_ : List[Path]
   /** all direct subobjects of this object with their context (excluding any outer context of this object) */
   def subobjects: List[(Context,Obj)]
   /** auxiliary function for subobjects in the absence of binding */
   protected def subobjectsNoContext(os: List[Obj]) = os.map(s => (Context(),s))
   /** returns the subobject at a given position and its context */
   def subobject(pos: Position) : (Context, Obj) =
     pos.indices.foldLeft((Context(),this)) {
         case ((con,obj), i) =>
            obj.subobjects.lift(i) match {
               case None => throw SubobjectError(this, pos)
               case Some((newCon, so)) => (con ++ newCon, so)
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
   def copyFrom(o: Obj): Unit = {
      metadata = o.metadata
   }
   /** applies copyFrom and returns this
     *
     * @return this object but with the metadata from o
     */
   def from(o: Obj): this.type = {
      copyFrom(o)
      this
   }
}

trait ThisTypeTrait

/**
 * A Term represents an MMT term.
 */
sealed abstract class Term extends Obj with ThisTypeTrait {
   final type ThisType = Term
   def strip : Term = this
   /** permits the intuitive f(t_1,...,t_n) syntax for term applications */
   def apply(args : Term*) = OMA(this, args.toList)
   /** the syntactically computed MPath of the term.
     * If the term is module level, this is guaranteed to be correct. */
   def toMPath : MPath = this match {
     case OMMOD(p) => p
     case OMPMOD(p,_) =>
        p // TODO maybe?
     case OMS(p) => p.module / p.name
     case _ => mmt.mmtbase ? Obj.toPathEncoding(this)
   }
}

/**
 * An OMID represents an MMT reference
 */
case class OMID(path: ContentPath) extends Term {
   def head = Some(path)
   def substitute(sub : Substitution)(implicit sa: SubstitutionApplier) = this
   private[objects] def freeVars_ = Nil
   private[objects] def paths_ = List(path)
   def subobjects = Nil
   override def toStr(implicit shortURIs: Boolean) = if (shortURIs) path.name.toStr else path.toString
   def toNode = path match {
      case doc ? mod => <om:OMS base={doc.toPath} module={mod.toPath}>{mdNode}</om:OMS>
      case doc ? mod ?? name => <om:OMS base={doc.toPath} module={mod.toPath} name={name.toPath}>{mdNode}</om:OMS>
      //case thy % name => <om:OMS name={name.toPath}>{mdNode}{thy.toNode}</om:OMS>
   }
}

object OMS {
   def apply(p: GlobalName) = OMID(p)
   def unapply(t: Term) : Option[GlobalName] = t match {
     case OMID(p : GlobalName) => Some(p)
     case _ => None
   }
}

/**
 * An OMBINDC represents a binding with arbitrarily many scopes
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
   override def toStr(implicit shortURIs: Boolean) = "(" + binder.toStr + " [" + context.toStr + "] " + scopes.map(_.toStr).mkString(" ") + ")"
   def substitute(sub : Substitution)(implicit sa: SubstitutionApplier) = {
      val (newCon, alpha) = Context.makeFresh(context, sub.freeVars)
      val subN = sub ++ alpha
      OMBINDC(binder ^^ sub, newCon ^^ sub, scopes.map(_ ^^ subN)).from(this)
   }
   private[objects] lazy val freeVars_ = binder.freeVars_ ::: context.freeVars_ ::: scopes.flatMap(_.freeVars_).filterNot(x => context.isDeclared(x))
   private[objects] def paths_ = binder.paths_ ::: context.paths_ ::: scopes.flatMap(_.paths_)
   def subobjects = ComplexTerm.subobjects(this) getOrElse {
     (Context(), binder) :: context.subobjects ::: scopes.map(s => (context, s))
   }
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
   override def toStr(implicit shortURIs: Boolean) = (fun :: args).map(_.toStr).mkString("(", " ", ")")
   def toNode =
      <om:OMA>{mdNode}
              {fun.toNode}
              {args.zipWithIndex.map({case (a,i) => a.toNode})}
      </om:OMA>
   def substitute(sub : Substitution)(implicit sa: SubstitutionApplier) = OMA(fun ^^ sub, args.map(_ ^^ sub)).from(this)
   private[objects] lazy val freeVars_ = fun.freeVars_ ::: args.flatMap(_.freeVars_)
   private[objects] def paths_ = fun.paths_ ::: args.flatMap(_.paths_)
   def subobjects = ComplexTerm.subobjects(this) getOrElse subobjectsNoContext(fun :: args)
}

/**
 * special application that elides empty argument lists:
 * @return f if args is Nil, OMA(f, args) otherwise; always matches
 */
object OMAorAny {
   def apply(f: Term, args: List[Term]) = if (args == Nil) f else OMA(f,args)
   def unapply(t: Term) = t match {
      case OMA(t, args) => Some((t, args))
      case t => Some((t, Nil))
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
   def %(tp : Term) = VarDecl(name,tp)
   def toNode = <om:OMV name={name.toPath}>{mdNode}</om:OMV>
   override def toStr(implicit shortURIs: Boolean) = name.toString
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
   private[objects] def paths_ = Nil
   def subobjects = Nil
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
   override def toStr(implicit shortURIs: Boolean) = "{" + arg.toStr + " : " + key.toStr + " -> " + value.toStr + "}"
   def toNode =
      <om:OMATTR>{mdNode}<om:OMATP>{key.toNode}{value.toNode}</om:OMATP>
                 {arg.toNode}
      </om:OMATTR>
   def substitute(sub : Substitution)(implicit sa: SubstitutionApplier) = OMATTR(arg ^^ sub, key, value ^^ sub).from(this)
   def subobjects = List(arg, key, value).map(s => (Context(), s))
   private[objects] def freeVars_ = arg.freeVars_ ::: value.freeVars
   private[objects] def paths_ = arg.paths_ ::: key.paths_ ::: value.paths_
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

/** The joint methods of [[OMLIT]] and [[UnknownOMLIT]] */
sealed trait OMLITTrait extends Term {
   def synType: Term
   /** canonical string representation of this literal */
   def valueString: String
   def head = None // synType.head is awkward because it false triggers rules
   def synTypeXML = Obj.toStringOrNode(synType)
   def toNode = addAttrOrChild(<om:OMLIT value={toString}/>, "type", synTypeXML)
   def substitute(sub : Substitution)(implicit sa: SubstitutionApplier) = this
   private[objects] def freeVars_ = Nil // free variables in a type do not make sense for literal values
   private[objects] def paths_ = synType.paths_
   def subobjects = Nil

   /** checks equality, including the case [[OMLIT]] =?= [[UnknownOMLIT]] */
   override def equals(that: Any): Boolean = (this, that) match {
      case (l: OMLIT, m: OMLIT) => l.rt == m.rt && l.value == m.value
      case (l: UnknownOMLIT, m: UnknownOMLIT) => l.synType == m.synType && l.valueString == m.valueString
      case (l: OMLIT, m: UnknownOMLIT) =>
         // second conjunct is sufficient, first conjunct only acts as guard for checking the second
         (l.synType == m.synType) && l == l.rt.parse(m.valueString)
      case (l: UnknownOMLIT, m: OMLIT) => m == l
      case _ => false
   }
   /** hash code depends only on synType and valueString */
   override def hashCode = (synType,valueString).hashCode
}

/**
 * A literal consists of a RealizedType rt and a value of [[SemanticType]] rt.semType.
 *
 * Literals can be constructed conveniently using RealizedType.apply
 *
 * invariant for structurally well-formed literals: the value is valid and normal, i.e,
 *   rt.semType.valid(value) and rt.semType.normalform(value) == value
 */
case class OMLIT(value: Any, rt: uom.RealizedType) extends Term with OMLITTrait {
   def synType: Term = rt.synType
   override def toStr(implicit shortURIs: Boolean) = valueString
   def valueString: String = rt.semType.toString(value)
}

/** degenerate case of OMLIT when no RealizedType was known to parse a literal
 *
 *  This class is awkward but necessary to permit a lookup-free parser, which delays parsing of literals to a later phase.
 *  UnknownOMLITs are replaced with OMLITs in the [[checking.MMTStructureChecker]].
 *
 *  @param synType the type of the this literal
 */
case class UnknownOMLIT(valueString: String, synType: Term) extends Term with OMLITTrait {
   override def toStr(implicit shortURIs: Boolean) = valueString
}

/**
 * An OMFOREIGN represents an OpenMath foreign object.
 * @param node the XML element holding the foreign object
 */
case class OMFOREIGN(node : Node) extends Term {
   def head = None
   def toStr(implicit shortURIs: Boolean) = "OMFOREIGN(" + node.toString() + ")"
   def toNode = <om:OMFOREIGN>{node}</om:OMFOREIGN>
   def substitute(sub : Substitution)(implicit sa: SubstitutionApplier) = this
   private[objects] def freeVars_ = Nil
   private[objects] def paths_ = Nil
   def subobjects = Nil
}


/** An OMSemiFormal represents a mathematical object that mixes formal and informal components */
@deprecated("MMT_TODO: this should be replaced with the urtheory for semiformal objects", since="forever")
case class OMSemiFormal(tokens: List[SemiFormalObject]) extends Term with SemiFormalObjectList {
   def head = None
   def toStr(implicit shortURIs: Boolean) = toString
   def substitute(sub : Substitution)(implicit sa: SubstitutionApplier) = {
      val newtokens = tokens map {
         case Formal(t) => Formal(t ^^ sub)
         case i => i
      }
      OMSemiFormal(newtokens).from(this)
   }
   private[objects] def freeVars_ = tokens.flatMap(_.freeVars)
   private[objects] def paths_ = tokens.flatMap(_.paths)
   def subobjects = {
      val terms = tokens.flatMap {
         case Formal(t) => List(t)
         case _ => Nil
      }
      subobjectsNoContext(terms)
   }
}

object OMSemiFormal {
  def apply(tokens: SemiFormalObject*) : OMSemiFormal = OMSemiFormal(tokens.toList)
}

/**
 * local declarations with type and/or definiens, with scope, but not subject to alpha-renaming and substitution
 *
 * These could be used for the typed/defined fields in a record type/value or the selection function.
 */
case class OML(name: LocalName, tp: Option[Term], df: Option[Term], nt: Option[TextNotation] = None, featureOpt : Option[String] = None) extends Term with NamedElement {
    def toStr(implicit shortURIs: Boolean) = if (tp.isEmpty && df.isEmpty && nt.isEmpty && featureOpt.isEmpty) name.toString else "(" + vd.toStr + ")"

   /**
     * Get a [[VarDecl]] representation of this OML, e.g. for insertion into a [[Context]].
     */
    def vd = VarDecl(name, featureOpt, tp, df, nt).from(this)
    private[objects] def freeVars_ = vd.freeVars
    private[objects] def paths_ = vd.paths
    def head = None
    def subobjects = subobjectsNoContext(vd.tp.toList ::: vd.df.toList)
    def substitute(sub: Substitution)(implicit sa: SubstitutionApplier) = OML(name, tp map (_ ^^ sub), df map (_ ^^ sub),nt,featureOpt)
    def toNode = vd.toNode.copy(label = "OML")
}

object OML {
  def apply(name: LocalName): OML = OML(name, None, None)
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
         case _ => None
      }
   }
   def apply(p: GlobalName, sub: Substitution, con: Context, args: List[Term]): Term = {
      val psub = OMATTRMany(OMS(p), LabelledTerms(sub))
      if (con.isEmpty && args.isEmpty) psub
      else if (con.isEmpty) OMA(psub, args)
      else OMBINDC(psub, con, args)
   }
   /** like apply, but takes sub, con, and args in a single list
    *  pre: subobjects of the form x:::y:::z with x:List[Sub], y: List[VarDecl], z: List[Term]
    */
   def apply(p: GlobalName, subobjects: List[Obj]): Term = {
     val (subs, conArgs) = subobjects.span(_.isInstanceOf[Sub])
     val (con,args) = conArgs.span(_.isInstanceOf[VarDecl])
     val subsI = subs.map(_.asInstanceOf[Sub])
     val conI = con.map(_.asInstanceOf[VarDecl])
     val argsI = args.map {
       case t: Term => t
       case o => throw ImplementationError("term expected, found: " + o)
     }
     ComplexTerm(p, subsI, conI, argsI)
   }
   def unapply(t: Term) : Option[(GlobalName, Substitution, Context, List[Term])] = t match {
      //case OMS(p) => Some((p, Nil, Context(), Nil))
      case OMATTRMany(OMS(p), LabelledTerms(sub)) if ! sub.isEmpty => Some((p, sub, Context(), Nil))
      case OMA(OMATTRMany(OMS(p), LabelledTerms(sub)), args) => Some((p, sub, Context(), args))
      case OMBINDC(OMATTRMany(OMS(p), LabelledTerms(sub)), con, scopes) => Some((p, sub, con, scopes))
      case OMBINDC(OMA(OMS(p), args), con, scopes) =>
        val sub = Substitution(args.map(a => Sub(LocalName.empty, a)):_*)
        Some((p, sub, con, scopes))
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
   def getConstants(t: Obj) = getCs(t).distinct
   private def getCs(t: Obj): List[GlobalName] = t match {
      case ComplexTerm(p, subs, con, args) => p :: getCs(subs) ::: getCs(con) ::: (args flatMap getCs)
      case OMS(p) => List(p)
      case c: Context => c flatMap getCs
      case vd: VarDecl => (vd.tp.toList:::vd.df.toList).flatMap(getCs)
      case s: Substitution => s flatMap getCs
      case s: Sub => getCs(s.target)
      case _ => Nil
   }

   /** use this in conjunction with utils.addAttrOrNode to generate XML with attributes instead of children where possible */
   def toStringOrNode(t: Term): Union[String,Node] = t match {
      case OMID(p) => Left(p.toPath)
      case _ => Right(t.toNode)
   }
   /** dual of toStringOrNode */
   def parseStringOrNode(v: Union[String,Node], nm: NamespaceMap): Term = v match {
      case Left(s) => Path.parse(s, nm) match {
         case cp: ContentPath => OMID(cp)
         case p => throw ParseError("Not a term: " + p)
      }
      case Right(c) => parseTerm(c, nm)
   }

   /** parses an object (term or context) relative to a base address (a single OMV is always parsed as a Term, never as a VarDecl) */
   def parse(n: Node, nsMap: NamespaceMap): Obj = {
     n.label match {
       case "OMOBJ" => parse(n.child.head, nsMap)
       case "OMBVAR" => Context.parse(n, nsMap)
       case _ => parseTerm(n, nsMap)
     }
   }

   /** parses a term relative to a base address
    *  @param N node to parse (may not contain metadata)
    *  @param nm namespace Map to resolve relative URIs
    *  @return the parsed term
    */
   def parseTerm(N : Node, nm: NamespaceMap): Term = {
      implicit val nsMap = nm
      xml.trimOneLevel(N) match {
         case <OMOBJ>{o}</OMOBJ> => parseTermRec(o)
         case o => parseTermRec(o)
      }
   }

   /**
    * the recursion for parseTermTop
    */
   private def parseTermRec(nd : Node)(implicit nsMap: NamespaceMap) : Term = {
      val Nmd = xml.trimOneLevel(nd)
      //this function unifies the two cases for binders in the case distinction below
      def doBinder(binder : Node, context : Node, scopes : List[Node]) = {
         val bind = parseTermRec(binder)
         val cont = Context.parse(context, nsMap)
         //if (cont.isEmpty)
           // throw ParseError("at least one variable required in " + Nmd.toString)
         val scopesP = scopes.map(parseTermRec(_))
         OMBINDC(bind, cont, scopesP)
      }
      val (n,mdOpt) = metadata.MetaData.parseMetaDataChild(Nmd, nsMap)
      val N = n
      val o = N match {
         case <OMS/> =>
            parseOMS(N) match {
               case p : ContentPath => OMID(p)
               case p => throw new ParseError("Not a t>erm: " + p + " " + N.toString)
            }
         case <OMV/> =>
            OMV(LocalName.parse(xml.attr(N,"name")))
         case <OMA>{child @ _*}</OMA> =>
            if (child.length == 0)
               throw ParseError("No operator given: " + N.toString)
            val fun = parseTermRec(child.head)
            val args = child.tail.toList.map(parseTermRec(_))
            OMA(fun, args)
         case <OMBIND>{binder}{context}{scopes @ _*}</OMBIND> =>
            doBinder(binder, context, scopes.toList)
         case <OMLIT/> | <OMLIT>{_*}</OMLIT> =>
            val (_, tpN) = xml.getAttrOrChild(N, "type")
            val tpN2 : Union[String, Node] = tpN match {
               case Right(<type>{c}</type>) => Right(c)
               case _ => tpN
            }
            val tp = parseStringOrNode(tpN2, nsMap)
            val v = xml.attr(N, "value")
            UnknownOMLIT(v, tp)
         case <OMI>{i}</OMI> => uom.OMLiteral.OMI.parse(i.toString)
         case <OMSTR>{s @ _*}</OMSTR> => uom.OMLiteral.OMSTR.parse(s.toString)
         case <OMF/> => uom.OMLiteral.OMF.parse(xml.attr(N, "dec"))
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
         case <OME>{child @ _*}</OME> =>
            throw ParseError("OME not supported - use OMA: " + N.toString)
         case <OMFOREIGN>{n}</OMFOREIGN> => OMFOREIGN(n)
         case <OMSF>{nodes @ _*}</OMSF> =>
            val sf = nodes.toList.map {
               case node @ <text>{scala.xml.PCData(t)}</text> => Text(xml.attr(node, "format"), t)
               case <node>{n}</node> => XMLNode(n)
               case n => Formal(parseTermRec(n))
            }
            OMSemiFormal(sf)
         case l:scala.xml.Elem if l.label == "OML" =>
            val vd = VarDecl.parse(l.copy(label = "OMV"), nsMap)
            vd.toOML
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
      /*case <OMS>{n}</OMS> =>
        val mod = parseTermRec(n)
        val name = xml.attr(N,"name")
        mod % name*/
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
     case ComplexTheory(Context.empty) => toPathEncodingAux(OMA(OMS(ModExp.complextheory), Nil))
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
            while (left.nonEmpty) {
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
