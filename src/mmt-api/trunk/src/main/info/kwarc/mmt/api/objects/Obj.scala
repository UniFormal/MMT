package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
import scala.xml.{Node}
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.presentation._
import Context._
import Substitution._

//import info.kwarc.mmt.api.utils.{log}

/**
 * An Obj represents an MMT object. MMT objects are represented by immutable Scala objects.
 */
sealed abstract class Obj extends Content {
   /** prints to OpenMath (with OMOBJ wrapper) */
   def toNodeID(pos : Position) : scala.xml.Node
   def toNode : scala.xml.Node = toNodeID(Position.None)
   def toOBJNode = <OMOBJ xmlns:om={xml.namespace("om")}>{toNode}</OMOBJ>
   /** applies a substitution to an object (computed immediately) */
   def ^ (sub : Substitution) : Obj
   def head : Option[Path]
   def role : Role
   def nkey = NotationKey(head, role)
   def components : List[Content]
   def presentation(lpar: LocalParams) = ByNotation(nkey, components, lpar)
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
}

/**
 * A Term represents an MMT term.
 */
sealed abstract class Term extends Obj {
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
   def ^ (sub : Substitution) = this
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
      val id = context.id
      val subid = sub ++ id
      // sub ++ id.take(i) represents sub, x_1/x_1, ..., x_{i-1}/x_{i-1} 
      val newcon = context.zipWithIndex map {case (vd, i) => vd ^ (sub ++ id.take(i))} 
      OMBINDC(binder ^ sub, newcon, condition.map(_ ^ subid), body ^ subid)
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

/**
 * An OMA represents an application of a term to a list of terms
 * @param fun the function term
 * @param args the list of argument terms
 */
case class OMA(fun : Term, args : List[Term]) extends Term {
   def head = fun.head
   def role = Role_application
   def components = fun :: args
   override def toString = (fun :: args).map(_.toString).mkString("@(", ", ", ")")
   def toNodeID(pos : Position) =
      <om:OMA>{fun.toNodeID(pos + 0)}
              {args.zipWithIndex.map({case (a,i) => a.toNodeID(pos+(i+1))})}
      </om:OMA> % pos.toIDAttr
   def ^ (sub : Substitution) = OMA(fun ^ sub, args.map(_ ^ sub))
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
   def /(s : Term) = Substitution(Sub(name, s))
   /** the declaration this:tp */
   def %(tp : Term) = Context(TermVarDecl(name, Some(tp), None))
   def toNodeID(pos : Position) = <om:OMV name={name}/> % pos.toIDAttr
   override def toString = name
   def ^(sub : Substitution) = try {sub(name)} catch {case SubstitutionUndefined(_) => this}
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
}
/*
 * OpenMath values are integers, floats, and strings (we omit byte arrays)
 * 
 */
case class OMI(value : BigInt) extends Term {
   def head = None
   def role = Role_value
   def components = List(StringLiteral(value.toString))
   def toNodeID(pos : Position) = <om:OMI>{value.toString}</om:OMI> % pos.toIDAttr
   def ^(sub : Substitution) = this
}
case class OMF(value : Double) extends Term {
   def head = None
   def role = Role_value
   def components = List(ValueLiteral(value))
   def toNodeID(pos : Position) = <om:OMF>{value.toString}</om:OMF> % pos.toIDAttr
   def ^(sub : Substitution) = this
}
case class OMSTR(value : String) extends Term {
   def head = None
   def role = Role_value
   def components = List(StringLiteral(value))
   def toNodeID(pos : Position) = <om:OMSTR>{value.toString}</om:OMSTR> % pos.toIDAttr
   def ^(sub : Substitution) = this
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
   def links = List(path) 
   override def asPath = Some(path)
   override def toString = path.toPath
}

/*
case object OMINIT extends TheoryObj with ComposedModuleObject {
   def role = Role_initial
    def components = Nil
}

case class OMPII(left: TheoryObj, right: TheoryObj) extends TheoryObj with ComposedModuleObject {
   def role = Role_pushout_ii
   def components = List(left, right)
   override def toString = left.toString + " + " + right.toString
}

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

case class OMEMPTY(from: TheoryObj, to: TheoryObj) extends Morph with MMTObject {
   def args = List(from,to)
   def path = mmt.emptymorphism
}

/*
object OMINITM {
   def apply(thy : TheoryObj) = OMEMPTY(OMINITIAL, thy)
   def unapply(t: Obj) : Option[TheoryObj] = t match {
      case OMEMPTY(OMINITIAL, thy) => Some(thy)
   }
}

case class OMUNION(morphs: left: Morph, right: Morph) extends Morph with ComposedModuleObject {
   def role = Role_pushout_iim
   def components = List(left, right)
   override def toString = left.toString + " + " + right.toString
}

case class OMPIM(push: Morph, along: Morph, wth: Morph) extends Morph with ComposedModuleObject {
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
   def parseTerm(N : Node, base : Path) : Term = parseTerm(N, base, x => ())
   /** parses a term relative to a base address and calls a function on every encountered identifier */
   def parseTerm(N : Node, base : Path, callback : Path => Unit) : Term = {
      //N can set local cdbase attribute; if not, it is copied from the parent
      val nbase = newBase(N, base)
      //this function unifies the two cases for binders in the case distinction below
      def doBinder(binder : Node, context : Seq[Node], condition : Option[Node], body : Node) = {
         val bind = parseTerm(binder, nbase, callback)
         val cont = Context.fromTerms(context.map(parseTerm(_, nbase, callback)).toList)
         if (cont.isEmpty)
            throw new ParseError("at least one variable required in " + cont.toString)
         val cond = condition.map(parseTerm(_, nbase, callback))
         val bod = parseTerm(body, nbase, callback)
         OMBINDC(bind, cont, cond, bod)
      }
      N match {
         case <OMS/> =>
            parseOMS(N, base) match {
               case p : GlobalName => callback(p); OMID(p)
               case p => throw new ParseError("Not a term: " + p + N.toString)
            }
         case <OMV/> =>
            OMV(xml.attr(N,"name"))
         case <OMA>{child @ _*}</OMA> if child.length == 3 && parseOMS(child.head, base) == mmt.morphismapplication =>
            val arg = parseTerm(child(1), nbase, callback)
            val morph = parseMorphism(child(2), nbase, callback)
            OMM(arg, morph)
         case <OMA>{child @ _*}</OMA> =>
            if (child.length <= 1)
               throw ParseError("No arguments given: " + N.toString)
            val ch = child.toList.map(parseTerm(_, nbase, callback))
            OMA(ch.head,ch.tail)
         case <OME>{child @ _*}</OME> =>
            val ch = child.toList.map(parseTerm(_, nbase, callback))
            OME(ch.head,ch.tail)
         case <OMBIND>{binder}<om:OMBVAR>{context @ _*}</om:OMBVAR>{condition}{body}</OMBIND> =>
            				doBinder(binder, context, Some(condition), body)
         case <OMBIND>{binder}<om:OMBVAR>{context @ _*}</om:OMBVAR>{body}</OMBIND> =>
            doBinder(binder, context, None, body)
         case <OMATTR><OMATP>{key}{value}</OMATP>{rest @ _*}</OMATTR> =>
            val k = parseTerm(key, nbase, callback)
            if (! k.isInstanceOf[OMID])
               throw new ParseError("key must be OMS in " + N.toString)
            val v = parseTerm(value, nbase, callback)
            if (rest.length == 0)
               throw ParseError("not a well-formed attribution: " + N.toString)
            val n = if (rest.length == 1)
               rest(0) else <OMATTR>{rest}</OMATTR>
            val t = parseTerm(n, nbase, callback)
            OMATTR(t, k.asInstanceOf[OMID], v)
         case <OMFOREIGN>{_*}</OMFOREIGN> => OMFOREIGN(N)
         case <OMI>{i}</OMI> => OMI(BigInt(i.toString))
         case <OMSTR>{s}</OMSTR> => OMSTR(s.toString)
         case <OMF/> => OMF(xml.attr(N, "dec").toDouble) //TODO hex encoding
         case <OMOBJ>{o}</OMOBJ> => parseTerm(o, nbase, callback)
         case _ => throw ParseError("not a well-formed term: " + N.toString)
      }
   } 
   
   /** parses a morphism relative to a base address */
   def parseMorphism(N : Node, base : Path) : Morph = parseMorphism(N, base, p => ())
   /** parses a morphism relative to a base address and calls a function on every encountered identifier */
   def parseMorphism(N : Node, base : Path, callback : Path => Unit) : Morph = {
      //N can set local cdbase attribute; if not, it is copied from the parent
      val nbase = newBase(N, base)
      N match {
         case <OMOBJ>{mor}</OMOBJ> => parseMorphism(mor, base, callback)
         case <OMMOR>{mor}</OMMOR> => parseMorphism(mor, base, callback)
         case <OMS/> =>
            parseOMS(N, base) match {
               case p : MPath => callback(p); OMMOD(p)
               case (t: TheoryObj) % name => callback(t % name); OMDL(t, name)
/*               case doc ? mod ?? str =>
                  throw ParseError("not a well-formed link reference (case was removed): " + N.toString)
                  val p = doc ? (mod / str)
                  callback(p)
                  OMMOD(p) */
               case _ => throw ParseError("not a well-formed link reference: " + N.toString)
            }
         case <OMA>{child @ _*}</OMA> if child.length >= 2 && parseOMS(child.head, base) == mmt.composition =>
            val links = child.toList.tail.map(parseMorphism(_, nbase, callback))
            OMCOMP(links)
         case <OMA>{child @ _*}</OMA> if child.length == 2 && parseOMS(child(0), base) == mmt.identity =>
            val theory = parseTheory(child(1), nbase, callback)
            OMIDENT(theory)
         case _ => throw ParseError("not a well-formed morphism: " + N.toString)
      }
   }

  /** parses a theory object relative to a base address */
  def parseTheory(N : Node, base : Path) : TheoryObj = parseTheory(N, base, p => ())
  /** parses a theory object relative to a base address and calls a function on every encountered identifier */
  def parseTheory(N : Node, base : Path, callback : Path => Unit) : TheoryObj = {
     val nbase = newBase(N, base)
     N match {
        case <OMOBJ>{thy}</OMOBJ> => parseTheory(thy, base, callback)
        case <OMS/> =>
           parseOMS(N, base) match {
               case p : MPath => callback(p); OMMOD(p)
               case _ => throw ParseError("not a well-formed theory reference: " + N.toString)
           }
        case _ => throw ParseError("not a well-formed theory: " + N.toString)
    }
  }
  
  private def parseOMS(N : Node, base : Path) : Path = {
     val doc = new xml.URI(xml.attr(N,"base"))
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
      case OMEMPTY(f,_) => f
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
      case OMEMPTY(_,t) => t
      case OMMOD(path) => try {
         lib.get(path) match {case l: Link => l.to}
      } catch {
         case _ => throw Invalid("not a well-formed morphism: " + m)
      }
      case OMDL(t, _ ) => t
   }
}