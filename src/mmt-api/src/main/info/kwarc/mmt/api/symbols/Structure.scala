package info.kwarc.mmt.api.symbols

import info.kwarc.mmt.api._
import libraries._
import modules._
import objects._
import presentation._

/**
 * A Structure represents an MMT structure.
 * 
 * Structures be declared (given by a list of assignments) or defined (given by an existing morphism).
 * These cases are distinguished by which subtrait of Link is mixed in.
 * 
 * @param parent the [[Path]] of the parent theory (also the codomain of the link)
 * @param name the name of the view
 * @param from the domain theory
 */
abstract class Structure extends Declaration with Link {
   type ThisType = Structure
   val feature = "structure"
   /** the domain/type of the structure */
   val tpC: TermContainer
   /** the domain of the structure as a Term, may fail if tpC is undefined */
   def from: Term = tpC.get.getOrElse {throw ImplementationError("access of unknown structure domain in " + path)}
   /** the domain of a structure is its home theory*/
   val to = home
   val isImplicit: Boolean
   def namePrefix = name
   
   def isInclude = Include.unapply(this).isDefined
   private def nameOrKeyword = this match {
      case Include(_, fromPath, _) => "include "
      case _ => name + " : "
   }
   /** override in order to permit implicit structures (identified by their domain) */
   override def implicitKey = this match {
      case Include(_, fromPath, _) => Some(fromPath)
      case _ => None
   }

   protected def outerString = nameOrKeyword + from.toString
   def toNode = {
      val nameAtt = if (isInclude) null else name.toPath
      val (fromAtt,fromNode) = backend.ReadXML.makeTermAttributeOrChild(from, "from")
      val implAtt =if (isInclude) null else if (isImplicit) "true" else null 
      <import name={nameAtt} from={fromAtt} implicit={implAtt}>{fromNode}{innerNodes}</import>
   }
}

/**
 * A DeclaredStructure represents an MMT structure given by a list of assignments.<p>
 * 
 * @param home the [[Term]] representing the parent theory
 * @param name the name of the structure
 * @param from the domain theory
 * @param isImplicit true iff the link is implicit
 */
class DeclaredStructure(val home : Term, val name : LocalName, val tpC: TermContainer, val isImplicit : Boolean)
      extends Structure with DeclaredLink {
   def getComponents = List(DomComponent(tpC))

   def translate(newHome: Term, prefix: LocalName, translator: Translator): DeclaredStructure = {
     def tl(m: Term)= translator.applyModule(Context.empty, m)
     val res = new DeclaredStructure(home, prefix/name, tpC map tl, isImplicit)
     getDeclarations foreach {d =>
       res.add(d.translate(res.toTerm, LocalName.empty, translator))
     }
     res
   }
   def merge(that: Declaration): Structure = {
     that match {
       case that: DefinedStructure =>
         new DefinedStructure(this.home, this.name, tpC.copy, that.dfC, isImplicit)
       case that: DeclaredStructure =>
         val res = new DeclaredStructure(this.home, this.name, tpC.copy, isImplicit)
         // TODO we may have to copy dThis and dThat
         this.getDeclarations foreach {dThis =>
            res.add(dThis)
         }
         that.getDeclarations.foreach {dThat =>
           this.getO(dThat.name) match {
             case None => res.add(dThat)
             case Some(dThis) => res.update(dThis merge dThat)
           }
         }
         res
       case _ => mergeError(that)
     }
   }
}

 /**
  * A DefinedStructure represents an MMT structure given by an existing morphism.
  * 
  * @param home the [[Term]] representing the parent theory
  * @param name the name of the structure
  * @param from the domain theory
  * @param df the definiens (the target if we see this as an assignment to a structure)
  * @param isImplicit true iff the link is implicit
  */
class DefinedStructure(val home : Term, val name : LocalName,
                       val tpC: TermContainer, val dfC : TermContainer, val isImplicit : Boolean)
      extends Structure with DefinedLink {
   def getComponents = List(DomComponent(tpC), DefComponent(dfC))
   
   def translate(newHome: Term, prefix: LocalName, translator: Translator): DefinedStructure = {
     def tl(m: Term)= translator.applyModule(Context.empty, m)
     new DefinedStructure(home, prefix/name, tpC map tl, dfC map tl, isImplicit)
   }
   def merge(that: Declaration): DefinedStructure = {
     that match {
       case that: DefinedStructure =>
         val dfM = that.dfC merge this.dfC
         new DefinedStructure(home, name, tpC.copy, dfM, isImplicit)
       case that: DeclaredStructure =>
         // this is a degenerate case
         new DefinedStructure(home, name, tpC.copy, dfC.copy, isImplicit)
       case _ => mergeError(that)
     }
   }
}

/** apply/unapply functions for [[DeclaredStructure]]s whose domain is an MPath */
object SimpleDeclaredStructure {
   def apply(home : Term, name : LocalName, tp: MPath, isImplicit : Boolean) =
      new DeclaredStructure(home, name, TermContainer(OMMOD(tp)), isImplicit) // TODO add metamorph?
   def unapply(ce: ContentElement) = ce match {
      case SimpleStructure(s: DeclaredStructure, p) => Some((s.home, s.name, p, s.isImplicit))
      case _ => None
   }
}

/** auxiliary functions */
object DeclaredStructure {
   def apply(home : Term, name : LocalName, from : Term, isImplicit : Boolean) =
      new DeclaredStructure(home, name, TermContainer(from), isImplicit) // TODO add metamorph?
}

/** auxiliary functions */
object DefinedStructure {
   def apply(home : Term, name : LocalName, from : Term, df: Term, isImplicit : Boolean) =
      new DefinedStructure(home, name, TermContainer(from), TermContainer(df), isImplicit)
}

/**
 * this can be wrapped around a pattern for matching a structure, e.g.,
 * case SimpleStructure(s, fromPath)
 */
object SimpleStructure {
   def unapply(ce: ContentElement) = ce match {
      case s: Structure => s.from match {
         case OMMOD(from) => Some((s,from))
         case _ => None
      }
      case _ => None
   }
}

/**
 * unnamed imports with automatic sharing are represented as special [[Structure]]s
 * 
 * they do not carry assignments
 * their name is LocalName(from)
 */
object Include {
   //TODO can there be assignments?
   def apply(home: Term, from: MPath, args: List[Term]) =
      DeclaredStructure(home, LocalName(from), OMPMOD(from, args), true)
   def unapply(t: ContentElement) : Option[(Term,MPath,List[Term])] = t match {
      case d: DeclaredStructure => d.from match {
         case OMPMOD(from, args) if d.name == LocalName(from) => Some((d.home, from, args))
         case _ => None
      }
      case _ => None
   }
}

/**
 * A PlainInclude represents an MMT inclusion between theories.
 *
 * @param from the domain of the inclusion
 * @param to the codomain of the inclusion
 */
object PlainInclude {
   def apply(from : MPath, to : MPath) = Include(OMMOD(to), from, Nil)
   def unapply(t: ContentElement) : Option[(MPath,MPath)] = t match {
      case Include(OMMOD(to), from, Nil) => Some((from, to))
      case _ => None
   }
}