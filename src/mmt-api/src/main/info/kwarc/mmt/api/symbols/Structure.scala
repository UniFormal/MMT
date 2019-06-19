package info.kwarc.mmt.api.symbols

import info.kwarc.mmt.api._
import libraries._
import modules._
import objects._
import presentation._

/**
 * MMT structures, given by a body and an optional definiens
 *
 * @param home the [[Term]] representing the parent theory
 * @param name the name of the structure
 * @param tpC the domain theory
 * @param isImplicit true iff the link is implicit
 */
class Structure(val home : Term, val name : LocalName, val tpC: TermContainer, val dfC: TermContainer, val isImplicit : Boolean) extends Declaration with Link with HasType {
   type ThisType = Structure
   val feature = "structure"
   /** the domain of a structure is its type */
   def fromC = tpC
   /** the domain of a structure is its home theory*/
   val toC = new FinalTermContainer(home)
   def namePrefix = name
   def isInclude = Include.unapply(this).isDefined

   def getComponents = List(TypeComponent(tpC), DefComponent(dfC))

   def getInnerContext = codomainAsContext
   
   def translate(newHome: Term, prefix: LocalName, translator: Translator,context : Context): Structure = {
     def tl(m: Term)= translator.applyModule(context, m)
     val res = new Structure(home, prefix/name, tpC map tl, dfC map tl, isImplicit)
     getDeclarations foreach {d =>
       res.add(d.translate(res.toTerm, LocalName.empty, translator,context))
     }
     res
   }
   def merge(that: Declaration): Structure = {
     that match {
       case that: Structure =>
         val res = new Structure(this.home, this.name, tpC.copy, dfC.copy, isImplicit)
         // TODO maybe use val dfM = that.dfC merge this.dfC
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
   
   private def nameOrKeyword = this match {
      case Include(id) => if (id.isRealization) "realize " else "include "
      case _ => implicitString + feature + " " + name + " : "
   }
   protected def outerString = nameOrKeyword + from.toString
   
   def toNode = {
      val nameAtt = if (isInclude) null else name.toPath
      val implAtt = if (isImplicit) "true" else null
      val node = <import name={nameAtt} implicit={implAtt}>{headerNodes}{innerNodes}</import>
      val fromN = Obj.toStringOrNode(from)
      utils.xml.addAttrOrChild(node, "from", fromN)
   }
}

/** apply/unapply functions for [[DeclaredStructure]]s whose domain is an MPath */
object SimpleDeclaredStructure {
   def apply(home : Term, name : LocalName, tp: MPath, isImplicit : Boolean) =
      new Structure(home, name, TermContainer(OMMOD(tp)), new TermContainer(), isImplicit)
   def unapply(ce: ContentElement) = ce match {
      case SimpleStructure(s: Structure, p) => Some((s.home, s.name, p, s.isImplicit))
      case _ => None
   }
}

/** auxiliary functions */
object Structure {
   def apply(home : Term, name : LocalName, from : Term, isImplicit : Boolean): Structure = apply(home, name, from, None, isImplicit)
   def apply(home : Term, name : LocalName, from : Term, df: Option[Term], isImplicit : Boolean): Structure =
      new Structure(home, name, TermContainer(from), TermContainer(df), isImplicit)
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
/* an include can be constitutive/definitional (typical include) or postulated (= realizations)
 * in the latter case, we set the implicit flag to false; this is awkward but works for now
 * The two concepts coincide if there is a definiens.
 */
object Include {
   // if definiens is given, any args are shifted into the definiens as OMINST
   def apply(home: Term, from: MPath, args: List[Term], df: Option[Term] = None, postulated: Boolean = false): Structure = {
      val (argsN,dfN) = if (df.isEmpty)
        (args,df)
      else
        (Nil, Some(OMCOMP(OMINST(from,args) :: df.toList)))
      Structure(home, LocalName(from), OMPMOD(from, argsN), dfN, !postulated)
   }
   def unapply(t: ContentElement) : Option[IncludeData] = t match {
      case d: Structure => d.fromC.get match {
         case Some(OMPMOD(from, args)) if d.name == LocalName(from) => Some(IncludeData(d.home, from, args, d.df, !d.isImplicit))
         case _ => None
      }
      case _ => None
   }
}

/** auxiliary class that collects information about a structure that acts like an include
 *  @param home the module in which this include is declared (theory, view, etc.)
 *  @param from the domain of the included theory
 *  @param args instantiations of the parameters of from (if any)
 *  @param df definiens (i.e., the included morphism if in a theory)
 *  @param postulated if true, this specifies a structural include that must be implemented by the containing theory;
 *   this becomes available as a morphism only at the end of the containing theory (even if there is a definiens,
 *     which can happen, e.g., if the definiens refers to other postulated includes) 
 *  
 *  invariants: if df contains mor then args.isEmpty && from is domain of df
 *              else OMPMOD(from,args) is included theory
 */
case class IncludeData(home: Term, from: MPath, args: List[Term], df: Option[Term], postulated: Boolean) {
  /** OMIDENT(from) or OMINST(from, args) or OMCOMP(the-former, df); OMStructuralInclude for realizations */
  def asMorphism = {
    if (isRealization) OMStructuralInclude(from, home.toMPath)
    else OMCOMP(OMINST(from, args) :: df.toList)
  }

  def toStructure = Include(home, from, args, df, postulated)
  
  /** true if this represents a realization */
  def isRealization = postulated
  
  def isPlain = (home,args,df) match {
    case (OMMOD(h),Nil,None) => Some((h,from))
    case _ => None
  }
  def isDefined = df map {d => (from,d)}
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
      case Include(id) => id.isPlain
      case _ => None
   }
}