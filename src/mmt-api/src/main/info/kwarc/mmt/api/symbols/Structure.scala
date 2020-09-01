package info.kwarc.mmt.api.symbols

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._

/**
  * MMT structures, given by a body and an optional definiens
  *
  * @param home       the [[Term]] representing the parent theory
  * @param name       the name of the structure
  * @param tpC        the domain theory
  * @param isImplicit true iff the link is implicit; only allowed if the structure is inside a theory
  */
class Structure(val home: Term, val name: LocalName, val tpC: TermContainer, val dfC: TermContainer, val isImplicit: Boolean, val isTotal: Boolean) extends Declaration with Link with HasType {
  type ThisType = Structure
  val feature = "structure"

  /** the domain of a structure is its type */
  def fromC: TermContainer = tpC

  /** the codomain of a structure is its home theory */
  // TODO this is not the codomain for a structure assignment in a link
  val toC = new FinalTermContainer(home)

  def namePrefix: LocalName = name

  def isInclude: Boolean = Include.unapply(this).isDefined

  def getComponents = List(TypeComponent(tpC), DefComponent(dfC))

  def getInnerContext: Context = codomainAsContext

  def translate(newHome: Term, prefix: LocalName, translator: Translator, context: Context): Structure = {
    def tl(m: Term) = translator.applyModule(context, m)

    val res = new Structure(home, prefix / name, tpC map tl, dfC map tl, isImplicit, isTotal)
    getDeclarations foreach { d =>
      res.add(d.translate(res.toTerm, LocalName.empty, translator, context))
    }
    res
  }

  def merge(that: Declaration): Structure = {
    that match {
      case that: Structure =>
        val res = new Structure(this.home, this.name, tpC.copy, dfC.copy, isImplicit, isTotal)
        // TODO maybe use val dfM = that.dfC merge this.dfC
        this.getDeclarations foreach { dThis =>
          res.add(dThis)
        }
        that.getDeclarations.foreach { dThat =>
          this.getO(dThat.name) match {
            case None => res.add(dThat)
            case Some(dThis) => res.update(dThis merge dThat)
          }
        }
        res
      case _ => mergeError(that)
    }
  }

  protected def totalString: String = if (isTotal) "total " else ""

  private def nameOrKeyword = this match {
    case Include(id) => if (id.isRealization) "realize " else "include "
    case _ => implicitString + totalString + feature + " " + name + " : "
  }

  protected def outerString: String = nameOrKeyword + from.toString

  def toNode = {
    val nameAtt = if (isInclude) null else name.toPath
    val implAtt = if (isImplicit) "true" else null
    val totalAtt = if (isTotal) "true" else null
    val node = <import name={nameAtt} implicit={implAtt} total={totalAtt}>
      {headerNodes}{innerNodes}
    </import>
    val fromN = Obj.toStringOrNode(from)
    utils.xml.addAttrOrChild(node, "from", fromN)
  }
}

/** apply/unapply functions for [[SimpleDeclaredStructure]]s whose domain is an MPath */
object SimpleDeclaredStructure {
  def apply(home: Term, name: LocalName, tp: MPath, isImplicit: Boolean, isTotal: Boolean = false) =
    new Structure(home, name, TermContainer(OMMOD(tp)), new TermContainer(), isImplicit, isTotal)

  def unapply(ce: ContentElement): Option[(Term, LocalName, MPath, Boolean, Boolean)] = ce match {
    case SimpleStructure(s: Structure, p) => Some((s.home, s.name, p, s.isImplicit, s.isTotal))
    case _ => None
  }
}

/** auxiliary functions */
object Structure {
  def apply(home: Term, name: LocalName, from: Term, isImplicit: Boolean, isTotal: Boolean): Structure = apply(home, name, from, None, isImplicit, isTotal)

  def apply(home: Term, name: LocalName, from: Term, df: Option[Term], isImplicit: Boolean, isTotal: Boolean): Structure =
    new Structure(home, name, TermContainer(from), TermContainer(df), isImplicit, isTotal)
}

/**
  * this can be wrapped around a pattern for matching a structure, e.g.,
  * case SimpleStructure(s, fromPath)
  */
object SimpleStructure {
  def unapply(ce: ContentElement): Option[(Structure, MPath)] = ce match {
    case s: Structure => s.from match {
      case OMMOD(from) => Some((s, from))
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
  def apply(home: Term, from: MPath, args: List[Term], df: Option[Term] = None, total: Boolean = false): Structure = {
    val (argsN, dfN) = if (df.isEmpty)
      (args, df)
    else
      (Nil, Some(OMCOMP(OMINST(from, home.toMPath, args) :: df.toList)))
    Structure(home, LocalName(from), OMPMOD(from, argsN), dfN, isImplicit = true, isTotal = total)
  }

  def unapply(t: ContentElement): Option[IncludeData] = t match {
    case d: Structure => d.fromC.get match {
      case Some(OMPMOD(from, args)) if d.name == LocalName(from) => Some(IncludeData(d.home, from, args, d.df, d.isTotal))
      case _ => None
    }
    case _ => None
  }
}

/** Auxiliary class that collects information about a structure that acts like an include.
  *
  * @param home  The module in which this include is declared (e.g. a theory T, view V, etc.)
  * @param from  The domain D of the included theory (into T, or into domain of V)
  * @param args  Instantiations of the parameters of from (if any, e.g. for parametric theories)
  * @param df    Definiens (of type D(args) -> T, or D(args) -> codomain of V)
  * @param total A total include is one that must be implemented by the containing theory
  *              this becomes available as a morphism only at the end of the containing theory (even if there is a definiens,
  *              which can happen, e.g., if the definiens refers to other total includes)
  *
  *              invariants: if df contains mor then args.isEmpty && from is domain of df
  *              else OMPMOD(from,args) is included theory
  *
  *              Note that concrete syntax may allow "include df" because D because can be infered;
  *              in a theory, "include D" is the standard for includes without definiens;
  *              in a view, we may also allow "include D" for the case where df is the identity of D.
  *
  *              See the examples below!
  * @see [[https://uniformal.github.io//doc/language/implicit.html]]
  * @example A [[Theory]] T includes a theory S in the usual way:
  *          '''
  *          theory S : ?someMetaTheory =
  *          ...
  *          ❚
  *          theory T : ?someMetaTheory =
  *          include ?S ❙
  *          ❚
  *          '''
  *          Both theories are non-parametric, i.e. as usual. Then [[Theory.getAllIncludes T.getAllIncludes]] will
  *          contain an `IncludeData(T.path, S.path, Nil, None, false)`.
  * @example Assume S, T as above, as well as a [[View]] v: T -> R including another view w: S -> R:
  *          '''
  *          view w : S -> R = ... ❚
  *          view v : T -> R =
  *          include ?S ❘ = ?w ❙
  *          ❚
  *          '''
  *          Then [[View.getAllIncludes v.getAllIncludes]] will contain an
  *          `IncludeData(v.path, S.path, Nil, w.path, false)`.
  * @example Let R, S, T be as below:
  *          '''
  *          theory R : ?someMetaTheory =
  *          ...
  *          ❚
  *          theory S : ?someMetaTheory =
  *          include ?R❙
  *          ...
  *          ❚
  *          theory T : ?someMetaTheory =
  *          include ?R ❙
  *          ...
  *          ❚
  *          '''
  *
  *          Pictorially, this is an inclusion triangle (with one missing edge):
  *          '''
  *          R
  *          / \
  *          T   S
  *          '''
  *          Now suppose we want a [[View]] v: T -> S which is the identity on R, i.e. v_R = id_R. In surface syntax
  *          we can do this as follows:
  *          '''
  *          view v: T -> S =
  *          include ?R❙
  *          ❚
  *          '''
  *          With this [[View.getAllIncludes v.getAllIncludes]] will contain an
  *          `IncludeData(v.path, R.path, Nil, Some(OMIDENT(R.path)), false)`.
  * @example If you have a [[View]] v: T -> S between two theories with the same meta theory mt, then
  *          [[View.getAllIncludes v.getAllIncludes]] will automatically include
  *          `IncludeData(v.path, mt.path, Nil, Some(OMIDENT(mt.path)), false)` similar to the previous example
  *          where the view is the identity on the included theory.
  */
case class IncludeData(home: Term, from: MPath, args: List[Term], df: Option[Term], total: Boolean) {
  /** OMIDENT(from) or OMINST(from, args) or OMCOMP(the-former, df); OMStructuralInclude for realizations */
  def asMorphism: Term = {
    if (isRealization) OMStructuralInclude(from, home.toMPath)
    else OMCOMP(OMINST(from, home.toMPath, args) :: df.toList)
  }

  def toStructure = Include(home, from, args, df, total)

  /** true if this represents a realization */
  def isRealization: Boolean = total

  def isPlain: Option[(MPath, MPath)] = (home, args, df) match {
    case (OMMOD(h), Nil, None) => Some((from, h))
    case _ => None
  }

  def isDefined: Option[(MPath, Term)] = df map { d => (from, d) }
}

/**
  * A PlainInclude represents an MMT inclusion between theories.
  */
object PlainInclude {
  /**
    * @param from the domain of the inclusion
    * @param to   the codomain of the inclusion
    * @return A plain inclusion between MMT theories
    */
  def apply(from: MPath, to: MPath) = Include(OMMOD(to), from, Nil)

  def unapply(t: ContentElement): Option[(MPath, MPath)] = t match {
    case Include(id) => id.isPlain
    case _ => None
  }
}