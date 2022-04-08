package info.kwarc.mmt.api.objects

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.notations._
import info.kwarc.mmt.api.objects.Conversions._
import info.kwarc.mmt.api.presentation.PresentationContext
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.utils._

import scala.xml.{Attribute, Elem, Node, Null}


/** represents an MMT term variable declaration
  *
  * @param name name
  * @param tp   optional type
  * @param df   optional definiens
  * @param not  optional notation
  */
case class VarDecl(name: LocalName, feature: Option[String], tp: Option[Term], df: Option[Term], not: Option[TextNotation]) extends Obj with NamedElement {
  type ThisType = VarDecl

  def toTerm = OMV(name)

  /** self-written copy method to copy metadata */
  def copy(name: LocalName = this.name, feature: Option[String] = this.feature, tp: Option[Term] = this.tp, df: Option[Term] = this.df,
           not: Option[TextNotation] = this.not) = {
    val vd = VarDecl(name, feature, tp, df, not)
    vd.copyFrom(this)
    vd
  }

  /** applies a function to type and definition */
  def map(f: Term => Term) = copy(tp = tp map f, df = df map f)

  def substitute(sub: Substitution)(implicit sa: SubstitutionApplier) = map(_ ^^ sub)

  private[objects] def freeVars_ = (tp map {
    _.freeVars_
  }).getOrElse(Nil) ::: (df map {
    _.freeVars_
  }).getOrElse(Nil)
  private[objects] def paths_ = tp.map(_.paths_).getOrElse(Nil) ::: df.map(_.paths_).getOrElse(Nil)

  def subobjects = subobjectsNoContext(tp.toList ::: df.toList)

  def head = tp.flatMap(_.head)

  /** true if this is stronger than that */
  def subsumes(that: VarDecl): Boolean = this.name == that.name && this.feature == that.feature && List((this.tp, that.tp), (this.df, that.df)).forall {
    case (_, None) => true
    case (None, Some(_)) => false
    case (Some(t1), Some(t2)) => t1 == t2
  }

  def toStr(implicit shortURIs: Boolean) = this match {
    case IncludeVarDecl(_, OMPMOD(p, args), df) => p.toString + args.map(_.toStr).mkString(" ")
    case _ => name.toString + tp.map(" : " + _.toStr).getOrElse("") + df.map(" = " + _.toStr).getOrElse("")
  }

  def toNode = <om:OMV name={name.toPath} feature={feature.orNull}>
    {mdNode}{tpN}{dfN}
  </om:OMV>

  private def tpN = tp.map(t => <type>
    {t.toNode}
  </type>).getOrElse(Nil)

  private def dfN = df.map(t => <definition>
    {t.toNode}
  </definition>).getOrElse(Nil)

  /** converts to an OpenMath-style attributed variable using two special keys */
  def toOpenMath: Term = {
    val varToOMATTR = OMV(name)
    (tp, df) match {
      case (None, None) => varToOMATTR
      case (Some(t), None) =>
        OMATTR(varToOMATTR, OMID(mmt.mmttype), t)
      case (None, Some(d)) => OMATTR(varToOMATTR, OMID(mmt.mmtdef), d)
      case (Some(t), Some(d)) => OMATTR(OMATTR(varToOMATTR, OMID(mmt.mmttype), t), OMID(mmt.mmtdef), d)
    }
  }

  def toConstant(mp: MPath, con: Context) = {
    val sub = con.map(vd => vd.name / (OMS(mp ? vd.name)))
    symbols.Constant(OMMOD(mp), name, Nil, tp map (_ ^? sub), df map (_ ^? sub), None)
  }

  def toOML = OML(name, tp, df)

  def toDeclaration(home: Term): Declaration = feature match {
    case None =>
      Constant(home, name, Nil, tp, df, None, NotationContainer(not))
    case Some(f) => this match {
      case IncludeVarDecl(_, OMPMOD(p, args), _) =>
        Include(home, p, args) //TODO defined include
      case StructureVarDecl(n, from, dfO) =>
        Structure(home, name, from, dfO, false, false)
      case DerivedVarDeclFeature(n, f, tp, None) =>
        new DerivedDeclaration(home, n, f, TermContainer(tp), NotationContainer(not), TermContainer(df))
    }
  }
}

/** use this to create apply/unapply functions for variable declarations for a specific feature */
class DerivedVarDeclFeature(val feature: String) {
  val path = Path.parseS("http://cds.omdoc.org/mmt?mmt?StructuralFeature", NamespaceMap.empty)

  def maketerm(feat: String, tp: Term) =
    OMA(OMS(path), List(OML(LocalName(feat)), tp))

  def apply(name: LocalName, tp: Term, df: Option[Term] = None, nt: Option[TextNotation] = None) =
    VarDecl(name, Some(feature), Some(tp), df, nt)

  def unapply(vd: VarDecl): Option[(LocalName, Term, Option[Term])] = {
    if (vd.feature contains feature) {
      vd match {
        case VarDecl(n, _, Some(tp), df, None) => Some((n, tp, df))
        case _ => throw ImplementationError("unsupported properties of derived variable declaration")
      }
    } else
      None
  }
}

object DerivedVarDeclFeature {
  def apply(name: LocalName, feat: String, tp: Term, df: Option[Term] = None) = VarDecl(name, Some(feat), Some(tp), df, None)

  def unapply(vd: VarDecl): Option[(LocalName, String, Term, Option[Term])] = vd match {
    case VarDecl(n, Some(f), Some(tp), df, _) => Some((n, f, tp, df))
    case _ => None
  }
}

object IncludeVarDecl extends DerivedVarDeclFeature("include") {
  def apply(p: MPath, args: List[Term]): VarDecl = apply(LocalName(p), OMPMOD(p, args))
}

object StructureVarDecl extends DerivedVarDeclFeature("structure")

/**
  * An MMT context as a list of variable declarations [[VarDecl]].
  *
  * Being a list especially implies that there is an **order of the entries**.
  * For example, this is important when using the [[Context]] in an [[OMBINDC]]
  * to represent a lambda term: (λxy. x + y) will be represented as [[OMBINDC]]
  * whose context has [[VarDecl]] entries "x", "y" in that order.<br>
  * Another situation where order is important is when later [[VarDecl]]
  * declarations refer to previous ones.
  *
  * Note that `variables` is not necessarily meant in a strict sense of only
  * variables, namely a context can also include declarations of a whole theory
  * and all of its (transitively) included theories by use of
  * [[IncludeVarDecl]]. There is an helper apply method on the companion
  * object of [[Context]]:
  * ```scala
  *   Context(MPath(/* ... */))
  * ```
  *
  * @param variables The context's variables
  */
case class Context(variables: VarDecl*) extends Obj with ElementContainer[VarDecl] with DefaultLookup[VarDecl] {
  type ThisType = Context

  def getDeclarations = variables.toList

  /** add a theory inclusion at the end */
  def ++(p: MPath): Context = this ++ Context(p)

  /** add variable at the end */
  def ++(v: VarDecl): Context = this ++ Context(v)

  /** concatenate contexts */
  def ++(that: Context): Context = this ::: that

  /** look up a variable by name, throws LookupError if not declared */
  def apply(name: LocalName): VarDecl = getO(name).getOrElse {
    throw LookupError(name, this)
  }

  /** @return the de Bruijn index of the variable, starting from 0 */
  def index(name: LocalName): Option[Int] = variables.lastIndexWhere(_.name == name) match {
    case -1 => None
    case i => Some(variables.length - i - 1)
  }

  def length = variables.length // can't be taken from implicit conversion to List because that competes with conversion to String

  /** @return the prefix up to and excluding the variable */
  def before(name: LocalName): Context = {
    index(name) match {
      case None => Context.empty
      case Some(i) => Context(variables.take(variables.length - i - 1): _*)
    }
  }

  /** @return the suffix after and excluding the variable */
  def after(name: LocalName): Context = {
    index(name) match {
      case None => this
      case Some(i) => Context(variables.drop(variables.length - i): _*)
    }
  }

  /** the subcontext whose variables occur (transitively closed) in the given variables
    *
    * if context |- t, then also context.minimalSubContext(t.freeVars) |- t
    */
  def minimalSubContext(req: List[LocalName]): Context = {
    if (req.isEmpty) return Context.empty // optimization
    var required = req
    val vds = variables.reverse.filter { vd =>
      if (required contains vd.name) {
        required = (required diff List(vd.name)) ::: vd.freeVars
        true
      } else
        false
    }
    Context(vds.reverse: _*)
  }

  /** a sound but not necessary complete criterion for whether this context is stronger than that one */
  // TODO could return true more often, but covers the practically important cases
  def subsumes(that: Context): Boolean = {
    val thisNames = this.variables.toList.map(_.name)
    val thatNames = that.variables.toList.map(_.name)
    if (hasDuplicates(thisNames) || hasDuplicates(thatNames))
      return false // this is too conservative, but shadowing is tricky
    if (thisNames.filter(thatNames.contains) != thatNames)
      return false // this is too conservative: some reordering can be allowed, but it is tricky when shadowing declarations from a common outer context
    this.forall { vd1 =>
      that.find(_.name == vd1.name) match {
        case None => true
        case Some(vd2) => vd1 subsumes vd2
      }
    }
  }

  /**
    * @return domain of this context, flattening nested ComplexTheories and ComplexMorphisms
    *
    *         Due to ComplexMorphism's, names may erroneously be defined but not declared.
    */
  def getDomain: List[DomainElement] = {
    var des: List[DomainElement] = Nil
    variables foreach {
      case StructureVarDecl(name, tp, df) =>
        val (total, definedAt): (Boolean, List[LocalName]) = df match {
          case None =>
            // no definition for any declaration in tp
            (false, Nil)
          case Some(ComplexMorphism(subs)) =>
            // partial morphism, defined at dom
            //TODO this does not cover the morphism case because tp with be empty
            val dom = subs.asContext.getDomain.map { de => de.name / name }
            (false, dom)
          case Some(morph) =>
            // everything else is a total morphism from tp, i.e., everything is defined
            (true, Nil) // second component is irrelevant
        }
        des ::= DomainElement(name, total, Some((tp, definedAt)))
      case VarDecl(n, _, _, df, _) =>
        des ::= DomainElement(n, df.isDefined, None)
    }
    des.reverse
  }

  /** all theories directly included into this context */
  def getIncludes: List[MPath] = variables.toList flatMap {
    case IncludeVarDecl(_, OMPMOD(p, args), _) => List(p)
    case _ => Nil
  }

  /** the identity substitution of this context */
  def id: Substitution = this map {
    vd => Sub(vd.name, OMV(vd.name))
  }

  /**
    * @return substitution that maps variables according to args, None if lengths do not match
    */
  def /(args: List[Term]): Option[Substitution] = {
    if (variables.length != args.length) None else {
      val s: Substitution = (variables.toList zip args).map { case (vd, a) => Sub(vd.name, a) }
      Some(s)
    }
  }

  def /!(args: List[Term]) = (this / args).getOrElse {
    throw ImplementationError("wrong number of arguments to substitute for context " + this + ": " + args.mkString(", "))
  }

  /** applies a function to the type/definiens of all variables (in the respective context)
    *
    * @return the resulting context
    */
  def mapTerms(f: (Context, Term) => Term): Context = {
    val newvars = variables.zipWithIndex map { case (vd, i) =>
      val con = Context(variables.take(i): _*)
      vd map {
        f(con, _)
      }
    }
    Context(newvars: _*)
  }

  /** @return an iterator over all declarations in their respective context */
  def declsInContext = new Iterator[(Context, VarDecl)] {
    private var sofar = Context()
    private var todo = variables

    def hasNext = !todo.isEmpty

    def next = {
      val hd :: tl = todo.toList
      val ret = (sofar, hd)
      sofar = sofar ++ hd
      todo = tl
      ret
    }
  }

  /** applies a function to each VarDecl, each time in the respective context
    *
    * @return the list of results
    */
  def mapVarDecls[A](f: (Context, VarDecl) => A): List[A] = {
    variables.zipWithIndex.toList map { case (vd, i) =>
      val con = Context(variables.take(i): _*)
      f(con, vd)
    }
  }

  /**
    * if c1 and c2 have the same length, then (c1 alpha c2 : c1 -> c2) is the substitution mapping corresponding variables
    *
    * This substitution can be used to alpha-rename c1-objects to c2-objects.
    */
  def alpha(that: Context): Option[Substitution] =
    if (this.length == that.length) {
      val subs = (this zip that).map { case (vd1, vd2) => vd1.name / OMV(vd2.name) }
      Some(Substitution(subs: _*))
    } else
      None

  /** substitutes in all variable declarations except for the previously declared variables
    * if |- G ++ H  and  |- sub : G -> G'  then  |- G' ++ (H ^ sub)
    * */
  def substitute(sub: Substitution)(implicit sa: SubstitutionApplier): Context = {
    val id = this.id // precompute value
    // sub ++ id.take(i) represents sub, x_1/x_1, ..., x_{i-1}/x_{i-1}
    val newvars = variables.zipWithIndex map { case (vd, i) => vd ^^ (sub ++ id.take(i)) }
    val ret = Context(newvars: _*)
    ret.copyFrom(this)
    ret
  }

  private[objects] def freeVars_ = {
    var except: List[LocalName] = Nil
    this flatMap { vd =>
      val fv = vd.freeVars_.filterNot(except contains _) //remove those variables that are declared previously in this context
      except ::= vd.name
      fv
    }
  }
  private[objects] def paths_ = this.flatMap(_.paths_)

  def subobjects = mapVarDecls { case (con, vd) => (con, vd) }

  /** returns this as a substitutions using those variables that have a definiens */
  def toPartialSubstitution: Substitution = {
    variables.toList mapPartial {
      case VarDecl(n, _, _, Some(df), _) => Some(Sub(n, df))
      case _ => None
    }
  }

  /** returns this as a substitution if all variables have a definiens */
  def toSubstitution: Option[Substitution] = {
    val subs = toPartialSubstitution
    if (subs.length == this.length) Some(subs) else None
  }

  def toStr(implicit shortURIs: Boolean) = this.map(_.toStr).mkString("", ", ", "")

  def toNode =
    <om:OMBVAR>
      {mdNode}{this.zipWithIndex.map({ case (v, i) => v.toNode })}
    </om:OMBVAR>

  def head = None

  def asDeclarations(home: Term): List[Declaration] = {
    var sub = Substitution()
    this.map { vd =>
      val d = (vd ^? sub).toDeclaration(home)
      sub ++ vd.name / d.toTerm
      d
    }
  }
}

/** a case in a substitution */
case class Sub(name: LocalName, target: Term) extends Obj {
  type ThisType = Sub

  def substitute(sub: Substitution)(implicit sa: SubstitutionApplier) = {
    val s = Sub(name, target ^^ sub)
    s.copyFrom(this)
    s
  }

  def map(f: Term => Term) = Sub(name, f(target))

  private[objects] def freeVars_ = target.freeVars_
  private[objects] def paths_ = target.paths_

  def subobjects = subobjectsNoContext(List(target))

  def toNode: Node = <om:OMV name={name.toString}>
    {mdNode}{target.toNode}
  </om:OMV>

  def toStr(implicit shortURIs: Boolean) = name + ":=" + target.toStr

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

  def unapply(s: Sub): Option[(LocalName, Term)] =
    s match {
      case Sub(n, mor) => Some((n, mor))
      case _ => None
    }
}

/** substitution between two contexts */
case class Substitution(subs: Sub*) extends Obj {
  type ThisType = Substitution

  def ++(n: String, t: Term): Substitution = this ++ Sub(LocalName(n), t)

  def ++(s: Sub): Substitution = this ++ Substitution(s)

  def ++(that: Substitution): Substitution = this ::: that

  def substitute(sub: Substitution)(implicit sa: SubstitutionApplier) = {
    val ret = this map { s => s ^^ sub }
    ret.copyFrom(this)
    ret
  }

  private[objects] def freeVars_ = (this flatMap {
    _.freeVars_
  })
  private[objects] def paths_ = this.flatMap(_.paths_)

  def subobjects = subobjectsNoContext(subs.toList)

  def maps(n: LocalName): Boolean = this exists {
    _.name == n
  }

  def domain = this.map(_.name)

  def apply(v: LocalName): Option[Term] = subs.reverse.find(_.name == v).map(_.target)

  def isIdentity: Boolean = subs forall {
    case Sub(n, OMV(m)) => m == n
    case _ => false
  }

  /** turns a substitution into a context by treating every substitute as a definiens
    * this permits seeing substitution application as a let-binding
    */
  def asContext = {
    val decls = subs map {
      case Sub(n, t) => VarDecl(n, None, None, Some(t), None)
    }
    Context(decls: _*)
  }

  def mapTerms(f: Term => Term): Substitution = this map { s =>
    Sub(s.name, f(s.target))
  }

  def toStr(implicit shortURIs: Boolean) = this.map(_.toStr).mkString("", ", ", "")

  // TODO this should not be OMBVAR
  def toNode =
    <om:OMBVAR>
      {mdNode}{subs.zipWithIndex.map(x => x._1.toNode)}
    </om:OMBVAR>

  def head = None

  def isEmpty = subs.isEmpty
}

/** helper object */
object Context {
  /** implicit conversion between a context and a list of variable declarations */
  implicit def list2context(l: List[VarDecl]): Context = Context(l: _*)

  implicit def context2list(c: Context): List[VarDecl] = c.variables.toList

  implicit def vardec2context(d: VarDecl): Context = Context(d)

  /** a context consisting of a single theory */
  def apply(p: MPath): Context = Context((IncludeVarDecl(p, Nil)))

  val empty: Context = Context()

  /** parses an OMBVAR into a context */
  def parse(Nmd: scala.xml.Node, nsMap: NamespaceMap): Context = {
    val (n, mdOpt) = metadata.MetaData.parseMetaDataChild(Nmd, nsMap)
    val c = xml.trimOneLevel(n) match {
      case <OMBVAR>{decls @ _*}</OMBVAR> => decls.toList.map(VarDecl.parse(_, nsMap))
      case _ => throw ParseError("not a well-formed context: " + n.toString)
    }
    mdOpt.foreach { md => c.metadata = md }
    c
  }

  private val sym = utils.mmt.context
  /** a typical notation for context: ,-separated bindings */
  val parsingRule = parser.ParsingRule(sym, Nil, TextNotation.fromMarkers(Precedence.integer(0), None)(Var(1, true, Some(Delim(",")))))
  val instanceParsingRule =
    parser.ParsingRule(utils.mmt.context, Nil, TextNotation.fromMarkers(Precedence.integer(0), None)(
      Delim("("), SimpSeqArg(1, Delim(","), CommonMarkerProperties.noProps), Delim(")")))

  /** helper functions to temporarily turn a context into a term, e.g., for checking contexts */
  object AsTerm {
    def apply(c: Context) = OMBINDC(OMS(sym), c, Nil)

    def unapply(t: Term) = t match {
      case OMBINDC(OMS(Context.sym), c, Nil) => Some(c)
      case _ => None
    }
  }

  object ParamsAsTerm {
    def apply(t: Term*) = OMA(OMS(sym), t.toList)

    def unapply(t: Term) = t match {
      case OMA(OMS(`sym`), args) => Some(args)
      case _ => None
    }
  }

  /** generate new variable name similar to x */
  private def rename(x: LocalName) = {
    x / "r"
  }

  /** picks a variable name that is fresh for context, preferably x1 */
  def pickFresh(context: Context, x1: LocalName): (LocalName, Substitution) = {
    var x = x1
    while (context.isDeclared(x)) {
      x = rename(x)
    }
    (x, x1 / OMV(x))
  }

  /** returns an alpha-renamed version of con that declares no variable from forbidden, and a substitution that performs the alpha-renaming */
  def makeFresh(con: Context, forbidden: List[LocalName]): (Context, Substitution) = {
    if (forbidden.forall(n => !con.isDeclared(n)))
    // optimization in case there is nothing to do
      return (con, con.id)
    var sub = Substitution()
    val conN = con map { vd =>
      var xn = vd.name
      while (forbidden contains xn) {
        xn = rename(xn)
      }
      val vdn = if (vd.name == xn && sub.isIdentity)
        vd
      else
        (vd ^? sub).copy(name = xn)
      sub = sub ++ OMV(vd.name) / OMV(xn)
      vdn
    }
    (conN, sub)
  }
}

/** helper object */
object VarDecl {
  def apply(n: LocalName, tp: Term = null, df: Term = null): VarDecl = VarDecl(n, None, Option(tp), Option(df), None)

  private def parseComponents(N: Seq[Node], nsMap: NamespaceMap): (Option[Term], Option[Term], Option[TextNotation]) = {
    var tp: Option[Term] = None
    var df: Option[Term] = None
    var not: Option[TextNotation] = None
    N.map(xml.trimOneLevel).foreach {
      case <type>{t}</type> => tp = Some(Obj.parseTerm(t, nsMap))
      case <definition>{t}</definition> => df = Some(Obj.parseTerm(t, nsMap))
      case <notation>{n}</notation> => not = Some(TextNotation.parse(n, nsMap))
      case n => throw ParseError("not a well-formed variable component: " + n.toString)
    }
    (tp, df, not)
  }

  def parse(Nmd: Node, nsMap: NamespaceMap): VarDecl = {
    val (n, mdOpt) = metadata.MetaData.parseMetaDataChild(Nmd, nsMap)
    xml.trimOneLevel(n) match {
      case <OMV>{body @ _*}</OMV> =>
        val name = LocalName.parse(xml.attr(n, "name"))
        val featureAtt = xml.attr(n, "feature")
        val feature = if (featureAtt == "") None else Some(featureAtt)
        val (tp, df, not) = parseComponents(body, nsMap)
        val vd = VarDecl(name, feature, tp, df, not)
        mdOpt.foreach { md => vd.metadata = md }
        vd
      case _ => throw ParseError("not a well-formed variable declaration: " + n.toString)
    }
  }
}

/** helper object */
object Substitution {
  /** implicit conversion between a substitution and a list of maps */
  implicit def list2substitution(l: List[Sub]): Substitution = Substitution(l: _*)

  implicit def substitution2list(s: Substitution): List[Sub] = s.subs.toList

  implicit def varsub2substitution(s: Sub): Substitution = Substitution(s)

  val empty = Substitution()

  /** parsers an OMBVAR into a substitution */
  def parse(Nmd: scala.xml.Node, nsMap: NamespaceMap): Substitution = {
    val (n, mdOpt) = metadata.MetaData.parseMetaDataChild(Nmd, nsMap)
    val s = xml.trimOneLevel(n) match {
      case <OMBVAR>{sbs @ _*}</OMBVAR> => sbs.toList.map(Sub.parse(_, nsMap))
      case _ => throw ParseError("not a well-formed substitution: " + n.toString)
    }
    mdOpt.foreach { md => s.metadata = md }
    s
  }
}

/** helper object */
object Sub {
  def parse(Nmd: Node, nsMap: NamespaceMap) = {
    val (n, mdOpt) = metadata.MetaData.parseMetaDataChild(Nmd, nsMap)
    val s = xml.trimOneLevel(n) match {
      case <OMV>
        {e}
        </OMV> => Sub(LocalName.parse(xml.attr(n, "name")), Obj.parseTerm(e, nsMap))
      case _ => throw ParseError("not a well-formed case in a substitution: " + n.toString)
    }
    mdOpt.foreach { md => s.metadata = md }
    s
  }
}
