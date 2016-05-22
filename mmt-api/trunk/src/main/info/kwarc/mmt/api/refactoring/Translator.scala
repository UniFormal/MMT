package info.kwarc.mmt.api.refactoring

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend.Extension
import info.kwarc.mmt.api.modules.{DeclaredLink, DeclaredTheory, DeclaredView}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.symbols.{DeclaredStructure, FinalConstant}
import info.kwarc.mmt.api.web.{Body, ServerExtension}

import scala.collection.mutable
import scala.util.{Success, Try}

/**
  * Top class for all translations
  */
abstract class Translation(val priority : Int) {
  val source : List[FullArchive]
  val target : List[FullArchive]

  // returns Some((p,l)) if applicable (None if not), where p is the symbol it applies to
  // (needed to prevent circular translation) and l the list of newly introduced symbols
  def isApplicable(t : Term) : Option[(GlobalName,List[GlobalName])]

  protected def translate(t : Term) : Term
  def apply(t : Term) : Term = {
    if (isApplicable(t).isDefined) translate(t) else t
  }

  override def toString = "Translation from " + source.map(_.name).mkString(", ") + " to " + target.map(_.name).mkString(", ")
}

/**
  * Groups translations together. Translations in the same group are preferably used jointly
  */
abstract class TranslationGroup extends Translation(0) {
  val translations : List[Translation]
  val source : List[FullArchive]
  val target : List[FullArchive]

  // similar as above, but in addition returns the specific translations that are applicable
  def isApplicable(t : Term) : Option[(GlobalName,List[GlobalName])] = translations.collectFirst {
    case tr if tr.isApplicable(t).isDefined => tr.isApplicable(t).get
  }
  def translate(t : Term) = translations.find(tr => tr.isApplicable(t).isDefined).map(tr => tr.apply(t)).get

  override def toString = "TranslationGroup from " + source.mkString(", ") + " to " + target.mkString(", ") +
    translations.map(" - " + _.toString).mkString("\n")
}

abstract class SimpleTranslation extends Translation(0) {
  val from : GlobalName
  val to : List[GlobalName]
}

sealed abstract class AlignmentTranslation(alignment : FormalAlignment) extends SimpleTranslation {
  require(alignment.to.mmturi.isInstanceOf[GlobalName])
  require(alignment.from.mmturi.isInstanceOf[GlobalName])
  val from = alignment.from.mmturi.asInstanceOf[GlobalName]
  val to = List(alignment.to.mmturi.asInstanceOf[GlobalName])

  override def toString = "AlignmentTranslation from " + source.map(_.name).mkString(", ") + " to " +
    target.map(_.name).mkString(", ") + "\nusing " + alignment.toString
}

case class SimpleLink(from : GlobalName, toTerm : Term, source : List[FullArchive], target: List[FullArchive], link:DeclaredLink) extends SimpleTranslation {
  val to = ArchiveStore.getSymbols(toTerm)

  lazy val toSymbols = ArchiveStore.getSymbols(toTerm)

  def isApplicable(t : Term) = t match {
    case OMS(p) if p == from => Some((p,toSymbols))
    case _ => None
  }

  protected def translate(t : Term) : Term = toTerm

  override def toString = "LinkTranslation from " + from.toString + " to " + toTerm.toString
}

sealed abstract class LinkTranslation extends TranslationGroup {
  val link : DeclaredLink
}

case class InverseViewTranslation(link : DeclaredView, source : List[FullArchive], target : List[FullArchive]) extends LinkTranslation {
  lazy val translations : List[Translation] = link.getDeclarations.collect{
    case x : FinalConstant if x.df.isDefined =>
      (x.name,x.df.get) match {
        case (LocalName(ComplexStep(path : MPath) :: ln),OMS(p)) =>
          Some(SimpleLink(p,OMS(path ? ln),source,target,link))
        case _ => None
      }
  }.collect{
    case Some(t) => t
  }

  override def toString = "InverseViewTranslation from " + source.map(_.name).mkString(", ") + " to " + target.map(_.name).mkString(", ") +
    "\nusing " + link.path.toString + "\n" +
    translations.map(" - " + _.toString).mkString("\n")
}

case class ViewTranslation(link : DeclaredView, source : List[FullArchive], target : List[FullArchive]) extends LinkTranslation {
  lazy val translations = link.getDeclarations.collect{
    case x : FinalConstant if x.df.isDefined =>
      x.name match {
        case LocalName(ComplexStep(path : MPath) :: ln) =>
          Some(SimpleLink(path ? ln, x.df.get,source,target,link))
        case _ => None
      }
  }.collect{
    case Some(t) => t
  }

  override def toString = "ViewTranslation from " + source.map(_.name).mkString(", ") + " to " + target.map(_.name).mkString(", ") +
    "\nusing " + link.path.toString + "\n" +
    translations.map(" - " + _.toString).mkString("\n")
}



class Translator extends ServerExtension("translate") {
  override def logPrefix = "Translator"

  override def start(args: List[String]) = {

  }

  def apply(path: List[String], query: String, body: Body) = {
    ???
  }

  private case class SimpleAlignmentTranslation(a: SimpleAlignment, source: List[FullArchive], target: List[FullArchive])
    extends AlignmentTranslation(a) {
    def isApplicable(t: Term) = t match {
      case OMS(p) if p == from => Some((from, to))
      case _ => None
    }

    protected def translate(t: Term): Term = t match {
      case OMS(p) if p == from => OMS(to.head)
      case _ => t // can not ever occur
    }
  }

  private case class ArgumentAlignmentTranslation(align: ArgumentAlignment, source: List[FullArchive], target: List[FullArchive])
    extends AlignmentTranslation(align) {

    abstract class Application {
      val symbol: Option[GlobalName]

      def unapply(t: Term): Option[(Term, List[Term])]

      def apply(f: Term, args: List[Term], ls: List[GlobalName]): Term
    }

    object OMApplication extends Application {
      val symbol = None

      def apply(f: Term, args: List[Term], ls: List[GlobalName]): Term = if (ls.isEmpty) OMA(f, args)
      else {
        val app = SymbolApplication(ls.head, this)
        app(f, args, ls.tail)
      }

      def unapply(t: Term) = t match {
        case OMA(f, args) => Some((f, args))
        case _ => None
      }
    }

    case class SymbolApplication(s: GlobalName, app: Application) extends Application {
      val symbol = Some(s)

      def apply(f: Term, args: List[Term], ls: List[GlobalName]): Term = if (ls.isEmpty) args.foldLeft(f)((o, a) => app(OMS(s), List(o, a), Nil))
      else {
        val napp = SymbolApplication(ls.head, this)
        napp(f, args, ls.tail)
      }

      def unapply(t: Term): Option[(Term, List[Term])] = t match {
        case app(OMS(sx), f :: args) if sx == s =>
          unapply(f) match {
            case None => Some((f, args))
            case Some((c, args0)) => Some((c, args0 ::: args))
          }
        case _ => None
      }
    }

    object Application {
      def apply(f: Term, args: List[Term], ls: List[GlobalName]) = OMApplication(f, args, ls)

      def unapply(t: Term): Option[(Term, List[Term], List[GlobalName])] = smartunapply(t, OMApplication)

      private def smartunapply(t: Term, app: Application): Option[(Term, List[Term], List[GlobalName])] = t match {
        case app(OMS(f), args) =>
          controller.get(f) match {
            case c: FinalConstant if c.rl contains "apply" =>
              val napp = SymbolApplication(f, app)
              smartunapply(t, napp).map(r => (r._1, r._2, f :: r._3))
            case _ =>
              Some((OMS(f), args, Nil))
          }
        case app(f, args) =>
          Some((f, args, Nil))
        case _ => None
      }
    }

    def isApplicable(t: Term) = t match {
      case OMS(p) if p == from => Some((from, to))
      case Application(OMS(p), args, _) if p == from => Some((from, to))
      case _ => None
    }

    private def reorder(ts: List[Term]): List[Term] = {
      val max = align.arguments.maxBy(p => p._2)._2
      (1 to max).map(i => {
        val ni = align.arguments.find(p => p._2 == i).map(_._1)
        if (ni.isEmpty) OMV(LocalName("_")) // TODO implicit arguments
        else ts(ni.get)
      }).toList
    }

    protected def translate(t: Term): Term = t match {
      case OMS(p) if p == from => OMS(to.head)
      case Application(OMS(p), args, appls) if p == from => Application(OMS(to.head), reorder(args), appls)
      case _ => t // can not ever occur
    }
  }

  case class StructureTranslation(link: DeclaredStructure, source: List[FullArchive], target: List[FullArchive]) extends LinkTranslation {
    val translations = link.from match {
      case OMMOD(p) =>
        controller.get(p) match {
          case th: DeclaredTheory =>
            th.getConstants.map(c => SimpleLink(c.path, OMS(link.path / c.name), source, target, link))
          case _ => Nil
        }
      case _ => Nil
    }

    override def toString = "StructureTranslation from " + source.map(_.name).mkString(", ") + " to " + target.map(_.name).mkString(", ") +
      "\nusing " + link.path.toString + "\n" +
      translations.map(" - " + _.toString).mkString("\n")
  }

  case class InverseStructureTranslation(orig: StructureTranslation) extends LinkTranslation {
    val target = orig.source
    val source = orig.target
    val link = orig.link
    val translations = orig.translations map {
      case SimpleLink(from, OMS(to), _, _, _) => SimpleLink(to, OMS(from), source, target, link)
    }

    override def toString = "InverseStructureTranslation from " + source.map(_.name).mkString(", ") + " to " + target.map(_.name).mkString(", ") +
      "\nusing " + link.path.toString + "\n" +
      translations.map(" - " + _.toString).mkString("\n")
  }

  var translations: List[Translation] = Nil
  var translationgroups: List[TranslationGroup] = Nil

  lazy val archives = controller.extman.get(classOf[ArchiveStore]).headOption.getOrElse {
    val a = new ArchiveStore
    controller.extman.addExtension(a)
    a
  }
  lazy val alignments = controller.extman.get(classOf[AlignmentsServer]).headOption.getOrElse {
    val a = new AlignmentsServer
    controller.extman.addExtension(a)
    a
  }

  def fromAlignment(a: FormalAlignment): List[AlignmentTranslation] = {
    val src = archives.find(a.from.mmturi)
    val trg = archives.find(a.to.mmturi)
    if (src.isEmpty || trg.isEmpty) return Nil
    a match {
      case al: SimpleAlignment =>
        val ret = if (a.invertible) List(SimpleAlignmentTranslation(al, src, trg),
          SimpleAlignmentTranslation(al.reverse.asInstanceOf[SimpleAlignment], trg, src))
        else List(SimpleAlignmentTranslation(al, src, trg))
        translations = (ret ::: translations).distinct
        ret
      case al: ArgumentAlignment =>
        val ret = if (a.invertible) List(ArgumentAlignmentTranslation(al, src, trg),
          ArgumentAlignmentTranslation(al.reverse.asInstanceOf[ArgumentAlignment], trg, src))
        else List(ArgumentAlignmentTranslation(al, src, trg))
        translations = (ret ::: translations).distinct
        ret
    }
  }

  def fromLink(v: DeclaredLink): Option[(LinkTranslation, LinkTranslation)] = {
    val (from, to) = (v.from, v.to) match {
      case (OMMOD(p: MPath), OMMOD(q: MPath)) => (p, q)
      case _ => return None
    }
    val src = archives.find(from)
    val trg = archives.find(to)
    if (src.isEmpty || trg.isEmpty) return None
    val ret = v match {
      case vw: DeclaredView =>
        (ViewTranslation(vw, src, trg), InverseViewTranslation(vw, trg, src))
      case str: DeclaredStructure =>
        val tr = StructureTranslation(str, src, trg)
        (tr, InverseStructureTranslation(tr))
    }

    add(ret._1)
    add(ret._2)
    Some(ret)
  }

  private def expandDefinition(p: GlobalName): Option[Term] = controller.get(p) match {
    case c: FinalConstant if c.df.isDefined => Some(c.df.get)
    case _ => None
  }

  /*
  private case class State(visited: List[GlobalName], applied: List[Translation], usedgroups: List[TranslationGroup]) {
    //def add(vs : GlobalName) = State(vs :: visited, applied,usedgroups)
    def add(vs: (GlobalName, List[GlobalName], Translation)) = State((vs._1 :: visited).distinct, (vs._3 :: applied).distinct, usedgroups)

    def add(gr: (TranslationGroup, GlobalName, List[GlobalName], Translation)) =
      State((gr._2 :: visited).distinct, (gr._4 :: applied).distinct, (gr._1 :: usedgroups).distinct)

    def +(s: State) = State(visited, (s.applied ::: applied).distinct, (s.usedgroups ::: usedgroups).distinct)
  }

  private def findApplicable(t: Term, state: State)(implicit target: FullArchive): List[State] = {
    // Use translations that have already been used
    var usedtranslations = state.applied.collect {
      case tr if tr.isApplicable(t).isDefined =>
        val ret = tr.isApplicable(t).get
        (ret._1, ret._2, tr)
    }.filter(p => !p._2.filter(!target.declares(_)).exists(state.visited.contains))
    if (usedtranslations.nonEmpty) return usedtranslations.map(u => state add u)

    // Use translation groups that have already been used
    val usedgroups = state.usedgroups.collect {
      case g if g.isApplicable(t).nonEmpty => g.isApplicable(t)
    }.flatten.filter(p => !p._2.filter(!target.declares(_)).exists(state.visited.contains))
    if (usedgroups.nonEmpty) return usedgroups.map(u => state add u)

    // find other applicable translations

    //open up new translation groups
    var returns : List[State] = Nil
    val newgroups = translationgroups.collect {
      case gr if gr.isApplicable(t).nonEmpty => gr.isApplicable(t).map(p => (gr, p._1, p._2, p._3))
    }.flatten.filter(p => !p._3.filter(!target.declares(_)).exists(state.visited.contains))
    returns = newgroups.map(gr => state add gr) ::: returns

    // find other applicable translations
    val applicables = translations.collect {
      case tr if tr.isApplicable(t).isDefined =>
        val ret = tr.isApplicable(t).get
        (ret._1, ret._2, tr)
    }.filter(p => !p._2.filter(!target.declares(_)).exists(state.visited.contains))
    returns = applicables.map(p => state add p) ::: returns

    returns
  }

  private def traverse(con: Context, state: State)(implicit target: FullArchive): List[(Context, State)] = {
    val ret = traverse(state, con.variables.map(v => v.tp.get): _*)
    ret.map {
      case (l, st) => (Context(con.indices.map(i => VarDecl(con(i).name, Some(l(i)), None, None)): _*), st)
    }
  }

  private def traverse(state: State, terms: Term*)(implicit target: FullArchive): List[(List[Term], State)] =
    if (terms.isEmpty) Nil
    else if (terms.length == 1) traverse(terms.head, state).map(p => (List(p._1), p._2))
    else {
      val last = traverse(terms.last, state) map {
        case (tm, st) =>
          (List(tm), st)
      }
      terms.dropRight(1).foldRight(last)({
        case (arg, list) => list flatMap {
          case (ls, st) =>
            traverse(arg, state + st) map {
              case (tm, nst) => (tm :: ls, nst)
            }
        }
      })
    }

  private def traverse(t: Term, state: State)(implicit target: FullArchive): List[(Term, State)] = {
    // target already declares the symbols:
    t match {
      case OMV(name) => return List((t, state))
      case OMS(p) if target.declares(p) => return List((t, state))
      case _ =>
    }

    // Find applicable translations and apply them
    val ways = findApplicable(t, state)
    if (ways.nonEmpty) ways.flatMap(s => traverse(s.applied.head.apply(t), s))

    // None applicable; traverse the term
    else t match {
      case OMS(p) =>
        // no translations for symbol p => Expand definition (if possible)
        val df = expandDefinition(p)
        if (df.isDefined) traverse(df.get, state)
        else Nil
      case OMA(f, args) =>
        traverse(state, f :: args: _*) map {
          case (ls, st) => (OMA(ls.head, ls.tail), st)
        }
      case OMBINDC(binder, con, bodies) =>
        val nbinds = traverse(binder, state)
        val ncons = nbinds flatMap {
          case (tm, st) =>
            val conts = traverse(con, state + st)
            conts.map {
              case (ncon, nst) => (tm, ncon, nst)
            }
        }
        ncons flatMap {
          case (nbinder, ncon, st2) =>
            traverse(state + st2, bodies: _*) map {
              case (ls, nst) => (OMBINDC(nbinder, ncon, ls), nst)
            }
        }
      case UnknownOMLIT(s, tm) => traverse(tm, state) map {
        case (ntm, st) => (UnknownOMLIT(s, ntm), st)
      }
      case OMLIT(value, rt) => traverse(rt.synType, state) map {
        case (ntm, st) => (UnknownOMLIT(value.toString, ntm), st)
      }
      case OML(VarDecl(name, tp, df, not)) =>
        val tps = if (tp.isDefined) traverse(tp.get, state).map(p => (Some(p._1), p._2)) else List((None, state))
        val dfs = tps flatMap {
          case (tpOpt, st) => if (df.isDefined) traverse(df.get, state + st).map(p => (tpOpt, Some(p._1), p._2)) else List((tpOpt, None, st))
        }
        dfs map {
          case (tpOpt, dfOpt, nst) => (OML(name, tpOpt, dfOpt), nst)
        }
      case _ => Nil
    }
  }
  */

  private def getContext(t : Term) : Context = {
    val syms = ArchiveStore.getSymbols(t)
    syms.map(m => Context(m.module)).foldLeft(Context.empty)((c1,c2) => c1 ++ c2)
  }

  /*
  private def apply(t : Term, target : FullArchive, simplify : Boolean) : List[Term] = traverse(t,State(Nil,Nil,Nil))(target).map(r =>
      if(simplify) controller.simplifier.apply(r._1,getContext(r._1)) else r._1).distinct
   */

  private object State {
    sealed class Status
    object done extends Status
    object open extends Status
    object checked extends Status
    case class blocked(value : Double) extends Status
    private var mods : List[(GlobalName,GlobalName)] = Nil
    private var origs : List[GlobalName] = Nil
    def reset(s : State) = {
      mods = Nil
      origs = s.syms
    }
    def revert(t : Term)(implicit target : FullArchive) : Term = {
      val trav = new StatelessTraverser {
        override def traverse(t: Term)(implicit con: Context, init: State): Term = t match {
          case OMS(p) if !target.declares(p) && !origs.contains(p) =>
            traverse(OMS(mods.find(q => q._2 == p).map(_._1).getOrElse(throw new Exception(""))))
          case _ => Traverser(this,t)
        }
      }
      trav(t,())
    }
    def addchange(from : GlobalName, to : GlobalName) = mods ::= (from,to)
  }

  private sealed abstract class State {
    val term : Term
    val orig : Term
    val traversed : List[GlobalName]
    val used : List[Translation]
    // val usedgroups : List[TranslationGroup]
    var status : State.Status
    private val topstate = this

    lazy val syms = ArchiveStore.getSymbols(term)
    def +(t : Term,travs : List[GlobalName],nused : List[Translation])(implicit target : FullArchive) = new State {
      val term = t
      val orig = topstate.orig
      val traversed = travs ::: topstate.traversed
      val used = nused ::: topstate.used
      var status = {
        val perc = syms.filter(p => target.declares(p))
        if (perc.length == syms.length) State.done else State.open
      }
    }

    def block(implicit target : FullArchive) = {
      val perc = syms.filter(p => target.declares(p))
      status = State.blocked(perc.length.toDouble / syms.length.toDouble)
    }
  }

  private def NewState(t : Term)(implicit target : FullArchive) : State = new State {
    val term = t
    val orig = t
    val traversed = Nil
    val used = Nil
    // val usedgroups = Nil
    var status = if (syms.forall(p => target.declares(p))) State.done else State.open
  }

  private case class ExpandDefinition(from : GlobalName,toTerm : Term) extends Translation(0) {
    lazy val toSymbols = ArchiveStore.getSymbols(toTerm)
    val target = Nil
    val source = Nil
    def isApplicable(t : Term) = t match {
      case OMS(p) if p == from => Some((p,toSymbols))
      case _ => None
    }

    protected def translate(t : Term) : Term = toTerm
  }

  private def findApplicable(state : State)(implicit target : FullArchive) : List[List[Translation]] = {
    var applicables : List[(Translation,GlobalName)] = Nil
    val trav = new StatelessTraverser {
      override def traverse(t: Term)(implicit con: Context, init: State): Term = {
        t match {
          case OMS(p) if target.declares(p) => return OMS(p)
          case OMS(p) =>
            val defi = expandDefinition(p)
            if (defi.isDefined) applicables ::= (ExpandDefinition(p,defi.get),p)
          case _ => {}
        }
        applicables :::= (translations ::: translationgroups).collect {
          case tr if tr.isApplicable(t).isDefined =>
            val ret = tr.isApplicable(t).get
            (tr,ret._1, ret._2)
        }.filter(p => !p._3.filter(!target.declares(_)).exists(state.traversed.contains)).map(tup => (tup._1,tup._2))
        Traverser(this,t)
      }
    }
    trav.apply(state.term,())
    if (applicables.isEmpty) {
      state.block
    }
    aggregate(applicables.groupBy(_._2).map(p => (p._1,p._2.map(_._1)))).map(_.distinct).distinct
  }

  private def aggregate(ls : Map[GlobalName,List[Translation]], dones : List[List[Translation]] = Nil)
  : List[List[Translation]] = {
    if (ls.isEmpty) dones
    else if (ls.head._2.length == 1) {
      if (dones.nonEmpty) aggregate(ls.tail,dones.map(l => ls.head._2.head :: l))
      else aggregate(ls.tail,List(List(ls.head._2.head)))
    }
    else aggregate(ls.tail,ls.head._2.flatMap(tr => dones.map(l => tr :: l)))
  }

  private def applytranslations(trls : List[Translation], state : State)(implicit target : FullArchive) : State = {
    var traversed : List[GlobalName] = Nil
    val trav = new StatelessTraverser {
      override def traverse(t: Term)(implicit con: Context, init: State): Term = {
        val ret = trls.collectFirst{case tr if tr.isApplicable(t).isDefined =>
          (tr,tr.isApplicable(t).get)}
        if (ret.isDefined) {
          traversed ::= ret.get._2._1
          State.addchange(ret.get._2._1,ret.get._2._2.head)
          Traverser(this,ret.get._1.apply(t))
        } else Traverser(this,t)
      }
    }
    state.status = State.checked
    state + (trav.apply(state.term,()),traversed,trls)
  }

  private def topapply(t : Term,getpartialresults : Boolean)(implicit target : FullArchive) : List[Term] = {
    var states = List(NewState(t))
    var newstates = states
    State.reset(states.head)
    while (newstates.nonEmpty && !newstates.exists(s => s.status == State.done)) {
      newstates = newstates.flatMap(st => st.status match {
        case State.open =>
          val apps = findApplicable(st)
          apps.map(tr => applytranslations(tr,st))
        case _ => Nil
      })
      states :::= newstates
    }
    val dones = states.filter(p => p.status == State.done)
    if (dones.nonEmpty) dones.map(_.term).distinct
    else if (getpartialresults) List(State.revert(states.collect {
      case st if st.status.isInstanceOf[State.blocked] => st
    }.maxBy(_.status.asInstanceOf[State.blocked].value).term))
    else Nil
  }

  def apply(t : Term, target : String, simplify : Boolean = true, getpartialresults : Boolean = false) : List[Term] = {
    val ret = topapply(t,getpartialresults)(archives.getArchive(target).getOrElse(return Nil))
    if(simplify) ret.map(r => controller.simplifier.apply(r,getContext(r))).distinct else ret
    // TODO simplify
  }

  def loadAlignments = {
    log("Getting Alignments from AlignmentServer...")
    logGroup {
      val als = alignments.getAll collect {
        case a: FormalAlignment => a
      }
      log(als.length + " Alignments found.")
      log("Adding Alignment Translations...")
      als foreach fromAlignment
      log("Done.")
    }
  }

  def loadLinks = {
    log("Collecting Links...")
    logGroup {
      archives.getArchives.foreach(a => a.read)
      val links = (controller.evaluator.evaluate(Paths(IsView)).asInstanceOf[ESetResult].h.toList :::
        controller.evaluator.evaluate(Paths(IsStructure)).asInstanceOf[ESetResult].h.toList).flatten.map(s =>
        Path.parse(s.toString)).map(p => Try(controller.get(p))) collect {
          case Success(l: DeclaredLink) => l
        }
      log(links.length + " Links found")
      log("Adding LinkTranslations...")
      links foreach fromLink
      log("Done.")
    }
  }

  def loadAll = {
    loadAlignments
    loadLinks
    log(translations.length + " (Simple) Translations and " + translationgroups.length + " Alignment groups present")
    /*
    log("Simple Translations:")
    logGroup{
      translations.foreach(t => log(t.toString))
    }
    log("Translation Groups:")
    logGroup{
      translationgroups.foreach(t => log(t.toString))
    }
    */
  }

  def add(t : Translation) = translations = t :: translations

  def add(t : TranslationGroup) = translationgroups = t :: translationgroups

}
