package info.kwarc.mmt.api.refactoring

import info.kwarc.mmt.api.archives.Archive
import info.kwarc.mmt.api.{GlobalName, LocalName, MPath}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.{DeclaredLink, DeclaredTheory}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.ontology.{ArgumentAlignment, FormalAlignment, SimpleAlignment}
import info.kwarc.mmt.api.symbols.{FinalConstant, UniformTranslator}
import info.kwarc.mmt.api.web.{Body, Server, ServerExtension, Session}

import scala.collection.mutable
import scala.util.{Success, Try}


// TODO replace by pragmatify ------------------------------------------------------------------------------------------
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
  var controller : Controller = null
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

//TODO continue --------------------------------------------------------------------------------------------------------
abstract class AcrossLibraryTranslation /* extends UniformTranslator */ {
  def applicable(tm : Term) : Boolean
  def apply(tm : Term) : Term
}
case class AlignmentTranslation(alignment : FormalAlignment) extends AcrossLibraryTranslation {
  lazy val traverser = alignment match {
    case SimpleAlignment(from,to,_) => new StatelessTraverser {
      override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
        case OMID(from.mmturi) => OMID(to.mmturi)
        case _ => Traverser(this,t)
      }
    }
    case align @ ArgumentAlignment(from,to,_,arguments) => new StatelessTraverser {
      private def reorder(ts: List[Term]): List[Term] = {
        val max = align.arguments.maxBy(p => p._2)._2
        (1 to max).map(i => {
          val ni = align.arguments.find(p => p._2 == i).map(_._1)
          if (ni.isEmpty) OMV(LocalName("_")) // TODO implicit arguments
          else ts(ni.get)
        }).toList
      }

      // TODO use pragmatic instead
      override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
        case Application(OMS(from.mmturi), args, appls) => Traverser(this,Application(OMID(to.mmturi), reorder(args), appls))
        case OMID(from.mmturi) => OMID(to.mmturi)
        case _ => Traverser(this,t)
      }
    }
  }
  def apply(context: Context, tm: Term): Term = apply(tm)
  def apply(tm : Term) = traverser(tm,())

  // TODO use pragmatic instead
  def applicable(tm : Term) = (alignment,tm) match {
    case (_,OMID(alignment.from.mmturi)) => true
    case (ArgumentAlignment(_,_,_,_),Application(OMS(alignment.from.mmturi), args, appls)) => true
    case _ => false
  }
}
case class LinkTranslation(ln : DeclaredLink) extends AcrossLibraryTranslation {

  // TODO maybe replace -----------------------------------------------------------------
  def getSymbols(tm : Term) = {
    var symbols : List[GlobalName] = Nil
    val traverser = new StatelessTraverser {
      def traverse(t:Term)(implicit con : Context, init : State) = t match {
        case OMS(p) =>
          symbols ::= p
          t
        case _ => Traverser(this,t)
      }
    }
    traverser(tm,())
    symbols
  }
  // TODO --------------------------------------------------------------------------------

  val from = ln.from match {
    case OMMOD(mp) => mp
    case _ => ??? // TODO materialize properly
  }

  def applicable(tm : Term) = getSymbols(tm).exists(s => s.module == from)
  def apply(tm : Term) = ??? // TODO apply morphism
}

class AcrossLibraryTranslator(controller : Controller, translations : List[AcrossLibraryTranslation]) /* extends ServerExtension("translate") */ {
  /* override def logPrefix = "Translator"

  override def start(args: List[String]) = {

  }

  def apply(path: List[String], query: String, body: Body, session: Session) = {
    ???
  }
  */

  private def innerTranslate(tc : TermClass)(implicit to : Archive) : Unit = {
    if (tc.finished) return
    val tr = findTranslations(tc.currentTerm).find(t => tc.applicable(t))
    if (tr.isDefined) tc.applyTranslation(tr.get) else {
      var changed = false
      val immsubs = tc.immediateSubterms
      immsubs foreach (it => if (!changed) {
        innerTranslate(it)
        if (it.changed) changed = true
      })
      if (!changed) tc.backtrack
    }
  }

  def translate(tm : Term, to : Archive) : (Term, Boolean) = {
    implicit val target = to
    val tc = TermClass(tm)
    try {
      while (!tc.finished) {
        innerTranslate(tc)
        tc.update
      }
      (tc.currentTerm,true)
    } catch {
      case Fail => (tc.revertPartially,false)
    }
  }

  private def findTranslations(tm : Term) : List[AcrossLibraryTranslation] = translations.filter(_.applicable(tm))

  object TermClass {
    private val store : mutable.HashMap[Term,TermClass] = mutable.HashMap.empty
    def apply(tm : Term)(implicit to : Archive) = store.getOrElse(tm, {
      val ret = new TermClass(tm)
      store(tm) = ret
      ret
    })
    def register(tm : Term, tc : TermClass) = store(tm) = tc
  }

  object Fail extends Exception

  class TermClass(val original : Term)(implicit to : Archive) {
    override def toString = "TermClass: " + steps.head + "\nPredecessors: " + steps.tail.map(_.toString).mkString("\n")
    private var steps : List[Term] = List(original)
    def currentTerm = steps.head
    private var usedTranslations : List[AcrossLibraryTranslation] = Nil

    def immediateSubterms : List[TermClass] = (currentTerm match {
      case OMID(_) => Nil
      case OMBINDC(binder,con,scopes) => TermClass(binder) :: con.variables.flatMap(v =>
        v.tp.toList ::: v.df.toList
      ).map(TermClass.apply).toList ::: scopes.map(TermClass.apply)
      case OMA(f,args) => TermClass(f) :: (args map TermClass.apply)
      case OMV(_) => Nil
      case OMATTR(arg,key,value) => List(TermClass(arg),TermClass(key),TermClass(value))
      case tx : OMLITTrait => List(TermClass(tx.synType))
      case OMFOREIGN(_) => Nil
      case OMSemiFormal(_) => Nil
      case OML(VarDecl(_,tpOpt,dfOpt,_)) => (tpOpt.toList ::: dfOpt.toList).map(TermClass.apply)
    }).distinct

    // private def subtermsNondistinct : List[TermClass] = this :: immediateSubterms.flatMap(_.subtermsNondistinct)

    // def subterms : List[TermClass] = subtermsNondistinct.distinct

    private var finishedVar : Boolean = false

    def finished : Boolean = finishedVar || {
      finishedVar = currentTerm match {
        case OMS(p) => inArchive(p)
        case _ => immediateSubterms.forall(_.finished)
      }
      finishedVar
    }

    finishedVar = finished

    def backtrack = if (steps.length > 1) {
      steps = steps.tail
      changedVar = true
    } else throw Fail

    def applicable(tr : AcrossLibraryTranslation) = !finishedVar &&
      !usedTranslations.contains(tr) &&
      tr.applicable(currentTerm) && {
      val res = tr.apply(currentTerm)
      !steps.contains(res)
    }

    private var changedVar = false

    def applyTranslation(tr : AcrossLibraryTranslation) = {
      usedTranslations ::= tr
      steps ::= tr(currentTerm)
      TermClass.register(steps.head,this)
      changedVar = true
    }

    def changed : Boolean = changedVar || immediateSubterms.exists(_.changed)

    private def updateVar(vd : VarDecl) : VarDecl = VarDecl(vd.name,vd.tp.map(t => TermClass(t).update),vd.df.map(t => TermClass(t).update),vd.not)
    private def updateContext(con : Context) : Context = con.variables.map(updateVar).toList

    def update : Term = if (changed) {
      changedVar = false
      val ret : Term = currentTerm match {
        case OMID(_) => currentTerm
        case OMBINDC(binder,con,scopes) => OMBINDC(TermClass(binder).update,updateContext(con),scopes.map(TermClass.apply).map(_.update))
        case OMA(f,args) => OMA(TermClass(f).update,(args map TermClass.apply).map(_.update))
        case OMV(_) => currentTerm
        case OMATTR(arg,key,value) => OMATTR(TermClass(arg).update,TermClass(key).update.asInstanceOf[OMID],TermClass(value).update)
          // TODO -----------v-----------------------------------------------------------------^
        case OMLIT(vl,_) => ???
        case UnknownOMLIT(vl,st) => UnknownOMLIT(vl,TermClass(st).update)
        case OMFOREIGN(_) => currentTerm
        case OMSemiFormal(_) => currentTerm
        case OML(vd) => OML(updateVar(vd))
      }
      if (ret != currentTerm) {
        steps ::= ret
        TermClass.register(ret,this)
      }
      ret
    } else currentTerm

    def revertPartially : Term = if (finished) currentTerm else currentTerm match {
      case OMS(p) =>
        steps = List(original)
        original
      case _ =>
        immediateSubterms.map(_.revertPartially)
        update
    }
  }

  // TODO (potentially) replace by better stuff ------------------------------------------------------------------------

  def inArchive(path : GlobalName)(implicit target : Archive) : Boolean = (controller.backend.findOwningArchive(path.module) contains target) || {
    val fndPath = target.foundation.getOrElse(return false)
    val fnd = try {
      val ret = controller.get(fndPath).asInstanceOf[DeclaredTheory]
      controller.simplifier(ret)
      ret
    } catch {
      case e : Exception => return false
    }
    val ths = fnd :: fnd.getIncludes.map(p => Try(controller.get(p))).collect{
      case Success(th : DeclaredTheory) =>
        controller.simplifier(th)
        th
    }
    ths.flatMap(_.getDeclarations.map(_.path)) contains path
  }
  // foundation stuff
  // the whole strict/pragmatic-stuff
  // TODO --------------------------------------------------------------------------------------------------------------

}

/*



class Translator extends ServerExtension("translate") {

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

  def apply(path: List[String], query: String, body: Body, session: Session) = path match {
    case "libs" :: rest =>
      Server.TextResponse(archives.getArchives.map(_.name).mkString(" "))
    case "to" :: arch :: rest =>
      val tm : Term = ???
      val ret = apply(tm,arch,simplify = true, getpartialresults = false).map(_.toNode)
      ??? // Server.XmlResponse(ret)
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
    var expdef = false

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
    val ret = state + (trav.apply(state.term,()),traversed,trls)
    if (trls.exists(_.isInstanceOf[ExpandDefinition])) ret.expdef = true
    ret
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
    else if (getpartialresults) {
      val ls = states.collect {
        case st if st.status.isInstanceOf[State.blocked] => st
      }
      List((if (ls.exists(!_.expdef)) ls.filter(!_.expdef) else ls).maxBy(_.status.asInstanceOf[State.blocked].value).term)
    }
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
*/