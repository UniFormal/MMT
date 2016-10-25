package info.kwarc.mmt.api.refactoring

import info.kwarc.mmt.api.archives.Archive
import info.kwarc.mmt.api.{GlobalName, LocalName}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.{DeclaredLink, DeclaredTheory}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.ontology.{ArgumentAlignment, FormalAlignment, SimpleAlignment}
import info.kwarc.mmt.api.symbols.{Constant, UniformTranslator}

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
        case c: Constant if c.rl contains "apply" =>
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
abstract class AcrossLibraryTranslation extends UniformTranslator {
  def applicable(tm : Term) : Boolean
  def apply(tm : Term) : Term
  def apply(context: Context, tm: Term): Term = apply(tm)
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

  object DefinitionExpander extends AcrossLibraryTranslation {
    def applicable (tm : Term) = tm match {
      case OMS(p) =>
        controller.get(p) match {
          case c : Constant if c.df.isDefined => true
          case _ => false
        }
      case _ => false
    }

    def apply(tm : Term) = tm match {
      case OMS(p) =>
        controller.get(p) match {
          case c : Constant if c.df.isDefined => c.df.get
        }
    }
  }

  private def innerTranslate(tc : TermClass)(implicit to : Archive) : Unit = tc.state match {
    case Finished =>
    case Failed =>
    case Changed(_) =>
    case _ =>
      val tr = findTranslations(tc.currentTerm).find(t => tc.applicable(t))
      if (tr.isDefined) tc.applyTranslation(tr.get)
      else {
        var changed = false
        val immsubs = tc.immediateSubterms
        immsubs foreach (it => if (!changed) {
          innerTranslate(it)
          if (it.state.isInstanceOf[Changed]) changed = true
        })
        if (!changed) tc.backtrack
      }
  }

  def translate(tm : Term, to : Archive) : (Term, Boolean) = {
    implicit val target = to
    val tc = TermClass(tm)
    try {
      while (tc.state != Finished) {
        innerTranslate(tc)
        tc.update
        if (tc.state == Failed) throw Fail
      }
      (tc.currentTerm,true)
    } catch {
      case Fail => (tc.revertPartially,false)
    }
  }

  private def findTranslations(tm : Term) : List[AcrossLibraryTranslation] = (DefinitionExpander :: translations).filter(_.applicable(tm))

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

  sealed class TermClassState
  case object Finished extends TermClassState
  case object Failed extends TermClassState
  case object Todo extends TermClassState

  sealed class Change
  case object New extends Change
  case object Backtracked extends Change
  case class Changed(ch : Change) extends TermClassState

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

    private var stateVar : TermClassState = Todo
    def state : TermClassState = {
      val newstate = stateVar match {
        case Finished => Finished
        case Failed => Failed
        case _ => currentTerm match {
          case OMS(p) if inArchive(p) => Finished
          case OMS(_) => stateVar
          case _ =>
            val states = immediateSubterms.map(_.state)
            if (states.forall(_ == Finished)) Finished
            else if (states contains Changed(New)) Changed(New)
            else if (states contains Changed(Backtracked)) Changed(Backtracked)
            else Todo
        }
      }
      stateVar = newstate
      newstate
    }

    state

    def backtrack = if (usedTranslations.nonEmpty && steps.length > 1) {
      steps = steps.tail
      stateVar = Changed(Backtracked)
    } else stateVar = Failed
    // prolly makes things faster
    var storedResult : (Term,AcrossLibraryTranslation,Term) = null

    def applicable(tr : AcrossLibraryTranslation) = !(stateVar == Finished) &&
      !usedTranslations.contains(tr) &&
      tr.applicable(currentTerm) && {
      val res = tr(currentTerm)
      if (!steps.contains(res)) {
        storedResult = (currentTerm,tr,res)
        true
      } else false
    }

    def applyTranslation(tr : AcrossLibraryTranslation) = {
      steps ::= (storedResult match {
        case (a, b, c) if a == currentTerm && b == tr => c
        case _ => tr(currentTerm)
      })
      usedTranslations ::= tr
      TermClass.register(steps.head, this)
      stateVar = Changed(New)
    }

    private def updateVar(vd : VarDecl) : VarDecl = VarDecl(vd.name,vd.tp.map(t => TermClass(t).update),vd.df.map(t => TermClass(t).update),vd.not)
    private def updateContext(con : Context) : Context = con.variables.map(updateVar).toList

    def update : Term = if (immediateSubterms.map(_.state) contains Changed(Backtracked) ) {
        backtrack
        immediateSubterms.map(_.update)
        stateVar = Todo
        currentTerm
      } else {
      val ret: Term = currentTerm match {
        case OMID(_) =>
          stateVar = Todo
          currentTerm
        case OMBINDC(binder, con, scopes) => OMBINDC(TermClass(binder).update, updateContext(con), scopes.map(TermClass.apply).map(_.update))
        case OMA(f, args) => OMA(TermClass(f).update, (args map TermClass.apply).map(_.update))
        case OMV(_) => {
          currentTerm
        }
        case OMATTR(arg, key, value) => OMATTR(TermClass(arg).update, TermClass(key).update.asInstanceOf[OMID], TermClass(value).update)
        // TODO -----------v-----------------------------------------------------------------^
        case OMLIT(vl, _) => ???
        case UnknownOMLIT(vl, st) => UnknownOMLIT(vl, TermClass(st).update)
        case OMFOREIGN(_) => currentTerm
        case OMSemiFormal(_) => currentTerm
        case OML(vd) => OML(updateVar(vd))
      }
      if (ret != currentTerm) {
        steps ::= ret
        TermClass.register(ret, this)
      }
      state
      ret
    }

    def revertPartially : Term = if (state == Finished) currentTerm else currentTerm match {
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
  // TODO: the whole strict/pragmatic-stuff
  // TODO --------------------------------------------------------------------------------------------------------------

}