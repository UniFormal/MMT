package info.kwarc.mmt.api.refactoring

import info.kwarc.mmt.api.archives.Archive
import info.kwarc.mmt.api.{GlobalName, ImplementationError, LocalName}
import info.kwarc.mmt.api.frontend.{Controller, Logger}
import info.kwarc.mmt.api.modules.{Link, Theory}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.ontology.QueryEvaluator.QuerySubstitution
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.parser.ParseResult
import info.kwarc.mmt.api.symbols.{Constant, Structure}
import info.kwarc.mmt.api.uom.RealizedType

import scala.collection.mutable
import scala.util.{Success, Try}


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


abstract class AcrossLibraryTranslation extends UniformTranslator {
  def applicable(tm : Term)(implicit translator: AcrossLibraryTranslator) : Boolean
  def apply(tm : Term)(implicit translator: AcrossLibraryTranslator) : Term
  def applyPlain(context: Context, tm: Term) : Term = tm
}

object AlignmentTranslation {
  def apply(alignment : FormalAlignment, controller : Controller) = {
    val (from,to) = (Try(controller.get(alignment.from.mmturi)),Try(controller.get(alignment.to.mmturi)))
    /* if (from.toOption.isDefined && to.toOption.isDefined) */ new AlignmentTranslation(alignment)
    /* else None */
  }
  object None extends AlignmentTranslation(SimpleAlignment(ParseResult.free,ParseResult.free,false)) {
    override def applicable(tm: Term)(implicit translator: AcrossLibraryTranslator): Boolean = false
  }
}

case class AlignmentTranslation(val alignment : FormalAlignment) extends AcrossLibraryTranslation {
  override def toString: String = "Alignment " + alignment.toString

  /*
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
  */

  def apply(tm : Term)(implicit translator: AcrossLibraryTranslator) = {//traverser(tm,())
    (alignment,tm) match {
      case (_,OMID(alignment.from.mmturi)) => OMID(alignment.to.mmturi)
      case (align: ArgumentAlignment, translator.Application(ls,alignment.from.mmturi,args)) =>
        // println("Pragma! " + ls + " from " + align.from.mmturi)
        def reorder(ts: List[Term]): List[Term] = {
          val max = align.arguments.maxBy(p => p._2)._2
          (1 to max).map(i => {
            val ni = align.arguments.find(p => p._2 == i).map(_._1)
            if (ni.isEmpty) OMV(LocalName("_")) // TODO implicit arguments
            else ts(ni.get - 1)
          }).toList
        }
        translator.Application(ls,align.to.mmturi match {
          case gn : GlobalName => gn
          case a => throw ImplementationError("expected a GlobalName")
        },reorder(args))
      case (align: DereferenceAlignment, translator.Application(_,_,hd::tl)) =>
        OMA(align.dotOperator(hd, OML(align.to.mmturi.name)), tl)
      case (align: AlignmentConcatenation, tm) =>
        val tm1 = AlignmentTranslation(align.first).apply(tm)
        val at2 = AlignmentTranslation(align.second)
        if (at2.applicable(tm1))
          AlignmentTranslation(align.second).apply(tm1)
        else {
          tm1
        }
    }
  }

  def applicable(tm : Term)(implicit translator: AcrossLibraryTranslator) = (alignment,tm) match {
    case (_,OMID(alignment.from.mmturi)) => true
    case (_:ArgumentAlignment,    translator.Application(_,alignment.from.mmturi, args)) if Try(translator.ctrl.get(alignment.to.mmturi)).isSuccess => true
    case (_:DereferenceAlignment, translator.Application(_,alignment.from.mmturi, _::_)) => true
    case (ac: AlignmentConcatenation, _) => AlignmentTranslation(ac.first).applicable(tm) // TODO check ac.second 
    case _ => false
  }
}

abstract class TranslationGroup {
  def applicable(tm : Term)(implicit trl : AcrossLibraryTranslator) : List[(AcrossLibraryTranslation,Term)]
}

case class LinkTranslation(ln : Link) extends TranslationGroup {
  override def toString: String = "link " + ln.path
  val from = ln.from match {
    case OMMOD(mp) => mp
    case _ => ??? // TODO materialize properly
  }
  case class InnerTranslation(p : GlobalName) extends AcrossLibraryTranslation {
    def applicable(tm : Term)(implicit translator: AcrossLibraryTranslator) = true
    def apply(tm : Term)(implicit translator: AcrossLibraryTranslator) = {
      require(tm == OMS(p))
      ln match {
        case s : Structure => OMS(s.parent ? (ln.name / p.name))
          // TODO views
      }
    }
  }

  def applicable(tm : Term)(implicit translator: AcrossLibraryTranslator) =
    AcrossLibraryTranslator.getSymbols(tm).filter(s => s.module == from).map(p => (InnerTranslation(p),OMS(p)))
}

object AcrossLibraryTranslator {
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

  def trivial(ctrl : Controller) = new AcrossLibraryTranslator(ctrl,Nil,Nil,ArchiveTarget(ctrl.backend.getArchives.head))
}

trait TranslationTarget {
  def inTarget(path: GlobalName, controller : Controller): Boolean
}

case class ArchiveTarget(archive : Archive) extends TranslationTarget {
  def inTarget(path: GlobalName, controller : Controller): Boolean = (controller.backend.findOwningArchive(path.module) contains archive) || {
    val fndPath = archive.foundation.getOrElse(return false)
    val fnd = try {
      val ret = controller.getAs(classOf[Theory], fndPath)
      controller.simplifier(ret)
      ret
    } catch {
      case e: Exception => return false
    }
    val ths = fnd :: fnd.getIncludes.map(p => Try(controller.get(p))).collect {
      case Success(th: Theory) =>
        controller.simplifier(th)
        th
    }
    ths.flatMap(_.getDeclarations.map(_.path)) contains path
  }
}

class AcrossLibraryTranslator(controller : Controller,
                              translations : List[AcrossLibraryTranslation],
                              groups: List[TranslationGroup],
                              target : TranslationTarget,expandDefs : Boolean = true) extends Logger/* extends ServerExtension("translate") */ {
  val report = controller.report
  override def logPrefix = "translator"
  val ctrl : Controller = controller

  // TODO maybe replace -----------------------------------------------------------
  object Application {
    def apply(ls: List[GlobalName], f: GlobalName, args: List[Term]) = OMApplication(OMS(f), args, ls)

    def unapply(t: Term): Option[(List[GlobalName],GlobalName, List[Term])] = smartunapply(t, OMApplication)

    private def smartunapply(t: Term, app: Application): Option[(List[GlobalName],GlobalName, List[Term])] = t match {
      case app(OMS(f), args) =>
        controller.get(f) match {
          case c: Constant if c.rl contains "apply" =>
            val napp = SymbolApplication(f, app)
            smartunapply(t, napp).map(r => (f :: r._1, r._2, r._3))
          case _ =>
            Some((Nil,f, args))
        }
      case app(OMS(f), args) =>
        Some((Nil, f, args))
      case _ => None
    }
  }

  // TODO -------------------------------------------------------------------------


  private implicit val trl = this

  object DefinitionExpander extends AcrossLibraryTranslation {
    def applicable(tm: Term)(implicit translator: AcrossLibraryTranslator) = tm match {
      case OMS(p) =>
        controller.getO(p) match {
          case Some(c: Constant) if c.df.isDefined => true
          case _ => false
        }
      case _ => false
    }

    def apply(tm: Term)(implicit translator: AcrossLibraryTranslator) = tm match {
      case OMS(p) =>
        controller.get(p) match {
          case c: Constant if c.df.isDefined => c.df.get
        }
    }
  }

  private var openGroups: List[TranslationGroup] = Nil
  private var backtrackstack: List[(AcrossLibraryTranslation, TermClass)] = Nil

  private def innerTranslate(tc: TermClass): Boolean = tc.state match {
    case Finished =>
      log("Finished: " + (tc.currentTerm match {
        case OMS(p) => p
        case t => controller.presenter.asString(t)
      }))
      false
    case Failed =>
      log("Failed: " + (tc.currentTerm match {
        case OMS(p) => p
        case t => controller.presenter.asString(t)
      }))
      false
    // case Changed(_) =>
    case _ =>
      log("Trying: " + (tc.currentTerm match {
        case OMS(p) => p
        case t => controller.presenter.asString(t)
      }))
      val tr = findTranslations(tc.currentTerm).find(t => tc.applicable(t))
      if (tr.isDefined) {
        tc.applyTranslation(tr.get)
        true
      }
      else {
        var changed = false
        val immsubs = tc.immediateSubterms
        immsubs foreach (it => /* if (!changed) */ {
          val prest = it.state
          val ret = innerTranslate(it)
          changed = changed || ret
        })
        if (!changed) {
          log("No change: " + (tc.currentTerm match {
            case OMS(p) => p
            case t => controller.presenter.asString(t)
          }))
          tc.backtrack
        } else true
      }
  }

  private def applyGroup(ls: List[TranslationGroup], tc: TermClass): Boolean = {
    var rest: List[(AcrossLibraryTranslation, Term)] = Nil
    val gr = ls.find(g => {
      rest = g.applicable(tc.currentTerm)
      rest.nonEmpty
    })
    if (gr.isDefined) {
      // println("Applying group " + gr.get)
      if (!openGroups.contains(gr.get)) openGroups ::= gr.get
      val ret = rest.find(p =>
        if (TermClass(p._2).applicable(p._1)) {
          TermClass(p._2).applyTranslation(p._1)
          true
        } else false
      )
      ret.isDefined
    } else false
  }

  private def translateGroups(tc: TermClass): Boolean = {
    if (openGroups.nonEmpty) {
      applyGroup(openGroups, tc)
    } else {
      // open up new
      applyGroup(groups, tc)
      //false
    }
  }

  var origs: List[Term] = Nil

  def translate(tm: Term): (Term,List[GlobalName]) = {
    log("Translating " + controller.presenter.asString(tm))
    val tc = TermClass(tm)
    def st(tm: TermClass): List[Term] = tm.currentTerm :: tm.immediateSubterms.flatMap(st)
    origs = st(tc)
    try {
      while (tc.state != Finished) {
        if (!translateGroups(tc)) {
          innerTranslate(tc)
        }
        tc.update
        if (tc.state == Failed) throw Fail
      }
      tc.update
      log("-------------- DONE (Success) ---------------")
      log(tc.currentTerm.toString)
      (tc.currentTerm,Nil)
    } catch {
      case Fail =>
        val t = tc.revertPartially
        //TermClass.getAll.foreach(println)
        val symbols = AcrossLibraryTranslator.getSymbols(t).filterNot(target.inTarget(_,controller)).distinct
        val missings = symbols /*.collect {
          case s if translate(OMS(s))._2.nonEmpty => s
        } */
        log("-------------- DONE (Failed) ---------------")
        log(tc.currentTerm.toString)
        log("Missing: " + missings.mkString(", "))
        (t,missings)
    }
  }

  private def findTranslations(tm: Term): List[AcrossLibraryTranslation] = 
    if (expandDefs) (DefinitionExpander :: translations).filter(_.applicable(tm))
    else translations.filter(_.applicable(tm))

  object TermClass {
    private val store: mutable.HashMap[Term, TermClass] = mutable.HashMap.empty

    def apply(tm: Term) = store.getOrElse(tm, {
      val ret = new TermClass(tm)
      store(tm) = ret
      ret
    })

    def register(tm: Term, tc: TermClass) = store(tm) = tc

    def getAll = store.values.toList.distinct
  }

  object Fail extends Exception

  sealed class TermClassState

  case object Finished extends TermClassState

  case object Failed extends TermClassState

  case object Todo extends TermClassState

  sealed class Change

  case object New extends Change

  case object Backtracked extends Change

  case class Changed(ch: Change) extends TermClassState

  class TermClass(val original: Term) {
    private def termtostr(tm: Term) = tm match {
      case OMS(p) => p.module.name.toString + "?" + p.name
      case _ => controller.presenter.objectLevel.asString(tm)
    }

    override def toString =
      "---------------\nTermClass: " + termtostr(original) + "\n" +
        "Steps:\n" + steps.init.map(t => termtostr(t._2)).mkString("\n") + "\n" +
        "---------------"

    //+ steps.head + "\nPredecessors: " + steps.tail.map(_.toString).mkString("\n")
    private var steps: List[(Option[AcrossLibraryTranslation], Term)] = List((None, original))

    def currentTerm = steps.head._2

    private var usedTranslations: List[AcrossLibraryTranslation] = Nil

    def immediateSubterms: List[TermClass] = (currentTerm match {
      case OMID(_) => Nil
      case OMBINDC(binder, con, scopes) => TermClass(binder) :: con.variables.flatMap(v =>
        v.tp.toList ::: v.df.toList
      ).map(TermClass.apply).toList ::: scopes.map(TermClass.apply)
      case OMA(f, args) => TermClass(f) :: (args map TermClass.apply)
      case OMV(_) => Nil
      case OMATTR(arg, key, value) => List(TermClass(arg), TermClass(key), TermClass(value))
      case tx: OMLITTrait => List(TermClass(tx.synType))
      case OMFOREIGN(_) => Nil
      case OMSemiFormal(_) => Nil
      case OML(_, tpOpt, dfOpt,_,_) => (tpOpt.toList ::: dfOpt.toList).map(TermClass.apply)
    }).distinct

    // private def subtermsNondistinct : List[TermClass] = this :: immediateSubterms.flatMap(_.subtermsNondistinct)

    // def subterms : List[TermClass] = subtermsNondistinct.distinct

    private var stateVar: TermClassState = Todo

    def state: TermClassState = {
      val newstate = stateVar match {
        case Finished => Finished
        case Failed => Failed
        case _ => currentTerm match {
          case OMS(p) if target.inTarget(p,controller) =>
            backtrackstack = backtrackstack.filterNot(p => p._2 == this && steps.exists(q => q._1 contains p._1))
            Finished
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

    def backtrack : Boolean = {
      log("Backtracking " + (currentTerm match {
        case OMS(p) => p
        case t => controller.presenter.asString(t)
      }))
      if ( /* usedTranslations.nonEmpty */ steps.head._1.isDefined && steps.length > 1) {
        usedTranslations ::= steps.head._1.get
        steps = steps.tail
        stateVar = Changed(Backtracked)
        log("New term: " + (currentTerm match {
          case OMS(p) => p
          case t => controller.presenter.asString(t)
        }))
        true
      } else {
        log("Not possible")
        stateVar = Failed
        false
      }
    }

    // prolly makes things faster
    var storedResult: (Term, AcrossLibraryTranslation, Term) = null

    def applicable(tr: AcrossLibraryTranslation) = !(stateVar == Finished) &&
      !usedTranslations.contains(tr) &&
      tr.applicable(currentTerm) && {
      val res = tr(currentTerm)
      if (!steps.map(_._2).contains(res)) {
        storedResult = (currentTerm, tr, res)
        true
      } else false
    }

    def applyTranslation(tr: AcrossLibraryTranslation) = {
      steps ::= (storedResult match {
        case (a, b, c) if a == currentTerm && b == tr => (Some(tr), c)
        case _ => (Some(tr), tr(currentTerm))
      })
      backtrackstack ::= ((steps.head._1.get, this))
      log("Applying " + tr.toString + "to: " + termtostr(steps(1)._2))
      // println(this)
      //usedTranslations ::= tr
      TermClass.register(currentTerm, this)
      stateVar = Changed(New)
    }

    private def updateVar(vd: VarDecl) = vd.map(t => TermClass(t).update)

    private def updateContext(con: Context): Context = con.variables.map(updateVar).toList

    def update: Term = {
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
        case OMLIT(v,rt) =>
          OMLIT(v,RealizedType(TermClass(rt.synType).update,rt.semType))
        case UnknownOMLIT(vl, st) => UnknownOMLIT(vl, TermClass(st).update)
        case OMFOREIGN(_) => currentTerm
        case OMSemiFormal(_) => currentTerm
        case o:OML => updateVar(o.vd).toOML
      }
      if (ret != currentTerm) {
        steps ::= (None, ret)
        TermClass.register(ret, this)
      }
      state
      ret
    }

    def revertPartially: Term = if (state == Finished) currentTerm
    else original match {
      case OMS(p) =>
        log("Reverting: " + p + " -> " + termtostr(original))
        steps = List((None, original))
        original
      case _ =>
        /*
        val reason = immediateSubterms.find(t => !origs.contains(t.original) &&
          t.state != Finished &&
          steps.exists(s => s._1.isDefined)
          )
        if (reason.isDefined) {
          println("Reverting: " + currentTerm + " -> " + original)
          println("Reason: " + reason.get.original)
          steps = List((None, original))
          original
        } else { */
          immediateSubterms.map(_.revertPartially)
          update
          // }
    }

  }
}



import QueryResultConversion._
import QueryTypeConversion._

class Translate extends QueryFunctionExtension("translate", StringType, ObjType) {
  def evaluate(argument: BaseType, params: List[String]) = {
    ???
  }
}
