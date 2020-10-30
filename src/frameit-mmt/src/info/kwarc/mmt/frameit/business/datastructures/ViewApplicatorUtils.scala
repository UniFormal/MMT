package info.kwarc.mmt.frameit.business.datastructures

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.libraries.Lookup
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.{Constant, OMSReplacer}
import info.kwarc.mmt.api.uom.SimplificationUnit
import info.kwarc.mmt.api.{??, GeneralError, GlobalName, LocalName}
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld.MetaAnnotations
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld.MetaAnnotations.MetaKeys
import info.kwarc.mmt.frameit.archives.MitM
import info.kwarc.mmt.frameit.archives.MitM.Foundation.StringLiterals
import info.kwarc.mmt.frameit.business.Utils

import scala.util.Try

sealed trait ScrollViewRenderer {
  def apply(term: Term): Term
  def hasAssignmentFor(symbol: GlobalName): Boolean
}

class StaticRenderer(private val scroll: ElaboratedScrollReference)(implicit ctrl: Controller) extends ScrollViewRenderer {
  private val simplifier = new FullSimplifier()
  override def apply(term: Term): Term = simplifier(term, Context(scroll.problemTheory))

  override def hasAssignmentFor(symbol: GlobalName): Boolean = false
}

// @todo make it simplifying, too!
class ScrollApplicationRenderer(private val scrollApp: ScrollApplication)(implicit ctrl: Controller) extends ScrollViewRenderer {
  private val viewApplicator = new OMSReplacer {
    override def replace(p: GlobalName): Option[Term] = scrollApp.assignments.get(p)
  }
  private val simplifier = new FullSimplifier()

  // todo: brittle, really we want [[VerbalizationSimplifier]] to be a [[Rule]]
  override def apply(term: Term): Term = {
    simplifier(
      viewApplicator(term, Context(scrollApp.ref.problemTheory)),
      Context(scrollApp.situationTheory)
    )
  }

  override def hasAssignmentFor(symbol: GlobalName): Boolean = scrollApp.assignments.contains(symbol)
}

// todo: to be replaced when [[VerbalizationSimplifier]] becomes a [[SimplificationRule]]
private class FullSimplifier(implicit ctrl: Controller) {
  private val verbSimplifier = new VerbalizationSimplifier()(ctrl.globalLookup)

  def apply(t: Term, context: Context): Term = {
    step(step(t, context), context) // to stabilize the verbSimplifier's output... brittle, we should really use MMT's Simplifier
  }

  private def step(t: Term, context: Context): Term = {
    val simplificationUnit = SimplificationUnit(context, expandDefinitions = false, fullRecursion = true)
    val s = verbSimplifier(t, context)
    try {
      ctrl.simplifier(s, simplificationUnit)
    } catch {
      case _: GeneralError =>
        System.err.println("error while simplifying, possibly known MMT bug (UniFormal/MMT#546)")
        s
    }
  }
}

class VerbalizationSimplifier(implicit lookup: Lookup) extends StatelessTraverser {
  def traverse(t: Term)(implicit con : Context, state : State): Term = t match {
    case MetaAnnotations.LabelVerbalization(args) =>
      val verbalizedArgs = args.map {
        case OMID(path) =>
          Utils
            .getAsO(classOf[Constant], path)
            .flatMap(c => Try(
              MetadataUtils.readTermMetaDatum(c.metadata, MetaKeys.label)
            ).toOption)
            .getOrElse(StringLiterals(
              s"could not label-verbalize symbol reference to `${path}`")
            )

        case term =>
          StringLiterals(s"cannot verbalize complex terms like `${term.toStr(true)}` yet")
      }

      verbalizedArgs.reduceLeft(MitM.Foundation.StringConcat.apply)

    case t => Traverser(this,t)
  }
}
