package info.kwarc.mmt.frameit.business.datastructures

import info.kwarc.mmt.api.GeneralError
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.libraries.Lookup
import info.kwarc.mmt.api.modules.View
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.{ApplyMorphism, Constant}
import info.kwarc.mmt.api.uom.SimplificationUnit
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld.{MetaFunctions, MetaKeys}
import info.kwarc.mmt.frameit.archives.MitM.Foundation.StringLiterals
import info.kwarc.mmt.frameit.business.Utils
import info.kwarc.mmt.lf.ApplySpine

import scala.util.Try

sealed trait ScrollViewRenderer {
  def apply(term: Term): Term
}

// @todo make it simplifying, too!
class StandardViewRenderer(private val view: View)(implicit ctrl: Controller) extends ScrollViewRenderer {
  private val viewApplicator = ApplyMorphism(ctrl.globalLookup, view.toTerm)

  // for partial views, no explicit separation actually possible, hence the names might be misleading
  private val domainCtx = Context(view.from.toMPath)
  private val codomainCtx = Context(view.to.toMPath)

  private val verbSimplifier = new VerbalizationSimplifier()(ctrl.globalLookup)

  // for MMT's simplifier
  private val simplicationUnit = SimplificationUnit(codomainCtx, expandDefinitions = false, fullRecursion = true)

  override def apply(term: Term): Term = {
    val verbSimplified = verbSimplifier(viewApplicator(domainCtx, term), codomainCtx)

    try {
      ctrl.simplifier.apply(
        verbSimplified,
        simplicationUnit
      )
    } catch {
      case e: GeneralError =>
        System.err.println("error while simplifying, possibly known MMT bug (UniFormal/MMT#546)")
        verbSimplified
    }
  }
}

class VerbalizationSimplifier(implicit lookup: Lookup) extends StatelessTraverser {
  def traverse(t: Term)(implicit con : Context, state : State): Term = t match {
    case ApplySpine(OMID(MetaFunctions.labelVerbOf), List(term, defaultVerbalization)) =>

      (term match {
        case OMID(path) =>
          Utils
            .getAsO(classOf[Constant], path)
            .flatMap(c => Try(MetadataUtils.readStringMetaDatum(c.metadata, MetaKeys.label)).toOption)
            .map(StringLiterals.apply)

        case _ =>
          Some(StringLiterals(s"cannot verbalize complex terms like `${term.toStr(true)}` yet"))
      }).getOrElse(defaultVerbalization)

    case t => Traverser(this,t)
  }
}