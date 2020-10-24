package info.kwarc.mmt.frameit.business.datastructures

import info.kwarc.mmt.api.{??, GeneralError, GetError, GlobalName, LocalName}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.libraries.Lookup
import info.kwarc.mmt.api.modules.View
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.{ApplyMorphism, Constant, UniformTranslator}
import info.kwarc.mmt.api.uom.SimplificationUnit
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

// @todo make it simplifying, too!
class StandardViewRenderer(private val view: View)(implicit ctrl: Controller) extends ScrollViewRenderer {
  private val viewApplicator = new ApplyPartialMorphism(ctrl.globalLookup, view.toTerm)

  // for partial views, no explicit separation actually possible, hence the names might be misleading
  private val domainCtx = Context(view.from.toMPath)
  private val codomainCtx = Context(view.to.toMPath)

  private val verbSimplifier = new VerbalizationSimplifier()(ctrl.globalLookup)

  // for MMT's simplifier
  private val simplicationUnit = SimplificationUnit(codomainCtx, expandDefinitions = false, fullRecursion = true)

  // todo: brittle, really we want [[VerbalizationSimplifier]] to be a [[Rule]]
  override def apply(term: Term): Term = step(step(step(term)))

  private def step(term: Term): Term = {
    val verbSimplified = verbSimplifier(viewApplicator(domainCtx, term), codomainCtx)

    try {
      ctrl.simplifier.apply(
        verbSimplified,
        simplicationUnit
      )
    } catch {
      case _: GeneralError =>
        System.err.println("error while simplifying, possibly known MMT bug (UniFormal/MMT#546)")
        verbSimplified
    }
  }

  override def hasAssignmentFor(symbol: GlobalName): Boolean = symbol match {
    case theo ?? ln =>
      view.getO(LocalName(theo) / ln).nonEmpty

    case _ => false
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

case class ApplyPartialMorphism(lup: Lookup, morph: Term) extends UniformTranslator {
  private val traverser = new ApplyPartialMorphs()(lup)
  def apply(context: Context, tm: Term): Term = traverser.traverse(tm)(context, morph)
}

private class ApplyPartialMorphs(implicit lookup: Lookup) extends Traverser[Term] {
  def traverse(t: Term)(implicit con: Context, morph: Term) : Term = {
    t match {
      case OMM(arg, via) =>
        traverse(arg)(con, OMCOMP(via, morph))
      case OMS(theo ?? ln) =>
        // look for an assignment in morph for that symbol
        Utils.getAsO(classOf[Constant], morph.toMPath ? (LocalName(theo) / ln)).flatMap(_.df) match {
          case Some(df) =>
            // case: morph contains an assignment => translate to assignment's RHS
            df
          case None =>
            // case: morph does not contain an assignment for the symbol
            //
            // => maybe the symbol was defined in the domain theory
            //    (and we can compute the homomorphic extension of morph on it)
            Utils.getAsO(classOf[Constant], theo ? ln).flatMap(_.df) match {
              case Some(df) =>
                traverse(df)

              case None =>
                // case: morph is partial on the symbol
                // just return it
                t
            }
        }

      case t => Traverser(this,t)
    }
  }
}