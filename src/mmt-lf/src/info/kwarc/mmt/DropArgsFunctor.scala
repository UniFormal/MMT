package info.kwarc.mmt

import info.kwarc.mmt.ArgumentDropper.{ArgPath, DropInfo}
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.libraries.Library
import info.kwarc.mmt.api.metadata.{MetaData, MetaDatum}
import info.kwarc.mmt.api.modules.diagrams._
import info.kwarc.mmt.api.modules.diagrams.oldstuff.{InwardsLinearConnector, LinearFunctor, LinearOperator}
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.{Constant, Declaration, TermContainer}
import info.kwarc.mmt.lf._

import scala.collection.mutable

trait LinearDropArgsHeuristic {
  def apply(c: Constant, cleanedTp: Option[Term], cleanedDf: Option[Term], dropped: DropInfo)(implicit library: Library): Set[ArgumentDropper.ArgPath]
}

class DropArgsFunctor(meta: Diagram, heuristic: LinearDropArgsHeuristic) extends LinearFunctor {
  override val dom: Diagram = meta
  override val cod: Diagram = meta
  override def applyDomainModule(m: MPath): MPath = m

  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_cleaned")

  private[mmt] val dropped: ArgumentDropper.DropInfo = mutable.Map()

  override def translateConstant(c: Constant)(implicit interp: DiagramInterpreter): List[Declaration] = {
    implicit val library: Library = interp.ctrl.library

    val List(cleanedTp, cleanedDf) = List(c.tp, c.df).map(_.map(ArgumentDropper.clean(_, dropped)))
    val argsToDrop = c.name match {
      // for assignment in links we take the heuristic's value at the constant being assigned
      // TODO(NR@FR): does this also hold for assignments after a realize statement?
      case ComplexStep(dom) / name => dropped(dom ? name)
      case _ => heuristic(c, cleanedTp, cleanedDf, dropped)
    }
    val List(droppedTp, droppedDf) = List(cleanedTp, cleanedDf).map(
      _.flatMap(ArgumentDropper.drop(_, argsToDrop, interp.errorCont))
    )

    val (newTp, newDf) = if ((c.tp.nonEmpty && droppedTp.isEmpty) || (c.df.nonEmpty && droppedDf.isEmpty)) {
      interp.errorCont(InvalidElement(c, "Type or definiens component contained non-removable argument " +
        s"positions. The argument position that the heuristic deemed to remove were `$argsToDrop`." +
        s"Arguments of referenced constants have been dropped, but arguments of the constant itself have been" +
        s"left intact."))

      dropped(c.path) = Set.empty
      (cleanedTp, cleanedDf)
    } else {
      dropped(c.path) = argsToDrop
      (droppedTp, droppedDf)
    }

    List(Constant(
      home = OMMOD(equiNamer(c.path).module),
      name = equiNamer(c.name),
      alias = c.alias,
      tpC = TermContainer.asParsed(newTp),
      dfC = TermContainer.asParsed(newDf),
      rl = c.rl,
      notC = NotationContainer.empty() // TODO: copy and just notation depending on argsToDrop
    ))
  }
}

/**
  * Creates morphisms `T -> out(meta, heuristic)(T)` that map constants `c` (of N-ary function type)
  * to `\arg1. ... \argN. c arg_{i1} ... arg_{iM}` where (i1, ..., iM) is the sublist of (1, ..., N)
  * specifying the indicies of the kept (i.e., not dropped) arguments positions.
  *
  * Actually, the assignments are computed more intelligent than sketched above: they are the shortest
  * eta-reduced form possible (and, e.g., maps `c` to `λ_. c` instead of `λ_. λy. c y`).
  */
class DropArgsConnector(override val out: DropArgsFunctor) extends InwardsLinearConnector {
  override def applyDomainModule(thy: MPath): MPath = thy
  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_clean")

  override def translateConstant(c: Constant)(implicit interp: DiagramInterpreter): List[Declaration] = {
    val droppedArgs = out.dropped(c.path)
    val outc = out.applyModulePath(c.path.module) ? c.name

    // Goal: synthesize an assignment `c := λ bindingCtx. c applicationCtx`
    //
    // where bindingCtx specifies all variables to be bound and applicationCtx
    // is the subset of bindingCtx to which the argument-dropped c is applied
    // ("the remaining arguments that were not dropped").
    //
    // Another goal: the assignment shall be as short as possible, i.e.,
    // we must not output things like `c := λx. c x` or `c := λ_. λy. c y`.
    // Instead, we should output `c := c` and `c := λ_. c`.

    val bindingCtx: Context = if (droppedArgs.isEmpty) Nil else {
      (ArgumentDropper.getArguments(c).getOrElse(Nil) : Context).take(droppedArgs.max) // arguments are one-based!
    }

    val applicationCtx: Context = bindingCtx.zipWithIndex.collect {
      case (vd, idx) if !droppedArgs.contains(idx + 1) => vd // arguments are one-based!
    }

    List(assgn(c.path, Lambda.applyOrBody(
      bindingCtx,
      ApplyGeneral(OMS(outc), applicationCtx.map(_.toTerm))
    )))
  }
}

object DefaultDropArgsOperator extends ParametricLinearOperator {
  override val head: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?drop_args_default")

  private val heuristic = new KeepAwareHeuristic(DropNoLongerUsedArgument)

  override def instantiate(parameters: List[Term])(implicit interp: DiagramInterpreter): Option[LinearOperator] = parameters match {
    case List(DiagramTermBridge(meta)) =>
      val functor = new DropArgsFunctor(meta, heuristic)
      val connector = new DropArgsConnector(functor)

      Some((functor :: connector).withFocus(0))
    case _ => None
  }
}

/**
  * Drops the first argument of all function constants.
  */
object DropFirstArgument extends LinearDropArgsHeuristic {
  override def apply(c: Constant, cleanedTp: Option[Term], cleanedDf: Option[Term], dropped: DropInfo)(implicit library: Library): Set[ArgPath] = {
    cleanedTp match {
      case Some(FunType(in, _)) if in.nonEmpty => Set(1)
      case _ => Set.empty
    }
  }
}

object DropNoLongerUsedArgument extends LinearDropArgsHeuristic {
  override def apply(c: Constant, cleanedTp: Option[Term], cleanedDf: Option[Term], dropped: DropInfo)(implicit library: Library): Set[ArgPath] = {
    c.getOrigin match {
      case GeneratedFrom(source: GlobalName, _, _) =>
        val oldc = util.Try(library.getConstant(source)).getOrElse(return Set())

        (oldc.tp zip cleanedTp).map {
          case (FunType(oldIn, _), FunType(cleanedIn, cleanedOut)) =>
            val previouslyNamedArgs = oldIn.map(_._1).zipWithIndex.collect {
              case (Some(argName), idx) => (argName, idx + 1) // argument positions are one-based for us
            }
            val argsToRemove = previouslyNamedArgs.flatMap {
              case (namedArg, idx) =>
                val partialCleanedTp = FunType(cleanedIn.drop(idx), cleanedOut)
                if (partialCleanedTp.freeVars.contains(namedArg)) { // argument is still used
                  None
                } else { // argument became unused, hence collect it in set of arguments to remove
                  Some(idx)
                }
            }.toSet
            argsToRemove
        }.getOrElse(Set.empty)

      case _ => Set.empty
    }
  }
}

class KeepAwareHeuristic(private val baseHeuristic: LinearDropArgsHeuristic) extends LinearDropArgsHeuristic {
  override def apply(c: Constant, cleanedTp: Option[Term], cleanedDf: Option[Term], dropped: DropInfo)(implicit library: Library): Set[ArgPath] = {
    val keepInfo = KeepAwareHeuristic.parse(c.metadata)
    baseHeuristic(c, cleanedTp, cleanedDf, dropped).filterNot(keepInfo.mustKeep)
  }
}

// TODO: rename and integrate somewhere
object KeepAwareHeuristic {
  sealed abstract class KeepInfo {
    def mustKeep(argPath: ArgPath): Boolean
    def toMetaDatum: MetaData = KeepAwareHeuristic.print(this)
  }
  case class KeepAll() extends KeepInfo {
    override def mustKeep(argPath: ArgPath): Boolean = true
  }
  case class KeepAsDesired() extends KeepInfo {
    override def mustKeep(argPath: ArgPath): Boolean = false
  }
  case class KeepSpecific(positions: Set[ArgPath]) extends KeepInfo {
    override def mustKeep(argPath: ArgPath): Boolean = positions.contains(argPath)
  }

  object Metadata {
    val keepInfoKey: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?drop_args_keep")
    val keepAll: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?drop_args_keep_all")
    val keepSpecific: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?drop_args_keep_specific")
  }

  /**
    * See urtheories/module-expressions.mmt for how KeepInfo needs to be given as meta values.
    */
  def parse(metadata: MetaData): KeepInfo = {
    def parseSpecificString(str: String) = KeepSpecific(str.split(",").map(Integer.parseInt).toSet)

    val keepInfo: Option[KeepInfo] =
      metadata.get(Metadata.keepInfoKey).headOption.map(_.value).collect {
        case OMS(Metadata.keepAll) => KeepAll()
        case Strings(keep) => parseSpecificString(keep) // syntax abbreviating case below
        case OMA(OMS(Metadata.keepSpecific), List(Strings(keep))) => parseSpecificString(keep)
      }

    keepInfo.getOrElse(KeepAsDesired())
  }

  def print(keepInfo: KeepInfo): MetaData = keepInfo match {
    case KeepAll() => MetaData(MetaDatum(Metadata.keepInfoKey, OMS(Metadata.keepAll)))
    case KeepAsDesired() => MetaData() // empty
    case KeepSpecific(positions) => MetaData(MetaDatum(
      Metadata.keepInfoKey,
      OMA(OMS(Metadata.keepSpecific), List(Strings(positions.mkString(","))))
    ))
  }
}