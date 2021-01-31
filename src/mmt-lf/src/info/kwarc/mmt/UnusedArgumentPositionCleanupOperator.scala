package info.kwarc.mmt

import info.kwarc.mmt.api.metadata.MetaData
import info.kwarc.mmt.api.modules.diagrams._
import info.kwarc.mmt.api.notations.{Mixfix, NotationContainer}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.{GlobalName, LocalName, Path}
import info.kwarc.mmt.lf._

import scala.collection.mutable

object UnusedArgumentPositionCleanupOperator extends ParametricLinearOperator {
  override val head: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?unused_arg_pos_cleanup_operator")


  override def instantiate(parameters: List[Term])(implicit interp: DiagramInterpreter): Option[LinearTransformer] =
    Some(UnusedArgumentPositionCleanupTransformer)
}

object UnusedArgumentPositionCleanupTransformer extends SimpleConstantsBasedModuleTransformer with LackingLinearStateOperator with OperatorDSL {
  override def operatorDomain: Diagram = Diagram.singleton(LF.theoryPath)

  override def operatorCodomain: Diagram = Diagram.singleton(LF.theoryPath)

  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_cleaned")

  type LinearState = MyState

  class MyState(diagramState: DiagramState, inContainer: Container) extends SkippedDeclsExtendedLinearState(diagramState, inContainer) {
    val removedArguments: mutable.Map[GlobalName, List[UnusedArgumentsCleaner.ArgPath]] = mutable.Map()

    override def inherit(other: MyState): Unit = {
      super.inherit(other)
      removedArguments ++= other.removedArguments
    }
  }

  override def initLinearState(diagramState: DiagramState, inContainer: Container): MyState = new MyState(diagramState, inContainer)


  override protected def applyConstantSimple(c: Constant, tp: Term, df: Option[Term])(implicit state: LinearState, interp: DiagramInterpreter): List[Constant] = {
    val outConstant = Constant(
      home = state.outContainer.toTerm,
      name = emptyRenamer(c.name),
      alias = c.alias,
      tp = c.tp.map(emptyRenamer(_)),
      df = c.df.map(emptyRenamer(_)),
      rl = c.rl,
      not = c.notC.copy()
    )
    outConstant.metadata.add(c.metadata.getAll : _*)
    UnusedArgumentsCleaner.cleanArgumentsOfType(outConstant, state.removedArguments)

    List(outConstant)
  }
}

/**
  * NOTE: removedArgs one-based everywhere (to be analogous to argument marker positions in notations).
  */
object UnusedArgumentsCleaner {
  object Metadata {
    val keepInfoKey: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?unused_arg_pos_cleanup_operator_meta_keep_info")
    val keepAll: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?unused_arg_pos_cleanup_operator_meta_keep_all")
    val keepSpecific: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?unused_arg_pos_cleanup_operator_meta_keep_specific")
  }

  type ArgPath = Int

  abstract class KeepInfo {
    def mustKeep(argPath: ArgPath): Boolean
  }
  sealed case class KeepAll() extends KeepInfo {
    override def mustKeep(argPath: ArgPath): Boolean = true
  }
  sealed case class KeepAsDesired() extends KeepInfo {
    override def mustKeep(argPath: ArgPath): Boolean = false
  }
  sealed case class KeepSpecific(positions: Set[ArgPath]) extends KeepInfo {
    override def mustKeep(argPath: ArgPath): Boolean = positions.contains(argPath)
  }


  /**
    * One-based since argument positions of notations are also one-based.
    */
  val INITIAL_PATH: ArgPath = 1

  def cleanArgumentsOfType(c: Constant, removedArgs: mutable.Map[GlobalName, List[ArgPath]]): Unit = {
    val keepInfo = readKeepInfo(c.metadata)

    c.tpC.analyzed = c.tp
      .map(cleanRemovedArguments(_, removedArgs.toMap))
      .map(cleanUnusedArguments(_, keepInfo))
      .map {
        case (cleanedTerm, removedArgsHere) =>
          if (removedArgsHere.nonEmpty) {
            removedArgs.put(c.path, removedArgsHere)
          }
          cleanedTerm
      }

    c.df

    c.notC.collectInPlace(not => {
      val newFixity = not.fixity match {
        case fixity: Mixfix =>
          fixity.removeArguments(removedArgs.getOrElse(c.path, Nil).toSet)
        case x => None // not implemented yet
      }
      newFixity.map(fix => not.copy(fixity = fix))
    })
  }

  private def readKeepInfo(metadata: MetaData): KeepInfo = {
    val keepInfo: Option[KeepInfo] =
      metadata.get(Metadata.keepInfoKey).headOption.map(_.value).collect {
        case OMS(Metadata.keepAll) =>
          KeepAll()
        case OMA(OMS(Metadata.keepSpecific), List(Strings(keep))) =>
          KeepSpecific(keep.split(",").map(Integer.parseInt).toSet)

        case ApplySpine(OMS(Metadata.keepSpecific), List(Strings(keep))) => // TODO: due to meta values being parsed very strangely
          KeepSpecific(keep.split(",").map(Integer.parseInt).toSet)
      }

    keepInfo.getOrElse(KeepAsDesired())
  }

  def cleanRemovedArguments(t: Term, removedArgs: Map[GlobalName, List[ArgPath]]): Term = {
    new StatelessTraverser {
      override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
        case ApplySpine.orSymbol(OMS(p), args) if removedArgs.contains(p) && removedArgs(p).nonEmpty =>
          val argsToRemove = removedArgs(p).toSet

          if (args.size < argsToRemove.max) { // recall: argsToRemove has one-based argument positions
            // error, not eta-expanded by user
            ???
          }

          val newArgs = args.zipWithIndex.filter {
            // recall: argsToRemove has one-based argument positions
            case (_, index) => !argsToRemove.contains(index + 1)
          }.map(_._1)
          ApplySpine(OMS(p), newArgs : _*)

        case t => Traverser(this, t)
      }
    }.apply(t, Context.empty)
  }

  def cleanUnusedArguments(t: Term, keepInfo: KeepInfo): (Term, List[ArgPath]) = keepInfo match {
    case KeepAll() => (t, Nil) // optimization
    case _ => cleanUnusedArgumentsHelper(t, INITIAL_PATH)(keepInfo)
  }

  def cleanUnusedArgumentsHelper(t: Term, path: ArgPath)(implicit keepInfo: KeepInfo): (Term, List[ArgPath]) = t match {
    // early-match on Arrow to prevent Pi from matching Arrows
    case Arrow(arg, body) =>
      val (newBody, removedArgs) = cleanUnusedArgumentsHelper(body, path + 1)
      (Arrow(arg, newBody), removedArgs)

    case t@(Pi(_, _, _) | Lambda(_, _, _)) =>
      val (v, tp, body) = (t: @unchecked) match {
        case Pi(v, tp, body) => (v, tp, body)
        case Lambda(v, tp, body) => (v, tp, body)
      }

      val (newBody, removedArgs) = cleanUnusedArgumentsHelper(body, path + 1)

      if (keepInfo.mustKeep(path) || occursIn(v, newBody)) {
        // keep bound variable
        (t: @unchecked) match {
          case Pi(_, _, _) => (Pi(v, tp, newBody), removedArgs)
          case Lambda(_, _, _) => (Lambda(v, tp, newBody), removedArgs)
        }
      } else {
        // remove bound variable
        (newBody, path :: removedArgs)
      }

    case t => (t, Nil)
  }

  private def occursIn(v: LocalName, t: Term): Boolean = {
    val signalFound = () => return true

    (new StatelessTraverser {
      override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
        case OMV(`v`) => signalFound()
        case _ => Traverser(this, t)
      }
    })(t, Context.empty)
    false
  }
}
