package info.kwarc.mmt

import info.kwarc.mmt.UnusedArgumentsCleaner.KeepInfo
import info.kwarc.mmt.api.libraries.Lookup
import info.kwarc.mmt.api.metadata.MetaData
import info.kwarc.mmt.api.modules.diagrams._
import info.kwarc.mmt.api.notations.{Mixfix, NotationContainer}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.{GlobalName, LocalName, Path}
import info.kwarc.mmt.lf._

import scala.collection.mutable

object DropUnusedArgumentsOperator extends ParametricLinearOperator {
  override val head: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?drop_args_operator")


  override def instantiate(parameters: List[Term])(implicit interp: DiagramInterpreter): Option[LinearTransformer] =
    Some(DropUnusedArgumentsTransformer)
}

object DropUnusedArgumentsTransformer extends SimpleConstantsBasedModuleTransformer with LackingLinearStateOperator with OperatorDSL {
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
    implicit val lookup: Lookup = interp.ctrl.globalLookup

    val keepInfo: KeepInfo = UnusedArgumentsCleaner.KeepInfo.parse(c.metadata)

    val newTp = c.tp
      .map(UnusedArgumentsCleaner.cleanArgumentsOfType(c.path, _, keepInfo, state.removedArguments))
      .map(emptyRenamer(_))

    val newDf = c.df
      .map(UnusedArgumentsCleaner.cleanArgumentsOfDef(c.path, _, state.removedArguments))
      .map(emptyRenamer(_))

    val newNotC = UnusedArgumentsCleaner.cleanArgumentsInNotation(c.path, c.notC, state.removedArguments)

    val outConstant = Constant(
      home = state.outContainer.toTerm,
      name = emptyRenamer(c.name),
      alias = c.alias,
      tp = newTp,
      df = newDf,
      rl = c.rl,
      not = newNotC
    )
    // outConstant.metadata.add(c.metadata.getAll : _*), too much clutter

    List(outConstant)
  }
}

/**
  * NOTE: removedArgs one-based everywhere (to be analogous to argument marker positions in notations).
  */
object UnusedArgumentsCleaner {
  object Metadata {
    val keepInfoKey: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?drop_args_keep")
    val keepAll: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?drop_args_keep_all")
    val keepSpecific: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?drop_args_keep_specific")
  }

  type ArgPath = Int

  abstract class KeepInfo {
    def mustKeep(argPath: ArgPath): Boolean
  }
  object KeepInfo {
    /**
      * See urtheories/module-expressions.mmt for how KeepInfo needs to be given as meta values.
      */
    def parse(metadata: MetaData): KeepInfo = {
      def parseSpecificString(str: String) = KeepSpecific(str.split(",").map(Integer.parseInt).toSet)

      val keepInfo: Option[KeepInfo] =
        metadata.get(Metadata.keepInfoKey).headOption.map(_.value).collect {
          case OMS(Metadata.keepAll) => KeepAll()
          case Strings(keep) => parseSpecificString(keep)
          case OMA(OMS(Metadata.keepSpecific), List(Strings(keep))) => parseSpecificString(keep)
          // TODO: due to meta values being parsed very strangely
          case ApplySpine(OMS(Metadata.keepSpecific), List(Strings(keep))) => parseSpecificString(keep)
        }

      keepInfo.getOrElse(KeepAsDesired())
    }
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

  def cleanArgumentsOfType(p: GlobalName, tp: Term, keepInfo: KeepInfo, removedArgs: mutable.Map[GlobalName, List[ArgPath]])(implicit lookup: Lookup): Term = {
    val (cleanedTerm, removedArgsHere) = cleanUnusedArguments(cleanRemovedArguments(tp, removedArgs.toMap), keepInfo)

    if (removedArgsHere.nonEmpty) {
      removedArgs.put(p, removedArgsHere)
    }
    cleanedTerm
  }

  def cleanArgumentsOfDef(p: GlobalName, df: Term, removedArgs: mutable.Map[GlobalName, List[ArgPath]])(implicit lookup: Lookup): Term = {
    cleanRemovedArguments(df, removedArgs.toMap) // insufficient for defined constants, but okay-ish for view assignments
  }

  /**
    * Cleans notation in a copy of input notation container
    */
  def cleanArgumentsInNotation(p: GlobalName, notC: NotationContainer, removedArgs: mutable.Map[GlobalName, List[ArgPath]]): NotationContainer = {
    notC.copy().collectInPlace(not => {
      val newFixity = not.fixity match {
        case fixity: Mixfix =>
          fixity.removeArguments(removedArgs.getOrElse(p, Nil).toSet)
        case _ => None // not implemented yet
      }
      newFixity.map(fix => not.copy(fixity = fix))
    })
  }

  def etaExpand(p: GlobalName)(implicit lookup: Lookup): Term = {
    lookup.getConstant(p).tp match {
      case Some(FunType(argTypes, _)) =>
        val ctx: Context = argTypes.zipWithIndex.map {
          case ((maybeName, argType), idx) =>
            val name = maybeName.getOrElse(LocalName(s"arg$idx"))
            VarDecl(name, argType)
        }

        Lambda(ctx, ApplySpine(OMS(p), ctx.map(_.toTerm): _*))

      case _ => ??? // cannot eta-expand constant without type component
    }
  }

  def cleanRemovedArguments(t: Term, removedArgs: Map[GlobalName, List[ArgPath]])(implicit lookup: Lookup): Term = {
    new StatelessTraverser {
      override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
        case ApplySpine.orSymbol(OMS(p), args) if removedArgs.contains(p) && removedArgs(p).nonEmpty =>
          val argsToRemove = removedArgs(p).toSet

          if (args.size >= argsToRemove.max) { // (recall: argsToRemove has one-based argument positions)
            // All argument positions that we want to remove are given.

            val newArgs = args.zipWithIndex.filter {
              case (_, index) => !argsToRemove.contains(index + 1)
            }.map(_._1)
            ApplySpine(OMS(p), newArgs : _*)
          } else {
            // At least one argument position that we wanted to remove is not given,
            // hence reduce to eta-expanded case.
            ApplySpine(traverse(etaExpand(p)), args : _*)
          }

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
