package info.kwarc.mmt.lf.comptrans

import info.kwarc.mmt.KeepAwareHeuristic
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.libraries.Library
import info.kwarc.mmt.api.modules.diagrams._
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects.{Context, OMA, OMMOD, OMS, Term}
import info.kwarc.mmt.api.symbols.{Constant, Declaration, TermContainer}
import info.kwarc.mmt.lf.{Beta, Strings}

import scala.collection.mutable

/**
  * The type ("skeleton") of a compositional translation deliberately excluding domains and codomains.
  *
  * @example A total morphism `m` is represented as [[Skeleton(0, Nil, Nil)]]
  * @example A morphism that `m'` is undefined at symbols in S is represented as [[Skeleton(0, Nil, S)]]
  * @example A total unary logical relation `r` over `m` is represented as [[Skeleton(1, List(m), Nil)]].
  * @example A total binary compositional translation over `m`, `r`, and again `m` is represented as
  *          [[Skeleton(3, List(m, r, m), Nil)]].
  *
  *          The configurations can be seen as trees.
  * @param baseSkeletons
  * @param initiallyUndefinedSymbols
  */
sealed case class Skeleton(
                            baseSkeletons: List[Skeleton],
                            initiallyUndefinedSymbols: Seq[GlobalName],
                            name: Option[String] = None
                          )

sealed case class AugmentedSkeleton(
                                     name: LocalName,
                                     baseSkeletons: List[AugmentedSkeleton],
                                     undefinedSymbols: mutable.Set[GlobalName]
                                   )
object AugmentedSkeleton {
  private val path: GlobalName =
    Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?comptrans_augmented_skeleton")
  def parse(t: Term): Option[AugmentedSkeleton] = t match {
    case OMA(OMS(`path`), Strings(name) +: baseSkeletonTerms :+ SymbolPaths(initiallyUndefinedSymbols)) =>
      val baseSkeletons = baseSkeletonTerms.map(parse).map {
        case Some(x) => x
        case _ => return None
      }
      Some(AugmentedSkeleton(LocalName(name), baseSkeletons, mutable.Set(initiallyUndefinedSymbols : _*)))

    case _ => None
  }
}


object CompTransOperator extends ParametricLinearOperator {
  override val head: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?comptrans")

  override def instantiate(parameters: List[Term])(implicit interp: DiagramInterpreter): Option[LinearOperator] = parameters match {
    case List(skeletonTerm, metaDiagramTerm) =>
      val skeleton = AugmentedSkeleton.parse(skeletonTerm).getOrElse(return None)
      val metaDiagram = interp(metaDiagramTerm).getOrElse(return None)

      Some(new CompTransFunctor(skeleton, metaDiagram))

    case _ => None
  }
}

/**
  *
  * @param skeleton is mutated to add homomorphically undefined symbols
  * @param metaDiagram
  */
class CompTransFunctor(skeleton: AugmentedSkeleton, metaDiagram: Diagram) extends LinearFunctor {
  override val dom: Diagram = metaDiagram
  override val cod: Diagram = metaDiagram
  override def applyDomainModule(m: MPath): MPath = m

  override protected def applyModuleName(name: LocalName): LocalName =
    name.suffixLastSimple("_comptrans") // / LogrelOperator.moduleSuffixFor(config)

  override def translateConstant(c: Constant)(implicit interp: DiagramInterpreter): List[Declaration] = {
    implicit val ctrl: Controller = interp.ctrl
    implicit val library: Library = ctrl.library

    def createThings(skel: AugmentedSkeleton): (List[Constant], CompositionalTranslation) = {
      val baseThings = skel.baseSkeletons.map(createThings)
      val isMorphism = skel.baseSkeletons == Nil

      val renamer = getRenamerFor(skel.name)
      val comptrans = new CompositionalTranslation(
        baseThings.map(_._2),
        p => if (skel.undefinedSymbols.contains(p)) None else Some(OMS(renamer(p))),
        LocalName(skel.name)
      )

      val outConstant: Option[Constant] = if (skel.undefinedSymbols.contains(c.path)) None else {
        c.tp
          .flatMap(oldTp => comptrans.getExpected(Context.empty, c.toTerm, oldTp))
          .map(Beta.reduce)
          .map(tp => {
            val df = c.df.flatMap(comptrans(Context.empty, Nil, _)).map(Beta.reduce)

            val outc = Constant(
              home = OMMOD(applyModulePath(c.path.module)),
              name = renamer(c.name),
              alias = c.alias,
              tpC = TermContainer.asParsed(renamer(tp)),
              dfC = TermContainer.asParsed(df.map(renamer(_))),
              rl = if (isMorphism) c.rl else None,
              notC = NotationContainer.empty()
            )

            if (isMorphism) {
              outc.metadata.add(c.metadata.getAll: _ *)
            } else {
              outc.metadata.add(KeepAwareHeuristic.KeepAll().toMetaDatum.getAll: _*)
            }
            outc
          })
      }
      if (outConstant.isEmpty) {
        skel.undefinedSymbols += c.path
      }
      (baseThings.flatMap(_._1) ::: outConstant.toList, comptrans)
    }

    createThings(skeleton)._1
  }
}
/*
class LogrelConnector(config: LogrelConfiguration, index: Integer) extends InwardsLinearConnector {
  require(0 <= index && index < config.arity)

  lazy override val out: LogrelFunctor = new LogrelFunctor(config)
  override def applyDomainTheory(thy: MPath): Term = OMMOD(thy)

  override protected def applyModuleName(name: LocalName): LocalName =
    name.suffixLastSimple(s"_logrel_view$index") // / LogrelOperator.moduleSuffixFor(config) / s"view${index}"

  override def translateConstant(c: Constant)(implicit interp: DiagramInterpreter): List[Declaration] = {
    List(assgn(c.path, OMS(out.copyRenamers(index).applyAlways(c.path))))
  }
}
*/