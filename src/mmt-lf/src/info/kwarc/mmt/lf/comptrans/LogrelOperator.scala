package info.kwarc.mmt.lf.comptrans

import info.kwarc.mmt.KeepAwareHeuristic
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.libraries.Library
import info.kwarc.mmt.api.modules.diagrams._
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects.{Context, OMMOD, OMS, Term}
import info.kwarc.mmt.api.symbols.{Constant, Declaration, TermContainer}
import info.kwarc.mmt.lf.{Beta, Strings}

import scala.collection.mutable

sealed case class LogrelConfiguration(arity: Integer, initiallyUndefinedSymbols: Seq[GlobalName], metaDiagram: Diagram) {
  require(arity >= 1)
}

object LogrelOperator extends ParametricLinearOperator {
  override val head: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?logrel")

  // currently unused, does not work
  def moduleSuffixFor(config: LogrelConfiguration): LocalName =
    LocalName(config.arity.toString) / config.initiallyUndefinedSymbols.map(_.toLocalName).reduce(_ / _)

  override def instantiate(parameters: List[Term])(implicit interp: DiagramInterpreter): Option[LinearOperator] = parameters match {
    case List(Strings(arityStr), metaDiagramTerm, SymbolPaths(initiallyUndefinedSymbols)) =>
      val metaDiagram = interp(metaDiagramTerm).getOrElse(return None)
      val config = LogrelConfiguration(
        Integer.parseInt(arityStr),
        initiallyUndefinedSymbols,
        metaDiagram
      )
      Some(new ZippingOperator( // order is important
        (0 until config.arity).map(new LogrelConnector(config, _)).toList :+ new LogrelFunctor(config)
      ).withFocus(-1))

    case _ => None
  }
}

class LogrelFunctor(config: LogrelConfiguration) extends LinearFunctor {
  override val dom: Diagram = config.metaDiagram
  override val cod: Diagram = config.metaDiagram
  override def applyDomainModule(m: MPath): MPath = m

  override protected def applyModuleName(name: LocalName): LocalName =
    name.suffixLastSimple("_logrel") // / LogrelOperator.moduleSuffixFor(config)

  private val arity = config.arity // shorthand

  val copyRenamers: Array[SystematicRenamer] = (0 until arity).map(
    i => getRenamerFor(s"_$i") // nicer labeling: UnicodeStrings.superscriptInteger(i)) ?
  ).toArray
  val lrRenamer: SystematicRenamer = getRenamerFor("_r") // nicer labeling: "ᕁ" ?

  lazy private val connectors = (0 until arity).map(new LogrelConnector(config, _)).toArray

  private val undefinedSymbols: mutable.Set[GlobalName] = mutable.Set(config.initiallyUndefinedSymbols : _*)

  override def translateConstant(c: Constant)(implicit interp: DiagramInterpreter): List[Declaration] = {
    implicit val ctrl: Controller = interp.ctrl
    implicit val library: Library = ctrl.library

    val connectingMorphisms: Array[Term] = connectors.map(conn =>
      OMMOD(conn.applyModulePath(expressionContext(c).toMPath))
    )

    val copies = (0 until arity).map(i => {
      def tr(t: Term): Term = ctrl.library.ApplyMorphs(t, connectingMorphisms(i))

      val copy = Constant(
        home = OMMOD(copyRenamers(i)(c.path).module),
        name = copyRenamers(i)(c.path).name,
        alias = Nil,
        tpC = TermContainer.asParsed(c.tp.map(tr)),
        dfC = TermContainer.asParsed(c.df.map(tr)),
        rl = c.rl,
        notC = NotationContainer.empty()
      )
      copy.metadata.add(c.metadata.getAll : _*)
      copy
    })

    val logrel = new CompositionalTranslation(
      connectingMorphisms.map(new CompositionalTotalMorphism(_)).toList,
      p => if (undefinedSymbols.contains(p)) None else Some(OMS(lrRenamer(p)))
    )

    /*val logrel = new PartialLogrel(
      mors = connectingMorphisms,
      p => if (undefinedSymbols.contains(p)) None else Some(OMS(lrRenamer(p))),
      interp.ctrl.library
    )*/

    val relationConstant: Option[Constant] = if (undefinedSymbols.contains(c.path)) None else {
      implicit val ctrl: Controller = interp.ctrl
      c.tp
        .flatMap(oldTp => logrel.getExpected(Context.empty, c.toTerm, oldTp))
        .map(Beta.reduce)
        .map(tp => {
          val df = c.df.flatMap(logrel(Context.empty, Nil, _)).map(Beta.reduce)
          require(!(c.df.nonEmpty && df.isEmpty)) // logical relations are term-total

          val relc = Constant(
            home = OMMOD(applyModulePath(c.path.module)),
            name = lrRenamer(c.name),
            alias = c.alias,
            tpC = TermContainer.asParsed(tp),
            dfC = TermContainer.asParsed(df),
            rl = None,
            notC = NotationContainer.empty()
          )
          relc.metadata.add(KeepAwareHeuristic.KeepAll().toMetaDatum.getAll : _*)
          relc
        })
    }

    if (relationConstant.isEmpty) undefinedSymbols += c.path

    copies.toList ::: relationConstant.toList
  }
}

class LogrelConnector(config: LogrelConfiguration, index: Integer) extends InwardsLinearConnector {
  require(0 <= index && index < config.arity)

  lazy override val out: LogrelFunctor = new LogrelFunctor(config)
  override def applyDomainModule(thy: MPath): MPath = thy

  override protected def applyModuleName(name: LocalName): LocalName =
    name.suffixLastSimple(s"_logrel_view$index") // / LogrelOperator.moduleSuffixFor(config) / s"view${index}"

  override def translateConstant(c: Constant)(implicit interp: DiagramInterpreter): List[Declaration] = {
    List(assgn(c.path, OMS(out.copyRenamers(index).applyAlways(c.path))))
  }
}
