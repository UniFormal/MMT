package info.kwarc.mmt.api.modules.diagrams

import info.kwarc.mmt.api.{ComplexStep, LocalName, MPath}
import info.kwarc.mmt.api.modules.ModuleOrLink
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects.{OMMOD, Term}
import info.kwarc.mmt.api.symbols.{Constant, FinalConstant}

class PushoutOperator(m: Term, dom: MPath, cod: MPath) extends LinearOperators {
  override val ops: LinearOperatorSpecs = resolve(List(
    ("P", Diagram.singleton(dom) -> Diagram.singleton(cod), DiagramFunctor.singleton(dom, cod), suffixBy("_Pushout")),
    ("in", Id(dom) -> "P", DiagramConnection.Singleton(dom, cod, m), suffixBy("_Projection"))
  ))

  def applyConstant(c: Constant, container: ModuleOrLink)(implicit interp: DiagramInterpreter): Unit = {
    def tr(t: Term): Term = interp.ctrl.library.ApplyMorphs(t, OMMOD(ops("in")(c.parent)))

    // todo: potentially use Constant.translate method
    val newC = new FinalConstant(
      home = OMMOD(ops("P")(c.parent)),
      name = ops("P")(c.name),
      alias = c.alias.map(ops("P")(_)),
      tpC = c.tpC.map(tr),
      dfC = c.dfC.map(tr),
      rl = c.rl,
      notC = NotationContainer.empty(),
      vs = c.vs
    )
    newC.metadata.add(c.metadata.getAll : _*) // TODO: shall we translate meta data via tr(), too?
    interp.add(newC)

    val cAssignment = Constant(
      home = OMMOD(ops("in")(c.parent)),
      name = LocalName(ComplexStep(c.parent) :: c.name),
      alias = Nil,
      tp = c.tpC.map(tr).get,
      df = Some(newC.toTerm),
      rl = None
    )
    interp.add(cAssignment)
  }
}
