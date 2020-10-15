import info.kwarc.mmt.api.objects.Context
import info.kwarc.mmt.api.uom.SimplificationUnit
import info.kwarc.mmt.api.{GlobalName, NamespaceMap, Path}

object TheoryParameterBug extends MagicTest("debug") {
  override def doFirst: Unit = {}

  override def run: Unit = {
    val c = controller.getConstant(Path.parseS("http://mathhub.info/FrameIT/frameworld/integrationtests?TheoryParameterBug?dist", NamespaceMap.empty))

    val ctx = Context(c.path.module)
    val simplicationUnit = SimplificationUnit(
      Context(c.path.module),
      expandDefinitions = true,
      fullRecursion = true
    )

    val simplifiedDf = controller.simplifier.apply(c.df.get, simplicationUnit)
    println(simplifiedDf)
  }
}