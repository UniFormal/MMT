import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.objects.{OMID, Term}
import info.kwarc.mmt.api.symbols.FinalConstant
import info.kwarc.mmt.api.{GlobalName, LocalName, NamespaceMap, Path}
import info.kwarc.mmt.frameit.ViewCompletion
import info.kwarc.mmt.lf.ApplySpine

/**
  * Playground for Navid's backend implementation of UFrameIT.
  * For debugging purposes only - might contain dirty code.
  *
  * @author Navid
  */
object FrameITTest extends MagicTest("debug") {

  override def doFirst: Unit = {
    super.doFirst
    // Only uncomment if rebuild is really necessary
    // hl("build FrameIT/frameworld mmt-omdoc")
    // hl("build MMT/urtheories mmt-omdoc")

    // Only uncomment if rebuild is really necessary
    // hl("build MitM/Foundation mmt-omdoc")

    // Clean first to prevent some spurious caching errors
    // hl("build Playground/frameit mmt-omdoc")
    //controller.extman.addExtension(new FrameitServerExtension)
  }

  private val frameworldArchiveNS = Path.parseD("http://mathhub.info/FrameIT/frameworld", NamespaceMap.empty)

  // This [[run]] method is run in parallel to the build process started above in [[doFirst]],
  // hence, we apply some dirty waiting mechanism here.
  override def run: Unit = {
    val domainTheoryP = frameworldArchiveNS ? "OppositeLen_Problem"
    val domainTheory = waitUntilAvailable(domainTheoryP).asInstanceOf[Theory]

    // test 1
    {
      val codomainTheory = controller.getTheory((frameworldArchiveNS / "integrationtests") ? "CloseGapTest_Codomain")

      // read off complex expression for assignment from the definiens of an existing constant
      val angleValue = codomainTheory.get(LocalName("complexAngleFact")).asInstanceOf[FinalConstant].df.get

      val assignments: List[(GlobalName, Term)] = List(
        (domainTheoryP ? "pangleABC", angleValue)
      )

      println(angleValue)

      println(ViewCompletion.closeGaps(
        assignments,
        domainTheory.meta.get
      )(controller))
    }
  }
}
