import info.kwarc.mmt.api.metadata.MetaDatum
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects.{OMID, OMMOD, Term}
import info.kwarc.mmt.api.presentation.MMTSyntaxPresenter
import info.kwarc.mmt.api.symbols.{FinalConstant, TermContainer, Visibility}
import info.kwarc.mmt.api.{GlobalName, LocalName, NamespaceMap, Path, presentation}
import info.kwarc.mmt.frameit.communication.SOMDoc
import info.kwarc.mmt.frameit.archives.Foundation.RealLiterals
import info.kwarc.mmt.frameit.business.ViewCompletion
import info.kwarc.mmt.lf.ApplySpine

/**
  * Playground for Navid's backend implementation of UFrameIT.
  * For debugging purposes only - might contain dirty code.
  *
  * @author Navid
  */
object FrameITTest extends MagicTest("debug") {

  override val serverport: Option[Int] = None

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
    val theoryPath = frameworldArchiveNS ? "DummyTheory"

    val c = new FinalConstant(
      home = OMMOD(theoryPath),
      name = LocalName("tuple"),
      alias = Nil,
      tpC = TermContainer.asParsed(OMID(frameworldArchiveNS ? "DummyTheory" ? "blah")),
      dfC = TermContainer.empty(),
      rl = None,
      notC = new NotationContainer,
      vs = Visibility.public
    )

    val thy = Theory.empty(theoryPath.parent, theoryPath.name, mt = None)

    c.metadata.add(
      MetaDatum(frameworldArchiveNS ? "DummyTheory" ? "metakey", OMMOD(frameworldArchiveNS ? "DummyTheory2"))
    )

    controller.add(thy)
    controller.add(c)

    val presenter = new MMTSyntaxPresenter()
    presenter.init(controller)

    val rh = new presentation.StringBuilder
    presenter(c)(rh)

    println(rh.get)
  }
}
