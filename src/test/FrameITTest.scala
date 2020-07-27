import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.objects.{OMID, Term}
import info.kwarc.mmt.api.symbols.FinalConstant
import info.kwarc.mmt.api.{GlobalName, LocalName, NamespaceMap, Path}
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
    val str = SOMDoc.JSONBridge.encode(
      SOMDoc.OMDocBridge.encode(ApplySpine(OMID(frameworldArchiveNS ? "DummyTheory" ? "tuple"), RealLiterals(1.0), RealLiterals(2.0), RealLiterals(3.0)))
    )

    print(str.toString())

    val obj = SOMDoc.OMDocBridge.decode(SOMDoc.JSONBridge.decodeTerm(str.toString()))
    print(obj)
  }
}
