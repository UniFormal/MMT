import cats.effect.IO
import info.kwarc.mmt.api.metadata.MetaDatum
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.notations.{Delim, Mixfix, MixfixNotation, NotationContainer, Precedence, SimpArg, TextNotation}
import info.kwarc.mmt.api.objects.{OMA, OMID, OML, OMMOD, OMV, Term}
import info.kwarc.mmt.api.presentation.MMTSyntaxPresenter
import info.kwarc.mmt.api.symbols.{FinalConstant, TermContainer, Visibility}
import info.kwarc.mmt.api.{GlobalName, LocalName, NamespaceMap, Path, presentation}
import info.kwarc.mmt.frameit.business.{KnownFact, TermPair, ViewCompletion}
import info.kwarc.mmt.frameit.communication.ServerEndpoints.{jsonBody, path, post}
import info.kwarc.mmt.lf.{ApplySpine, FunTerm, FunType}
import io.circe.Encoder
import io.circe.generic.auto._
import io.finch.{Endpoint, Ok}

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
    println("Test")
  }
}
