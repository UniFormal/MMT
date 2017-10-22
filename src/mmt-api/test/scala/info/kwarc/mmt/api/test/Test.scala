package scala.info.kwarc.mmt.api.test

import info.kwarc.mmt.api.DPath
import info.kwarc.mmt.doc.Setup
import info.kwarc.mmt.api.backend.MathHub
import info.kwarc.mmt.api.frontend.Run
import info.kwarc.mmt.api.utils.{File, URI}
import org.scalatest.{BeforeAndAfterAll, FlatSpec, Matchers}

abstract class MMTTest(archives : String*)(extensions : String*) extends FlatSpec with Matchers /* with BeforeAndAfterAll */ {
  lazy val controller = Run.controller
  val standardextensions =
    List(
      "info.kwarc.mmt.api.ontology.AlignmentsServer",
      "info.kwarc.mmt.api.web.JSONBasedGraphServer"
  )
  val setupFolder = File("test/resources").canonical
  val content = setupFolder / "content" 
  lazy val mathhub = new MathHub(MathHub.defaultURL,content,report = controller.report,https=true)
  def hl(s : String) = controller.handleLine(s).throwErrorIfAny()
  def shouldhl(s : String) = it should s in hl(s)

  behavior of "MMT"

  val setup = new Setup
  controller.extman.addExtension(setup)
  setup.setup(setupFolder/"system", content, None)

  (standardextensions ::: extensions.toList) foreach (e =>
    it should "add Extension " + e in hl("extension " + e)
    )

  val ret = it should "add archives" in {
    hl("mathpath archive " + content.toJava.getAbsolutePath)
    hl("log console")
  }

  // override protected def afterAll(): Unit = content.deleteDir
}

class APITest extends MMTTest("MMT/examples")() {
  behavior of "APITest"
  lazy val brackets = (DPath(URI.http colon "cds.omdoc.org") / "mmt") ? "mmt" ? "brackets"
  it should "get a Constant: " in controller.getConstant(brackets)
}