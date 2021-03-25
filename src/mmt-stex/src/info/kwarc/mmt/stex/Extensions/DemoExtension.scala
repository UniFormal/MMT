package info.kwarc.mmt.stex.Extensions

import info.kwarc.mmt.api.GlobalName
import info.kwarc.mmt.api.objects.Term
import info.kwarc.mmt.api.utils.MMTSystem
import info.kwarc.mmt.api.web.{ServerRequest, ServerResponse}
import info.kwarc.mmt.odk.Sage.SageSystem
import info.kwarc.mmt.stex.translations
import info.kwarc.mmt.stex.translations.DemoContent
import info.kwarc.mmt.stex.xhtml.{PreElement, XHTML}

object DemoExtension extends STeXExtension {
  override def start(args: List[String]): Unit = try {
    super.start(args)
    DemoContent.add(controller)
    implicit val xhtmlrules = BasicExtension.xhtmlRules
    val filecontent = XHTML.applyString(MMTSystem.getResourceAsString("mmt-web/stex/demo/test.xhtml")).head
    PreElement.extract(filecontent)(controller)
  } catch {
    case t : Throwable =>
      print("")
  }
  override def serverReturn(request: ServerRequest): Option[ServerResponse] = {
    if (request.path.tail.isEmpty)
      Some(ServerResponse(MMTSystem.getResourceAsString("/mmt-web/stex/demo/index.html"),"html"))
    else None
  }

  override lazy val translators: List[Translator] = {
    val mitm_translator = new Translator("MitM") {
      override def applicable(tm: Term): Boolean = true
      val trl = translations.MitM.getTranslator(controller)
      override def translate(tm: Term): (Term, List[GlobalName]) = trl.translate(tm)
    }
    val sagesys = controller.extman.get(classOf[SageSystem]) match {
      case ss :: _ => ss
      case _ =>
        val ss = new SageSystem
        controller.extman.addExtension(ss)
        ss.warmup()
        ss
    }
    val sage_translator = new Translator("Sage") {
      override def applicable(tm: Term): Boolean = true
      val trl = sagesys.translator_to
      override def translate(tm: Term): (Term, List[GlobalName]) = trl.translate(tm)
    }
    List(mitm_translator,sage_translator)
  }
}