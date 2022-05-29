import info.kwarc.mmt.api.ImplementationError
import info.kwarc.mmt.api.frontend.{Controller, Extension}

object TPTPPlaygroundTest extends MagicTest {

  //1. LATIN project -> LATIN module settings: mark lib/tptp-parser/main/scala as sources root
  //2. MMT project -> src module: open module settings, add LATIN module as module dependency

  override def doFirst(): Unit = {
    super.doFirst()
  }

  def run(): Unit = {
    controller.handleLine("log+ debug")
    //controller.handleLine("log+ object-checker")
    if (!loadMMTExtensionFromArchives("latin2.tptp.TPTPExporter")(controller)) {
      throw ImplementationError("Extension cannot be loaded from archive!")
    }
    //controller.handleLine("build MMT/LATIN2 mmt-omdoc casestudies/2022-tptp/tptp-exporter_monoid.mmt")
    //controller.handleLine("build MMT/LATIN2 tptp casestudies/2022-tptp/tptp-exporter_monoid.omdoc")

    //controller.handleLine("build MMT/LATIN2 mmt-omdoc casestudies/2022-tptp/tptp-exporter_hol.mmt")
    //controller.handleLine("build MMT/LATIN2 tptp casestudies/2022-tptp/tptp-exporter_hol.omdoc")

    controller.handleLine("build MMT/LATIN2 mmt-omdoc casestudies/2022-tptp/peano.mmt")
    controller.handleLine("build MMT/LATIN2 tptp casestudies/2022-tptp/peano.omdoc")
  }

  /**
    * Instantiates an [[Extension]] (to be found in some loaded archive) and adds it to the [[Controller]].
    *
    * @param clazz Fully qualified Scala class name referencing the MMT extension (a Scala class)
    */
  private def loadMMTExtensionFromArchives(clazz: String)(implicit ctrl: Controller): Boolean = {
    ctrl.backend
      .loadClass(clazz)
      .map(_.getDeclaredConstructor().newInstance().asInstanceOf[Extension])
      .map(ctrl.extman.addExtension(_))
      .isDefined
  }
}
