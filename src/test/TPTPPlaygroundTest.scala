import info.kwarc.mmt.api.ImplementationError
import info.kwarc.mmt.api.frontend.{Controller, Extension}

object TPTPPlaygroundTest extends MagicTest {

  override def doFirst(): Unit = {
    super.doFirst()
  }

  def run(): Unit = {
    if (!loadMMTExtensionFromArchives("latin2.tptp.TPTPExporter")(controller)) {
      throw ImplementationError("Extension cannot be loaded from archive!")
    }
    controller.handleLine("build MMT/LATIN2 mmt-omdoc playground/tptp-exporter_monoid.mmt")
    controller.handleLine("build MMT/LATIN2 tptp playground/tptp-exporter_monoid.omdoc")
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
