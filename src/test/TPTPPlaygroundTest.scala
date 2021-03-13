import info.kwarc.mmt.api.frontend.{Controller, Extension}

object TPTPPlaygroundTest extends MagicTest {

  override def doFirst(): Unit = {
    super.doFirst()
  }

  def run(): Unit = {
    loadMMTExtensionFromArchives("latin2.tptp.SFOLExporter")(controller)
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
