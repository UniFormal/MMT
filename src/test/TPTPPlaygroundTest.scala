import info.kwarc.mmt.api.frontend.{Controller, Extension}

object TPTPPlaygroundTest extends MagicTest {

  override def doFirst(): Unit = {
    super.doFirst()
  }

  def run(): Unit = {
    // fully-qualified Scala class name to Extension object in one MMT archive
    val clazz = "latin2.tptp.Playground"

    val ext = loadMMTExtensionFromArchives(clazz)(controller).get
    controller.extman.addExtension(ext)
  }

  /**
    * Loads an [[Extension]] object from MMT archives.
    *
    * @param objClazz Fully qualified Scala class name referencing the MMT extension (a Scala object), i.e. without
    *                 trailing $.
    */
  private def loadMMTExtensionFromArchives(objClazz: String)(implicit ctrl: Controller): Option[Extension] = {
    ctrl.backend.loadClass(objClazz + "$").map(_.getField("MODULE$").get(null).asInstanceOf[Extension])
  }
}
