package info.kwarc.mmt.api.test.utils.testers

import info.kwarc.mmt.api.test.utils.ExtensionSpec

/** trait implementing testing for extensions */
trait ExtensionTester extends BaseTester {

  /** list of extensions to be checked */
  val extensions: List[ExtensionSpec]

  /** check that an extension gets loaded properly */
  private def shouldAddExtension(extension: ExtensionSpec): Unit = {
    it should s"add extension ${extension.name}" in {
      val ext = controller.extman.addExtension(extension.name, extension.args.toList)
      assert(ext != null)
    }
  }

  /** checks that all extensions are loaded */
  def shouldLoadExtensions(): Unit = extensions.foreach(e => shouldAddExtension(e))
}
