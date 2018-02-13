package info.kwarc.mmt.api.test.utils.testers

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

/** an extension to be tested */
case class ExtensionSpec(name: String, args: String*)
