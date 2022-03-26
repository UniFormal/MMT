package info.kwarc.mmt.api.utils

object RunJavaClass {
  /** Same as apply, but without arguments */
  def apply(clz: String): ProcessBuilder = apply(clz, Nil, s => ())

  /**
    * Run a java class (i.e. one with a main method) inside a new java process
    * Inherits the classpath from the current J
    */
  def apply(clz: String, cargs: List[String], log: String => Unit): ProcessBuilder = {
    val cp = System.getProperty("java.class.path")
    val args =
      sys.env.get("JAVA_OPTS").map(_.split(" ").toList).getOrElse(Nil) ::: List(
        "-cp",
        cp,
        clz,
      ) ::: cargs
    log(args.mkString(" "))
    runLongJavaCommand(
      javaBinary = System.getProperty("java.home") + java.io.File.separator + "bin" + java.io.File.separator + "java",
      args
    )
  }

  /**
    * Invoke the `java` command with possibly large arguments.
    *
    * To support large arguments, we use the @argumentFile syntax:
    * https://docs.oracle.com/javase/9/tools/java.htm#JSWOR-GUID-4856361B-8BFD-4964-AE84-121F5F6CF111
    *
    * That is, we create a file, say `file.txt`, holding the arguments and then invoke `java @file.txt`.
    *
    * @param javaBinary Path to Java binary
    * @param args Arguments
    */
  private def runLongJavaCommand(javaBinary: String, args: List[String]): ProcessBuilder = {
    val argsFile = java.io.File.createTempFile("MMT-RunJavaClassJavaArgs", ".txt")
    File.write(argsFile, args.mkString(" "))
    // TODO: temp file produced by RunJavaClass is never deleted
    new ProcessBuilder(javaBinary, s"@${argsFile.getAbsolutePath}")
  }
}
