// DESCRIPTION: build and deploy MMT, using sbt

object Build_MMT extends isabelle.Isabelle_Tool.Body
{
  import isabelle._

  def apply(args: List[String])
  {
    val progress = new Console_Progress()

    progress.bash("sbt mmt/deploy " + Bash.strings(args),
      cwd = Path.explode("$ISABELLE_MMT_ROOT/src").file,
      echo = true).check
  }
}
