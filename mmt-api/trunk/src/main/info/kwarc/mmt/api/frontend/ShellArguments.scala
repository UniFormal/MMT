package info.kwarc.mmt.api.frontend

import java.nio.file.{Files, Paths}

import scala.util.Try

/** describe the kind of a command line option */
sealed class OptionArgument

case object NoArg extends OptionArgument

// just a flag

case object IntArg extends OptionArgument

// expect an integer argument

case object StringArg extends OptionArgument

case object StringListArg extends OptionArgument

// collect multiple values

/** the possible values given by options */
sealed class OptionValue {
  /** convenience method for string lists */
  def getStringList = this.asInstanceOf[StringListVal].value
}

case object NoVal extends OptionValue

case class IntVal(value: Int) extends OptionValue

case class StringVal(value: String) extends OptionValue

case class StringListVal(value: List[String]) extends OptionValue

/** a description of one option */
case class OptionDescr(long: String, short: String, arg: OptionArgument, description: String) {
  def shortMatch(opt: String) = "-" + short == opt

  def longExact(opt: String) = "--" + long == opt

  def longStart(opt: String) = opt.startsWith("--" + long)

  def longEqual(opt: String) = opt.startsWith("--" + long + "=")

  def exactMatch(opt: String) = shortMatch(opt) || longExact(opt)

  def mayMatch(opt: String) = shortMatch(opt) || longStart(opt)
}

object AnaArgs {
  /** extract matching options */
  def anaArgs(opts: List[OptionDescr], args: List[String]): (Map[String, OptionValue], List[String]) = {
    args match {
      case Nil => (Map.empty, Nil)
      case hd :: tl =>
        val optO = opts.find(_.mayMatch(hd))
        optO match {
          case None =>
            val (m, r) = anaArgs(opts, tl)
            (m, hd :: r)
          case Some(o) =>
            o.arg match {
              case NoArg if o.exactMatch(hd) =>
                val (m, r) = anaArgs(opts, tl)
                (m + (o.long -> NoVal), r)
              case _ if o.longEqual(hd) =>
                val v = hd.substring(o.long.length + 3)
                anaArgs(opts, "--" + o.long :: v :: tl)
              case k if o.exactMatch(hd) => tl match {
                case snd :: rt =>
                  val (m, r) = anaArgs(opts, rt)
                  k match {
                    case IntArg => Try(snd.toInt).toOption match {
                      case Some(i) if m.get(o.long).isEmpty => // trailing options win
                        (m + (o.long -> IntVal(i)), r)
                      case _ =>
                        (m, ("--" + o.long + "=" + snd) :: r)
                    }
                    case StringArg if m.get(o.long).isEmpty => // trailing options win
                      (m + (o.long -> StringVal(snd)), r)
                    case StringListArg =>
                      (m + (o.long -> StringListVal(snd ::
                        m.getOrElse(o.long, StringListVal(Nil)).getStringList)), r)
                    case _ =>
                      (m, hd :: snd :: r)
                  }
                case Nil =>
                  (Map.empty, args)
              }
              case _ =>
                val (m, r) = anaArgs(opts, tl)
                (m, hd :: r)
            }
        }
    }
  }
}

/**
  * Represents arguments parsed to MMT on the command line.
  *
  * @param help       Should we print an help text
  * @param about      Should we print an about text
  * @param send       Should we send commands to a remote port instead of running them locally?
  * @param mmtFiles   List of MMT files to load
  * @param scalaFiles List of SCALA files to load
  * @param cfgFiles   List of config files to load
  * @param commands   List of commands to run
  * @param prompt     Should we start a prompt?
  * @param runCleanup Should we cleanup running threads and exit after commands have been processed
  */
case class ShellArguments(
                           help: Boolean,
                           about: Boolean,
                           send: Option[Int],
                           mmtFiles: List[String],
                           scalaFiles: List[String],
                           cfgFiles: List[String],
                           commands: List[String],
                           prompt: Boolean, // run interactive shell
                           runCleanup: Boolean // do not keep alive but terminate/cleanup processes
                         )

object ShellArguments {

  private val toplevelArgs: List[OptionDescr] = List(
    OptionDescr("help", "h", NoArg, "command line help"),
    OptionDescr("about", "a", NoArg, "about the program"),
    OptionDescr("shell", "i", NoArg, "start an interactive shell"),
    OptionDescr("keepalive", "w", NoArg, "wait for processes to finish"),
    OptionDescr("noshell", "w", NoArg, "same as keepalive"),
    OptionDescr("send", "r", IntArg, "send commands to remote port"),
    OptionDescr("mbt", "m", StringListArg, "mbt input file "),
    OptionDescr("file", "f", StringListArg, "msl input file"),
    OptionDescr("cfg", "c", StringListArg, "config input file")
  )

  def parse(arguments: List[String]): Option[ShellArguments] = {
    val (m, cs) = AnaArgs.anaArgs(toplevelArgs, arguments)
    val helpFlag = m.get("help").isDefined
    val aboutFlag = m.get("about").isDefined
    val os = m.get("keepalive").toList ++ m.get("shell").toList ++ m.get("noshell").toList
    val sa = ShellArguments(
      help = helpFlag,
      about = aboutFlag,
      send = m.get("send").map(a => a.asInstanceOf[IntVal].value),
      mmtFiles = m.getOrElse("file", StringListVal(Nil)).getStringList,
      scalaFiles = m.getOrElse("mbt", StringListVal(Nil)).getStringList,
      cfgFiles = m.getOrElse("cfg", StringListVal(Nil)).getStringList,
      commands = cs,
      prompt = m.get("shell").isDefined,
      runCleanup = m.get("keepalive").isEmpty && m.get("noshell").isEmpty)
    val fs = sa.mmtFiles ++ sa.scalaFiles ++ sa.cfgFiles
    if (helpFlag && aboutFlag) {
      println("atmost one of --help and --about arguments can be used.")
      None
    }
    else if (os.length > 1) {
      println("Atmost one of --shell, --noshell and --keepalive arguments can be used.")
      None
    } else {
      var fail = false
      fs.foreach { f =>
        val path = Paths.get(f)
        if (!Files.isRegularFile(path)) {
          println("Argument " + f + " cannot be used: " + path.toString + " is not a file.")
          fail = true
        }
      }
      if (fail) None else Some(sa)
    }
  }
}
