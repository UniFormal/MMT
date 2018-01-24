package info.kwarc.mmt.api.utils

/** describe the kind of a command line option */
sealed abstract class OptionArgument

/** just a flag */
case object NoArg extends OptionArgument

/** expect an integer argument */
case object IntArg extends OptionArgument

/** expect an optional integer argument that must not be given as a separate argument */
case object OptIntArg extends OptionArgument

case object StringArg extends OptionArgument

/** collect multiple values */
case object StringListArg extends OptionArgument

/** the possible values given by options */
sealed abstract class OptionValue {
  /** convenience methods */
  def getStringList: List[String] = this.asInstanceOf[StringListVal].value

  def getStringVal: String = this.asInstanceOf[StringVal].value

  def getIntVal: Int = this.asInstanceOf[IntVal].value
}

case object NoVal extends OptionValue

case class IntVal(value: Int) extends OptionValue

case class StringVal(value: String) extends OptionValue

case class StringListVal(value: List[String]) extends OptionValue

/** a description of one option */
case class OptionDescr(long: String, short: String, arg: OptionArgument, description: String) {
  def shortMatch(opt: String) = short.nonEmpty && "-" + short == opt

  def longExact(opt: String) = "--" + long == opt

  def longEqual(opt: String) = opt.startsWith("--" + long + "=")

  def exactMatch(opt: String) = shortMatch(opt) || longExact(opt)

  def mayMatch(opt: String) = exactMatch(opt) || longEqual(opt)
}

/** function for parsing command line arguments */
object AnaArgs {
  type OptionDescrs = List[OptionDescr]
  type OptionMap = Map[String, OptionValue]

  /** extract matching options */
  def apply(opts: OptionDescrs, args: List[String]): (OptionMap, List[String]) = {
    args match {
      case Nil => (Map.empty, Nil)
      case hd :: tl =>
        val optO = opts.find(_.mayMatch(hd))
        optO match {
          case None =>
            val (m, r) = apply(opts, tl)
            (m, hd :: r)
          case Some(o) =>
            val equalOpt = o.longEqual(hd)
            val checkTail = equalOpt || o.arg == NoArg || tl.isEmpty ||
              o.arg == OptIntArg && toOptInt(tl.head).isEmpty
            val v = if (equalOpt)
               Some(hd.substring(o.long.length + 3))
            else if (checkTail)
               None
            else
               Some(tl.head)
            val (m, r) = apply(opts, if (checkTail) tl else tl.tail)
            getOptOptionValue(m, o, v) match {
              case None => (m, hd :: (if (checkTail) r else tl.head :: r))
              case Some(e) => (m + (o.long -> e), r)
            }
        }
    }
  }

  def getStringList(m: OptionMap, arg: String): List[String] =
    m.getOrElse(arg, StringListVal(Nil)).getStringList

  private def toOptInt(s: String) = scala.util.Try(s.toInt).toOption

  private def getOptOptionValue(m: OptionMap, o: OptionDescr, value: Option[String]): Option[OptionValue] = {
    if (m.get(o.long).isDefined && o.arg != StringListArg) None // already defined
    else {
      value match {
        case None =>
          if (o.arg == NoArg || o.arg == OptIntArg) Some(NoVal) else None
        case Some(v) =>
          o.arg match {
            case OptIntArg | IntArg =>
              toOptInt(v).map(IntVal)
            case StringArg =>
              Some(StringVal(v))
            case StringListArg =>
              Some(StringListVal(v ::
                m.getOrElse(o.long, StringListVal(Nil)).getStringList))
            case _ =>
              None
          }
      }
    }
  }

  def splitOptions(args: List[String]): (List[String], List[String]) =
    args.partition(_.startsWith("-"))

  def usageMessage(optionDescrs: OptionDescrs): List[String] =
    "supported command line options:" ::
    optionDescrs.map { case OptionDescr(long, short, optArg, descr) =>
      val longOpt =
        (if (optArg == StringListArg) " (--" else "  --") +
          long + (optArg match {
        case NoArg => " "
        case IntArg => "=INT "
        case OptIntArg => "[=N] "
        case StringArg => "=STR "
        case StringListArg => "=STR)+ "
      })
      (if (short.nonEmpty) {
        "  -" + short +
          (optArg match {
          case NoArg => "    "
          case IntArg => " INT"
          case OptIntArg => " [N]"
          case _ => " STR"
        })
      }
      else "        ") + longOpt +
        List.fill(24)(" ").mkString.substring(Math.min(longOpt.length, 23)) +
        descr
    }
}
