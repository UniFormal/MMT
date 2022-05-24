package info.kwarc.mmt.api.proving.itp

/**
  * the returnmessgae of a tactic when executed
  */
trait Msg {
  def combineMsg(m : Msg): Msg
}

/**
  * error message
  * @param s
  */
case class HasError(s : String ) extends Msg {
  override def combineMsg(m: Msg): Msg = this

    /*m match {
    case HasError(st) => HasError(s ++ "\n" ++ st)
    case WarningMsg(st) => HasError(s ++ "\n" ++ st)
    case NoMsg() => this
  }

     */
}

/**
  * warning message
  * @param s
  */
case class WarningMsg(s : String) extends Msg {
  override def combineMsg(m: Msg): Msg = m match {
    case HasError(st) => HasError(s ++ "\n" ++ st)
    case WarningMsg(st) => WarningMsg(s ++ "\n" ++ st)
    case NoMsg() => this
  }
}

/**
  * plain message (i.e. no error or warning)
  * @param s
  */
case class SimpleMsg(s : String) extends Msg {
  override def combineMsg(m: Msg): Msg = m match {
    case HasError(st) => HasError(s ++ "\n" ++ st)
    case WarningMsg(st) => WarningMsg(s ++ "\n" ++ st)
    case NoMsg() => this
  }
}

/**
  *  no message at all (i.e. no warning or error)
  */
case class NoMsg() extends Msg {
  override def combineMsg(m: Msg): Msg = m match {
    case HasError(st) => HasError(st)
    case WarningMsg(st) => WarningMsg(st)
    case NoMsg() => this
  }
}