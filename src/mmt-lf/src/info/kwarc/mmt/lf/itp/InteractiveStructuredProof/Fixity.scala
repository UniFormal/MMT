package info.kwarc.mmt.lf.itp.InteractiveStructuredProof

trait Argpos {
  def getprio : Int
}

case class ArgL(prio : Int) extends Argpos{
  override def getprio: Int = prio
}
case class ArgR(prio : Int) extends Argpos{
  override def getprio: Int = prio
}




abstract class Fixity(val prio:Int , val  arglist : List[Argpos]) {
  def isleft : Boolean
}

case class FixityLeft(override val prio:Int, override  val arglist : List[Argpos]) extends Fixity(prio, arglist){
  override def isleft: Boolean = true
}
case class FixityRight(override val prio:Int, override  val arglist : List[Argpos]) extends Fixity(prio,arglist){
  override def isleft: Boolean = false
}

