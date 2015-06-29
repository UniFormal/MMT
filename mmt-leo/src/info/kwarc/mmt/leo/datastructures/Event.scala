package info.kwarc.mmt.leo.datastructures


/**
 * Created by mark on 6/27/15.
 */
case class Event[A](node : ProofTree[A], flags: List[String]){
  def getNode : ProofTree[A] = node
  def hasFlag(f:String): Boolean = flags.contains(f)
  def hasFlag(l:List[String]): Boolean = l.exists(flags.contains(_))
  var readBy: List[Agent[A]]=Nil
  def wasReadBy(a: Agent[A]): Boolean = readBy.contains(a)
}


