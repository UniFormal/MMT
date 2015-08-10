package scala

object Worksheet {
  val graphs: List[(String,Int)] = List(("rel1",4))

  def hasRelation(string: String) ={
    graphs.exists{
      case (s,_) if s==string => true
      case _ => false
    }
  }


  hasRelation("rel1")
  hasRelation("rel2")
}

