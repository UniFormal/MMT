package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
import libraries._
import modules._
import utils._

case class Record(fields: List[(GlobalName,Term)]){
}

object OMREC {
  val recordSymbol = utils.mmt.mmtbase ? "records" ? "record"
  def apply(r : Record) : Term =  r match {
    case Record(fields) =>
      fields match {
        case (k,v)::l => OMATTR(apply(Record(l)), OMID(k), v)
        case Nil => OMID(recordSymbol)
      }
  }
  def unapply(m : Term) : Option[Record] = m match {
    case OMATTR(arg, OMID(key:GlobalName), value) =>
      unapply(arg) match {
          case Some(Record(fields)) => Some(Record((key, value)::fields))
          case None => None
      }
    case OMID(this.recordSymbol) => Some(Record(Nil))
    case _ => None
  }
}
