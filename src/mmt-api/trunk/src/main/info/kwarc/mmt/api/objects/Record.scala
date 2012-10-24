package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
import libraries._
import modules._
import utils._

case class Record(fields: List[(LocalName,Term)]){
}

object OMREC {
  val recordkey = utils.mmt.mmtbase ? "recordlabel"
  val recordSymbol = utils.mmt.mmtbase ? "records" ? "record"
  def apply(r : Record) : Term =  r match {
    case Record(fields) =>
      fields match {
        case (k,v)::l => OMATTR(apply(Record(l)), OMID(recordkey ? k), v)
        case Nil => OMID(recordSymbol)
      }
  }
  def unapply(m : Term) : Option[Record] = m match {
    case OMATTR(arg, key, value) => key.path match {
      case GlobalName(OMMOD(this.recordkey), name) =>
        unapply(arg) match {
          case Some(Record(fields)) => Some(Record((name, value)::fields))
          case None => None // should be illegal
        }
      case _ => None
    }
    case OMID(this.recordSymbol) => Some(Record(Nil))
    case _ => None
  }
}
