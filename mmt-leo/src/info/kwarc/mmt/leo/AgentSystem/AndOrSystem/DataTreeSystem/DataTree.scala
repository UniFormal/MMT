package info.kwarc.mmt.leo.AgentSystem.AndOrSystem.DataTreeSystem

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.leo.AgentSystem.AndOrSystem.AndOr


/**
 * Created by Mark on 7/21/2015.
 * This trait represents an and/or tree with data storage capabilities.
 */
trait Data[D,T<:Data[D,T]] extends AndOr[T] { self: T =>
  override def logPrefix = "DataTree"

  /** the data stored at each node*/
  var data: D

  /** update the sata and status of each node*/
  def update(dataVar:D,conjVar: Boolean, isSatVar: Option[Boolean]) ={
    this.data = dataVar
    this.conj = conjVar
    this.sat = isSatVar
  }

  /*  def mkData[B,U<:Data[B,U]](dataVar:B, conjVar:Boolean, isSatVar:Option[Boolean]): U ={
      new U{var data = dataVar
        var conj = conjVar
        var isSat = isSatVar
        }
    }

    /** maps a tree of one data-type to a tree of another*/
    def map[B,U<:Data[B,U]](f: D => B): U = {
      val fThis = mkData(f(this.data),this.conj,this.isSat)
      def recur(n : Data[D,T],fn: U): Unit = {
        for (r <- n.children) {
          val addition = mkData(f(r.data),r.conj,r.isSat)
          fn.addChild(addition)
          recur(r,addition)
        }
      }
      recur(this,fThis)
      fThis
    }*/

}

class DataTree[D](dataVar: D,conjVar:Boolean, isSatVar: Option[Boolean]=None)(implicit controller: Controller)
  extends Data[D,DataTree[D]] {

     var data = dataVar
     var conj = conjVar
     var sat = isSatVar

     def apply(dataVar:D,conjVar: Boolean, isSatVar: Option[Boolean]) ={
       this.data = dataVar
       this.conj = conjVar
       this.sat = isSatVar
     }

     def unapply[B](t:DataTree[B]) = {
       Some((t.data,t.conj, t.sat))
     }

     override def present: String ={
       "\t"*depth +"data: "+data + " isAnd: "+isAnd.toString+" isSatisfiable: "+sat.toString
     }

   }
