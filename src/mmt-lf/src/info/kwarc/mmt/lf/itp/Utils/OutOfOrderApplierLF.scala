package info.kwarc.mmt.lf.itp.Utils

import info.kwarc.mmt.api.LocalName
import info.kwarc.mmt.api.objects.{Context, Term}
import info.kwarc.mmt.lf.{Arrow, FunType, Pi}

import scala.collection.mutable.{Map => mmap}


/*
object OutOfOrderApplierLF {

  def alpharename(t : Term , ctx : Context) : Term = {
    val FunType(args , conc) = t

    def alpharenameL(ags : List[(Option[LocalName]  , Term )], lctx  :Context) = ags match {
      case Nil =>
      case (Some(nm) , tt):: xs => {

      }
      case (None , tt)::xs => {

      }
    }
  }


  def forward(t : Term, ctx : Context ) = {
    val FunType(args , conc) = t

    val mp : mmap[(Int , LocalName), Int] = mmap()

    def alpharename(ags : List[(Option[LocalName] , Term)] , pos : Int): Unit = ags match {
      case Nil => ()
      case (Some(nm) , tt )::xs => {

      }
      case (None, tt)::xs => {

      }
    }

  }
}

 */
/*
  def deconstructLFTerm(t : Term) = t match {

  }


 */
  /*
  def mkRefs(t : Term, g : Term => Term, s : Term => (Term => Term)  , b : Box[Term]): List[Ref[Term]] = t match {
    case Arrow(h,tl) => {
      val news = { x : Term =>{
        y : Term => {
          g(x) match { case Arrow(hh,tt) => s(x)(Arrow(hh, y))}
        }
      }}

      val tmp = mkRefs(tl , {x : Term => g(x) match {case Arrow(hh,tt) => tt }}, news , b)
      val newss = {x : Term => b.v = s(b.v)(x)}
      new Ref(g(b.v) ,newss) :: tmp
    }
    case Pi(nm , tp , bd) => {
      val news = { x : Term =>{
        y : Term => {
          g(x) match { case Pi(nmm, tpp ,bdd) => s(x)(Pi(nmm, tpp, y))}
        }
      }}

      val tmp = mkRefs(bd , {x : Term => g(x) match {case Pi(nmm,tpp, bdd) => bdd }}, news , b)
      val newss = {x : Term => b.v = s(b.v)(x)}
      new Ref(g(b.v) ,newss) :: tmp
    }
    case _ => {
      val newss = {x : Term => b.v = s(b.v)(x)}
      List(new Ref(g(b.v) , newss))
    }
  }



  def makedeps() : Map[Ref[Term], List[Ref[Term]]] = {

  }


}

   */
