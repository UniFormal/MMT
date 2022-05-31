package info.kwarc.mmt.lf.itp.Utils

import info.kwarc.mmt.api.LocalName
import info.kwarc.mmt.api.objects.Obj.getConstants
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.lf.{Apply, ApplySpine, Arrow, Pi}

object TermUtil {
  def getTerm(tt: Term, ninit: Int): (Term => Term, Term) = {
    val currprevini =  (x: Term) => x
    val currini: Term = tt
    def getTermLoop(currprev : Term => Term , curr : Term , n : Int): (Term => Term , Term) =  {
      if (n == 0) return (currprev, curr)
      curr match {
        case Arrow(h, tl) => {
          val tmp = (x: Term) => currprev(Arrow(h, x))
          getTermLoop(tmp , tl , n - 1)
        }
        case Pi(nm, tp, bd) => {
          val tmp = (x: Term) => currprev(Pi(nm, tp, x))
          getTermLoop(tmp, bd, n - 1)
        }
        case v => (currprev, curr)
      }


    }
    getTermLoop(currprevini , currini , ninit)

  }

  def getTermSafe(tt: Term, ninit: Int): Option[(Term => Term, Term)] = {
    val currprevini =  (x: Term) => x
    val currini: Term = tt
    def getTermLoop(currprev : Term => Term , curr : Term , n : Int): Option[(Term => Term , Term)] =  {
      if (n == 0) return Some((currprev, curr))
      curr match {
        case Arrow(h, tl) => {
          val tmp = (x: Term) => currprev(Arrow(h, x))
          getTermLoop(tmp , tl , n - 1)
        }
        case Pi(nm, tp, bd) => {
          val tmp = (x: Term) => currprev(Pi(nm, tp, x))
          getTermLoop(tmp, bd, n - 1)
        }
        case v => None
      }


    }
    getTermLoop(currprevini , currini , ninit)

  }

  def getTermByName(tt: Term, l: LocalName, pos: Int): Option[(Term, Int)] = tt match {
    case Arrow(h, tl) => getTermByName(h, l, pos + 1)
    case Pi(nm, tp, bd) => {
      if (nm == l) {
        Some(tt, pos)
      } else {
        getTermByName(bd, l, pos + 1)
      }
    }
    case _ => None
  }


  def getTermByNameWithExternalBinders(tt: Term, l: LocalName, pos: Int): Option[(Term, Int)] = tt match {
    case Arrow(h, tl) => getTermByName(h, l, pos + 1)
    case Pi(nm, tp, bd) => {
      if (nm == l) {
        Some(tt, pos)
      } else {
        getTermByNameWithExternalBinders(bd, l, pos + 1)
      }
    }
    case _ => None
  }

  def termlength(tt: Term): Int = tt match {
    case Arrow(_, tl) => {
      termlength(tl) + 1
    }
    case Pi(_, _, bd) => {
      termlength(bd) + 1
    }
    case _ => 1
  }

  def genDeps(ttt: Term): List[Set[(LocalName, Term, Int)]] = {
    def loopGenDeps(tt: Term, vars: Set[(LocalName, Term, Int)], pos: Int): List[Set[(LocalName, Term, Int)]] = tt match {
      case Arrow(h, tl) => {
        val tmp = loopGenDeps(tl, vars, pos + 1)
        vars :: tmp
      }
      case Pi(nm, tp, bd) => {
        val newelm: (LocalName, Term, Int) = (nm, tp, pos)
        val tmp = loopGenDeps(bd, vars + newelm, pos + 1)
        vars :: tmp
      }
      case trm => List(vars)
    }

    loopGenDeps(ttt, Set.empty, 0)
  }


  def genDepsSmart(ttt: Term): List[Set[(LocalName, Term, Int)]] = {
    def loopGenDepsSmart(tt: Term, vars: Set[(LocalName, Term, Int)], pos: Int): List[Set[(LocalName, Term, Int)]] = tt match {
      case Arrow(h, tl) => {
        val tmp = loopGenDepsSmart(tl, vars, pos + 1)
        vars.filter(x => h.freeVars.contains(x._1)) :: tmp
      }
      case Pi(nm, tp, bd) => {
        val newelm: (LocalName, Term, Int) = (nm, tp, pos)
        val newvars = vars.filter(p => p._1 != nm)
        val tmp = loopGenDepsSmart(bd, newvars + newelm, pos + 1)
        vars.filter(x => tp.freeVars.contains(x._1)) :: tmp
      }
      case trm => List(vars.filter(v => trm.freeVars.contains(v)))
    }

    loopGenDepsSmart(ttt, Set.empty, 0)
  }


  def toBinaryRelation(t : Term, rel : Option[Term] = None ) : Term ={
    t match{
      case OMA(OMID(Apply.path) , List(rel,a,b)) => t
      case OMA(OMID(Apply.path) , rel:: x::xs) => {
        toBinaryRelation(ApplySpine(Apply(rel ,  x) , xs : _* ))
      }
    }
  }


  def arrowify(t : Term) : Term = t match {
    case Arrow(h , tl) => Arrow(h , arrowify(tl))
    case Pi(nm , tp , bd) => {
      if (bd.freeVars.contains(nm)){
        Pi(nm , tp , arrowify(bd))
      }else {
        Arrow(tp, arrowify(bd) )
      }
    }
    case _ => t
  }

  class ExternalBinder(val position : Int)



/*
  def genDepsSmartWithExternalBinders(ttt: Term , extbind : List[(LocalName, Term)]): (List[Set[(LocalName, Term, Either[Int, ExternalBinder])]] , List[(LocalName,Term,ExternalBinder)]) = {
    def loopGenDepsSmartWithExternalBinders(tt: Term, vars: Set[(LocalName, Term, Either[Int,ExternalBinder ])], pos: Int): List[Set[(LocalName, Term, Either[Int,ExternalBinder ])]] = tt match {
      case Arrow(h, tl) => {
        val tmp = loopGenDepsSmartWithExternalBinders(tl, vars, pos + 1)
        vars.filter(x => h.freeVars.contains(x._1)) :: tmp
      }
      case Pi(nm, tp, bd) => {
        val newelm: (LocalName, Term, Either[Int,ExternalBinder ]) = (nm, tp, Left(pos) : Either[Int, ExternalBinder ])
        val newvars = vars.filter(p => p._1 != nm)
        val tmp = loopGenDepsSmartWithExternalBinders(bd, newvars + newelm, pos + 1)
        vars.filter(x => tp.freeVars.contains(x._1)) :: tmp
      }
      case trm => List(vars.filter(v => trm.freeVars.contains(v)))
    }
    val actualexternalbinders = extbind.foldLeft(List[(LocalName,Term, Either[Int,ExternalBinder])]() , 0)((f ,curr) => ((curr._1 , curr._2 , Right(new ExternalBinder(f._2))) :: f._1 , f._2 + 1))._1.filter(ff => ttt.freeVars.contains(ff._1))
    val res = loopGenDepsSmartWithExternalBinders(ttt, Set.from(actualexternalbinders.foldLeft(List[(LocalName,Term, Either[Int,ExternalBinder])]() , 0)((f ,curr) => ((curr._1 , curr._2 , Right(new ExternalBinder(f._2))) :: f._1 , f._2 + 1))._1 ), 0)
    ( res , actualexternalbinders.map(pp => pp.copy(_3 = pp._3.asInstanceOf[Right].value.asInstanceOf[ExternalBinder])) )
  }

 */
/*
  def getHyps(t : Term) : List[Term] = {

  }


 */
  def getConclusion(t : Term) : Term = t match {
    case Arrow(h , tl) => getConclusion(tl)
    case Pi(nm , tp , bd) => getConclusion(bd)
    case _ => t
  }

  def nextBinders(t : Term) : List[LocalName] = t match {
    case Arrow(h,tl) => nextBinders(tl)
    case Pi(nm , tp , bd) => {
      val res = nextBinders(bd)
      nm::res

    }
    case _ => Nil
  }



  def binderRenaming(t : Term, lctx : Context ,  prev : List[LocalName]) : Term = t match {
    case Arrow(h , tl) => Arrow(h , binderRenaming(tl , lctx , prev))
    case Pi(nm , tp , bd) => {
      val nxtbinders = nextBinders(bd).map(f => VarDecl(f))
      val (ln , su) = Context.pickFresh( lctx ++ Context.list2context(prev.map(x => VarDecl(n = x)))  ++ Context.list2context(nxtbinders), nm)
      val newrestterm = bd.substitute(su)(PlainSubstitutionApplier)
      val res = binderRenaming(newrestterm, lctx , nm :: prev)
      Pi(ln , tp, res)
    }
    case _ => t
  }

  def getNotLocallyBoundNames(t : Term) = getConstants(t).map(_.name) ++ t.freeVars
  /*
  def binderRenaiming(t : Term , lctx : Context) : Term =  {
    val nms = getNotLocallyBoundNames()
  }

   */
/*
  def getNotLocallyBoundNames (t : Term, binders : List[LocalName]) : List[LocalName] = t match {
    case OMV(name) => {
      if (binders.contains(name)) {
        Nil
      } else {
        List(name)
      }
    }
    case Arrow(h , tl) =>  {
      val res = getNotLocallyBoundNames(h , binders)
      val res0 = getNotLocallyBoundNames(tl , binders)
      res ++ res0
    }
    case Pi(nm , tp  , bd  ) => {
      getNotLocallyBoundNames(bd , nm :: binders)
    }
    case OMID(path) => List(path.name)
    case _ => Nil
  }

 */
}
