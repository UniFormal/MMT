package info.kwarc.mmt.morphisms

import info.kwarc.mmt.api._
import objects._
import objects.Conversions._
import symbols._
import notations._
import frontend._

import info.kwarc.mmt.lf._

/**
 * elaborates each instance that is added
 *
 * instances are constant whose type is THEORY
 */
class InstanceElaborator extends ChangeListener {
  /** if th:THEORY and this returns Some((cont, List(subs1,...,subsn))), then th = cont ^ subs1 ^ ... ^ subsn */
  private def expandTheory(th: Term): Option[(Context, List[Substitution])] = th match {
     case ApplyGeneral(OMS(pat), args) =>
        // th = pat args
        controller.globalLookup.getO(pat) match {
           case Some(c: Constant) => c.tp match {
              case Some(FunType(_, TheoryType())) =>
                 // pat : {params} THEORY
                 c.df match {
                    case Some(FunTerm(params, body)) =>
                       // pat : {params} THEORY = [params] body
                       expandTheory(body) flatMap {case (cont, subss) =>
                          // body = cont ^ subs1 ^ ... ^ subsn
                          if (params.length == args.length) {
                             // th : THEORY
                             val subs: Substitution = (params zip args).map {case ((n,_),a) => Sub(n, a)}
                             // th = cont ^ subs1 ^ ... ^ subsn ^ (params/args)
                             Some((cont, subss ::: List(subs)))
                          } else
                             None
                       }
                    case _ => None
                 }
              case _ => None
           }
           case _ => None
        }
     case ComplexTheory(cont) =>
        // th : THEORY = cont
        Some((cont, Nil))
     case _ =>
        None
  }
  
  /** uses expandTheory to turn the type of a [[Constant]] into ComplexTheory(cont) ^ subs1 ^ ... ^ subsn
   *  and uses that to generate one [[ElaboratedConstant]] for each variable in cont
   */
  override def onAdd(e: ContentElement) {
      e match {
         case c: Constant =>
            c.tp foreach {tp =>
               expandTheory(tp) foreach {case (cont, subss) =>
                  cont foreach {vd =>
                     val lc = new ElaboratedConstant(c, vd, subss)
                     controller.add(lc)
                  }
               }
            }
         case _ =>
      }
   }
}

/**
 * produced and added by [[InstanceElaborator]]
 * 
 * @param instance the elaborated instance
 * @param vd the variable declaration giving rise to this Constant
 * @param subss the substitutions that have to be applied to the type/definiens of vd
 *    These are applied lazily, i.e., when they are first accessed.
 */
class ElaboratedConstant(instance: Constant, vd: VarDecl, subss: List[Substitution]) extends
       LazyConstant(instance.home, instance.name / vd.name) {
   _not = vd.not
   def onAccessTp {
      _tp = vd.tp map {tp => subss.foldLeft(tp) {case (sofar, next) => sofar ^? next}}
   }
   def onAccessDf {
      _df = vd.df map {df => subss.foldLeft(df) {case (sofar, next) => sofar ^? next}}
   }
   def onAccessOther {}
}