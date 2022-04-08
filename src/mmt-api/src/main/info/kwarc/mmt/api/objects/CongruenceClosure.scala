package info.kwarc.mmt.api.objects
import Conversions._

/**
 * tries to reduce an equality to a list of equalities by applying congruence and alpha-renaming
 * no context-specific reasoning or simplification is applied
 * @param assume callback that is tested before recursing
 *
 * invariant: Conjunction(CongrunceClosure(eq))  ===>  eq
 */
class CongruenceClosure(assume: Equality => Option[Boolean]) {
  def apply(eq: Equality): Option[List[Equality]] = {
     val eqs = aux(eq.context, eq.tm1, eq.tm2)
     if (eqs == List(eq))
       None else Some(eqs)
  }

  private def aux(cont: Context, t1: Term, t2: Term): List[Equality] = {
    val eq = Equality(Stack(cont), t1, t2, None)
    val noMatch = List(eq)
    assume(eq) match {
      case Some(true) => return Nil
      case Some(false) => return noMatch
      case None =>
    }
    (t1, t2) match {
         case (OMV(x1),OMV(x2)) =>
           if (x1 == x2) Nil else noMatch
         case (OMID(p1),OMID(p2)) =>
           if (p1 == p2) Nil else noMatch
         case (l1: OMLIT, l2: OMLIT) =>
           //TODO match types, open problem: how to handle subtyping of literals
           if (l1.value == l2.value) Nil else noMatch
         case (OMA(f1, args1), OMA(f2,args2)) =>
            if (args1.length != args2.length || f1 != f2) {
              noMatch
            } else ((f1 :: args1) zip (f2 :: args2)) flatMap {
               case (x,y) => aux(cont, x,y)
            }
         // recurse into components, keeping track of variables
         case (OMBINDC(b1, bound1, sc1), OMBINDC(b2, bound2, sc2)) =>
            if (b1 != b2 || bound1.length != bound2.length || sc1.length != sc2.length) {
              noMatch
            } else {
              val bM = aux(cont,b1,b2)
              val (boundM,rename) = auxCon(cont,bound1,bound2).getOrElse {
                return noMatch
              }
              val sM = (sc1 zip sc2) flatMap {
                case (s1,s2) => aux(cont ++ bound1,s1,s2 ^? rename)
              }
              bM ::: boundM ::: sM
            }
         case (OML(n1,t1,d1,_,f1), OML(n2,t2,d2,_,f2)) =>
           if (n1 != n2 || f1 != f2)
             noMatch
           else List((t1,t2),(d1,d2)).flatMap {
             case (Some(x1),Some(x2)) => aux(cont,x1,x2)
             case (None,None) => Nil
             case _ => return noMatch
           }
         // all asymmetric combinations
         case (t1,t2) =>
           if (t1 hasheq t2) Nil else noMatch
      }
   }
   /**
    * like aux but for contexts: We match for context, queryVars, bound |- goal = query : Ctx
    * @param goal a context
    * @param query a context with query variables to match against goal
    * @param context joint free variables of goal and query
    * @return if goal and query match up to alpha-renaming, the substitution query -> goal that performs the alpha-renaming
    *
    * terms occurring inside query are alpha-renamed accordingly
    */
   private def auxCon(cont: Context, c1: Context, c2: Context): Option[(List[Equality],Substitution)] = {
      val noMatch = None
      if (c1.length != c2.length) return noMatch
      var rename = Substitution()
      var eqs: List[Equality] = Nil
      (c1.declsInContext.toList zip c2) foreach {
         case ((c1Bound, VarDecl(x1,f1,tp1,df1, _)), VarDecl(x2,f2,tp2, df2, _)) =>
            if (f1 != f2) return noMatch
            List((tp1,tp2), (df1,df2)) foreach {
               case (None,None) => Nil
               case (Some(t1), Some(t2)) =>
                  eqs :::= aux(cont ++ c1Bound, t1, t2 ^? rename)
               case _ => return noMatch
            }
            rename ++= (x2/OMV(x1))
      }
      Some((eqs, rename))
   }
}

/** trivial case without any assumptions */
object CongruenceClosure extends CongruenceClosure(_ => None)
