package info.kwarc.mmt.api.objects

import info.kwarc.mmt.api._
import parser._

/**
 * first attempt at abstracting from unknowns, not used yet
 *
 * apply/unapply for unknowns (meta-variables) and the substitution for their free variables
 */
case class Unknown(unknowns: Context, bound: Context) {self =>
   def apply(v: LocalName, args: List[Term]) = OMA(OMS(ParseResult.substitute), OMV(v) :: args)
   def unapply(t: Term): Option[(LocalName,List[Term])] = t match {
     case OMA(OMS(ParseResult.substitute), OMV(v) :: args) if unknowns.isDeclared(v) =>
       Some((v,args))
     case _ =>
       None
   }
   /** matches for an unknown applied to distinct bound variables */
   object Solvable {
     def unapply(t: Term): Option[(LocalName,List[LocalName])] = self.unapply(t) flatMap {case (v,args) =>
       val names = args map {
         case OMV(n) if bound.isDeclared(n) => n
         case _ => return None
       }
       if (names.distinct.length != names.length)
         None
       else
         Some((v,names))
     }
   }
   /** builds the solution for an unknown */
   object Solution {
     def apply(names: List[LocalName], tm: Term) = OMBIND(OMS(ParseResult.free), names map {n => VarDecl(n)}, tm)
   }
 }
