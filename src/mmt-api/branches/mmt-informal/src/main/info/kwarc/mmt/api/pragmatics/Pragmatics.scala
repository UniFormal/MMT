package info.kwarc.mmt.api.pragmatics

import info.kwarc.mmt.api._
import objects._
import modules._
import frontend._
import libraries._

import objects.Conversions._

class Pragmatics(controller: Controller) {
   private lazy val lup = controller.globalLookup // must be lazy due to order of class initialization
   private lazy val prags = controller.extman.pragmaticConstructors
   
   def makeStrict(level: Option[MPath], op: GlobalName, subs: Substitution, con: Context, args: List[Term], attrib: Boolean)(implicit newUnkwown: () => Term) : Term = {
      lazy val default = ComplexTerm(op, subs, con, args) 
      level match {
         case None => default
         case Some(l) =>
            prags.find(_.level == l) match {
               case None =>
                  lup.getTheory(l) match {
                     case t: DeclaredTheory => makeStrict(t.meta, op, subs, con, args, attrib)
                     case t: DefinedTheory => default 
                  }
               case Some(pc) =>
                  pc.makeStrict(op, subs, con, args, attrib)
            }
      }
   }
   
   /** the default treatment for application-like constructions without a known operator
    *  used for: apply meta-variables to its dependent bound variables; whitespace operator
    *  @return currently the OMA formed using the first apply operator found; should be cleaned up
    */
   def defaultApplication(level: Option[MPath], fun: Term, args: List[Term]): Term = {
      lazy val default = OMA(fun, args) 
      level match {
         case None => default
         case Some(l) =>
            prags.find(_.level == l) match {
               case None =>
                  lup.getTheory(l) match {
                     case t: DeclaredTheory => defaultApplication(t.meta, fun, args)
                     case t: DefinedTheory => default
                  }
               case Some(pc) =>
                  OMA(OMS(pc.apply), fun::args)
            }
      }
   }
   
   def makePragmatic(t: Term) : List[PragmaticTerm] = {
      t match {
         case ComplexTerm(op, sub, con, args) =>
            val default = PragmaticTerm(op, sub, con, args, false, Position.positions(t))
            prags.find(_.apply == op) match {
               case None => List(default)
               case Some(pc) => default :: pc.makePragmatic(t) 
            }
         case _ => Nil
      }
   }
   def mostPragmatic(t: Term) : Term = {
      makePragmatic(t).headOption match {
         case Some(h) => h.term
         case _ => t
      }
   }
 
  /** provides constructor/pattern-matching for pragmatic applications, independent of strictification */ 
  object StrictOMA {
     /**
      * @param t the matched term
      * @return tuple of
      *   the list of strict application symbols (outermost first),
      *   the pragmatic operator,
      *   the arguments
      * post-inverse of apply
      */
     def unapply(t: Term): Option[(List[GlobalName],GlobalName,List[Term])] = t match {
        case OMA(OMS(s), hd::tl) =>
           if (prags.exists(_.apply == s)) {
              unapply(OMA(hd,tl)) match {
                 case None => Some((Nil, s, hd::tl))
                 case Some((str, fun, args)) => Some((s::str, fun, args))
              }
           } else
              Some((Nil, s, hd::tl))
        case _ => None
     }
     /**
      * @param strictApps the strict application symbols to wrap around the application
      * @param fun the operator
      * @param args the arguments
      * @return intuitively: OMA(strictApps, fun, args)
      * pre-inverse of unapply
      */
     def apply(strictApps: List[GlobalName], fun: GlobalName, args: List[Term]) = strictApps match {
        case hd::tl => OMA(OMS(hd), tl.map(OMS(_)) ::: OMS(fun) :: args)
        case Nil => OMA(OMS(fun), args)
     }
  }
  
  object StrictTyping {
     def unapply(t: Term): Option[Term] = t match {
        case OMA(OMS(s), List(tp)) if prags.exists(_.typeAtt == s) => Some(tp)
        case _ => None
     }
  }
}


case class PragmaticTerm(op: GlobalName, subs: Substitution, con: Context, args: List[Term], attribution: Boolean, pos: List[Position]) {
   def term = ComplexTerm(op, subs, con, args)
}

/**
 * OMA(apply, op, args)) <--> OMA(op, args)
 * 
 * OMA(apply, op, OMBIND(bind, context, args)) <--> OMBIND(op, context, args)
 * 
 * x: OMA(typeAtt, tp) <--> x: tp
 */
class PragmaticConstructor(val level: MPath, val apply: GlobalName, bind: GlobalName, val typeAtt: GlobalName) {
   private def application(op: GlobalName, args: List[Term])(implicit newUnknown: () => Term) : Term =
      OMA(OMS(apply), OMS(op)::args)
   def makeStrict(op: GlobalName, subs: Substitution, con: Context, args: List[Term], attrib: Boolean)(implicit newUnknown: () => Term) : Term =
      if (attrib) {
         val ptp = if (subs.isEmpty && con.isEmpty && args.isEmpty)
            OMS(op)
         else
            makeStrict(op, subs, con, args, false)
         OMA(OMS(typeAtt), List(ptp))
      } else {
         // for now: strict form treats substitution as extra arguments
         val subargs = subs.map(_.target)
         if (con.isEmpty)
            application(op, subargs ::: args)
        else
          application(op, subargs ::: List(OMBINDC(OMS(bind), con, args)))
      }

   def makePragmatic(t: Term): List[PragmaticTerm] = t match {
      case OMA(OMS(this.apply), OMS(op)::rest) =>
         val appPos = (0 until rest.length+1).toList.map(i => Position(i+1))
         val appTerm = PragmaticTerm(op, Substitution(), Context(), rest, false, appPos)
         rest.reverse match {
            case OMBINDC(OMS(this.bind), con, args) :: _ =>
               // last argument is binder
               val subs = rest.init.map(a => Sub(OMV.anonymous, a))
               val bindPos = Position(1) :: (0 until con.length+args.length).toList.map(i => Position(rest.length+1) / (i+1))  
               val bindTerm = PragmaticTerm(op, subs, con, args, false, bindPos) 
               List(appTerm, bindTerm)
            case _ => List(appTerm)
         }
      case OMA(OMS(this.typeAtt), List(tp)) => tp match {
         case OMS(op) =>
            List(PragmaticTerm(op, Substitution(), Context(), Nil, true, List(Position(1))))
         case OMA(OMS(this.apply), OMS(op)::rest) =>
            List(PragmaticTerm(op, Substitution(), Context(), rest, true, (0 until rest.length+1).toList.map(i => Position(1) / (i+1))))
      }
      case _ => Nil
   }
}