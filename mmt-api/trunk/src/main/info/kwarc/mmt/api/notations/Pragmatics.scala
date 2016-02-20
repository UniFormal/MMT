package info.kwarc.mmt.api.notations

import info.kwarc.mmt.api._
import objects._
import modules._
import frontend._
import libraries._

import objects.Conversions._

class Pragmatics(controller: Controller) {
   private lazy val lup = controller.globalLookup // must be lazy due to order of class initialization
   private lazy val notExts = controller.extman.notationExtensions

   private def applicableByLevel(level: Option[MPath]): NotationExtension = {
      val applicable = notExts.filter {ne => (ne.applicableLevel, level) match {
         case (None, None) => true
         case (Some(aL), Some(l)) => controller.globalLookup.hasImplicit(OMMOD(aL), OMMOD(l))
         case _ => false
      }}
      if (applicable.isEmpty) {
         level match {
            case None => MixfixNotation
            case Some(l) =>
               lup.getTheory(l) match {
                  case t: DeclaredTheory => applicableByLevel(t.meta)
                  case t: DefinedTheory => MixfixNotation 
               }
         }
      } else {
         applicable.maxBy(_.priority) 
      }
   }
   def makeStrict(level: Option[MPath], op: GlobalName, subs: Substitution, con: Context, args: List[Term], attrib: Boolean, not: TextNotation
         )(implicit newUnkwown: () => Term) : Term = {
      applicableByLevel(level).constructTerm(op, subs, con, args, attrib, not)
   }
   
   /** the default treatment for application-like constructions without a known operator
    *  used for: apply meta-variables to its dependent bound variables; whitespace operator
    *  @return currently the OMA formed using the first apply operator found; should be cleaned up
    */
   def defaultApplication(level: Option[MPath], fun: Term, args: List[Term]): Term = {
      applicableByLevel(level).constructTerm(fun, args) 
   }
   
   def makePragmatic(t: Term)(implicit getNotations: GlobalName => List[TextNotation]) : Option[PragmaticTerm] = {
      val applicable = notExts.filter(_.isApplicable(t)).sortBy(_.priority).reverse
      applicable.foreach {ne =>
         ne.destructTerm(t).foreach {tP => return Some(tP)}
      }
      return None
   }
   
   def mostPragmatic(t: Term) : Term = makePragmatic(t)(p => presentation.Presenter.getNotations(controller, p, false)) match {
      case Some(tP) => tP.term
      case None => t
   }
 
  private def hoass = notExts.flatMap {
     case h: HOASNotation => List(h.hoas)
     case _ => Nil
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
           if (hoass.exists(_.apply == s)) {
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
        case OMA(OMS(s), List(tp)) if hoass.exists(_.typeAtt == s) => Some(tp)
        case _ => None
     }
  }
}