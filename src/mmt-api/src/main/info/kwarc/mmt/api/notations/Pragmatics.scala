package info.kwarc.mmt.api.notations

import info.kwarc.mmt.api._
import objects._
import modules._
import frontend._
import libraries._
import symbols._

import objects.Conversions._

class Pragmatics extends ChangeListener {
   private lazy val lup = controller.globalLookup // must be lazy due to order of class initialization

   /** caches (via change-listening) all known NotationExtensions */
   private var notExts: List[(MPath, NotationExtension)] = Nil

   override def onAdd(se: StructuralElement): Unit = {
     se match {
       case rc: RuleConstant => rc.df.foreach {
         case ne: NotationExtension =>
           notExts ::= (rc.home.toMPath, ne)
         case _ =>
       }
       case _ =>
     }
   }
   override def onDelete(se: StructuralElement): Unit = {
     se match {
       case rc: RuleConstant => rc.df.foreach {
         case ne: NotationExtension =>
           notExts = notExts.filterNot {case (_,neC) => ne == neC}
         case _ =>
       }
       case _ =>
     }
   }
   override def onClear: Unit = {
     notExts = Nil
   }

   /** a NotationExtension is applicable at level mp if it is visible to mp */
   private def applicableByLevel(levelOpt: Option[MPath]): NotationExtension = {
      val level = levelOpt getOrElse {return MixfixNotation}
      val applicable = notExts.flatMap {case (thy, ne) =>
        if (controller.globalLookup.hasImplicit(OMMOD(thy), OMMOD(level)))
          List(ne)
        else
          Nil
      }
      if (applicable.isEmpty)
        MixfixNotation
      else
        applicable.maxBy(_.priority)
   }
   def makeStrict(level: Option[MPath], op: GlobalName, subs: Substitution, con: Context, args: List[Term], not: TextNotation)(implicit newUnkwown: () => Term) : Term = {
      applicableByLevel(level).constructTerm(op, subs, con, args, not)
   }

   /**
    * the default treatment for application-like constructions without a known operator
    * used for: apply meta-variables to its dependent bound variables; whitespace operator; application of MPath
    *
    * @return as dictated by notation extension, OMA by default
    */
   def defaultApplication(level: Option[MPath], fun: Term, args: List[Term]): Term = {
      fun match {
        case OMMOD(_) => OMA(fun, args)
        case _ => applicableByLevel(level).constructTerm(fun, args)
      }
   }

   def makePragmatic(t: Term)(implicit getNotations: GlobalName => List[TextNotation]) : Option[PragmaticTerm] = {
      var applicable = (MixfixNotation::notExts.map(_._2)).filter(_.isApplicable(t)).sortBy(r => -r.priority)
      applicable.foreach { ne =>
         ne.destructTerm(t).foreach {tP => return Some(tP)}
      }
      return None
   }

   def mostPragmatic(t: Term) : Term = makePragmatic(t)(p => presentation.Presenter.getNotations(controller, p, false)) match {
      case Some(tP) => tP.term
      case None => t
   }

  private def hoass = notExts.flatMap {
     case (_, h: HOASNotation) => List(h.hoas)
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
      * @param fun the operator (may be Term, contrary to unapply method)
      * @param args the arguments
      * @return intuitively: OMA(strictApps, fun, args)
      * pre-inverse of unapply
      */
     def apply(strictApps: List[GlobalName], fun: Term, args: List[Term]) = strictApps match {
        case hd::tl => OMA(OMS(hd), tl.map(OMS(_)) ::: fun :: args)
        case Nil => OMA(fun, args)
     }
  }
}

/** apply/unapply for a pragmatic OMA under a given list of HOAS apply operators */
class OMAUnder(under: List[GlobalName]) {
  private val underL = under.length
  private val underT = under.map(OMS(_))
  def apply(f: Term, args: List[Term]) = {
    val comps = underT ::: f :: args
    OMA(comps.head, comps.tail)
  }
  def unapply(t: Term): Option[(Term,List[Term])] = t match {
    case OMA(f,uargs) if (f::uargs).startsWith(underT) =>
      val comps = (f::uargs).drop(underL)
      Some((comps.head, comps.tail))
    case _ => None
  }
}