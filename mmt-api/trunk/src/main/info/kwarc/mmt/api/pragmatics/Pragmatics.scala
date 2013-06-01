package info.kwarc.mmt.api.pragmatics

import info.kwarc.mmt.api._
import objects._
import modules._
import frontend._
import libraries._

class Pragmatics(controller: Controller) {
   private val ps = controller.extman.pragmaticStore
   private lazy val lup = controller.globalLookup // must be lazy due to order of class initialization
   def strictApplication(theory: MPath, fun: Term, args: List[Term], includeSelf: Boolean = false) : Term = {
      //called if we continue by trying the meta-theory
      def tryMeta: Term = {
         lup.getTheory(theory) match {
            case d: DeclaredTheory => d.meta match {
               case None => OMA(fun, args)
               case Some(meta) => strictApplication(meta, fun, args, true)
            }
            case d: DefinedTheory => OMA(fun, args) //TODO what to do here?
         }
      }
      if (includeSelf) {     
         ps.getApplication(theory) match {
            case Some(a) =>
               strictApplication(theory, OMID(a.apply), fun :: args)
            case None =>
               tryMeta
         }
      } else tryMeta
   }
   def strictAttribution(theory: MPath, key: Term, value: Term): Term = {
      lup.getTheory(theory) match {
         case d: DeclaredTheory => d.meta match {
            case None => value // not returning OMA(key, List(value)) here means the outermost key (e.g., the : is dropped)
            case Some(meta) =>
               ps.getTyping(meta) match {
                  case None =>
                     strictAttribution(meta, key, value)
                  case Some(h) =>
                     val arg = strictApplication(theory, key, List(value))
                     strictAttribution(meta, OMID(h.hastype), arg)
               }
         }
         case d: DefinedTheory => OMA(key, List(value)) //TODO what to do here?
      }
      
   }
   def strictBinding(theory: MPath, binder: Term, context: Context, scopes: List[Term]): Term = {
      lup.getTheory(theory) match {
         case d: DeclaredTheory => d.meta match {
            case None => OMBINDC(binder, context, scopes)
            case Some(meta) =>
               ps.getHOAS(meta) match {
                  case None =>
                     strictBinding(meta, binder, context, scopes)
                  case Some(h) =>
                     val arg = strictBinding(meta, OMID(h.lambda), context, scopes)
                     strictApplication(meta, OMID(h.apply), List(binder, arg))
               }
         }
         case d: DefinedTheory => OMBINDC(binder, context, scopes) //TODO what to do here?
      }
   }
   /**
    * removes the strict symbols to obtain a pragmatic term
    * @param t the strict term
    * @return (tP,ps) where tP is the pragmatic version of t and ps gives the positions of the components of tP in t
    *   i.e., (tP.components zip ps) forall {(c,p) => t.subobject(p) = c}
    */
   def pragmaticHead(t: Term) : Term = {
      pragmaticHeadWithPositions(t)._1
   }
   def pragmaticHeadWithPositions(t: Term) : (Term, List[Position]) = {
      pragmaticHeadAux(t,Position.positions(t))
   }
   private def pragmaticHeadAux(t: Term, pos: List[Position]) : (Term, List[Position]) = t match {
      case OMA(OMS(apply @ OMMOD(meta) % _), fun :: args) =>
         ps.getApplication(meta) match {
            case Some(a) =>
               if (a.apply == apply) {
                  val (tP, posP) = args match {
                     case List(OMBIND(OMS(lambda @ OMMOD(meta2) % _), context, scope)) =>
                        
                        ps.getHOAS(meta2) match {
                           case Some(h) =>
                              if (h.apply == apply && h.lambda == lambda) {
                                 // OMA(apply, fun, OMBIND(lambda, context, scope)) ---> OMBIND(fun, context, scope)
                                 val tP = OMBIND(fun, context, scope)
                                 val posP = pos(1) :: Position.positions(tP).tail.map(p => pos(2)/p)
                                 (tP, posP)
                              } else
                                 (OMA(fun, args), pos.tail)
                           case None =>
                                 (OMA(fun, args), pos.tail)
                        }
                     case _ =>   (OMA(fun, args), pos.tail)
                  }
                  pragmaticHeadAux(tP, posP)
               } else
                  (t,pos)
            case _ => (t,pos)
         }
      case _ => (t,pos)
   }
}

abstract class Feature {
   val theory: MPath
}

trait Application extends Feature {
   val apply: GlobalName
   override def toString = "Application: apply = " + apply.toPath
}

trait HOAS extends Application {
   val lambda: GlobalName
   override def toString = super.toString + "; HOAS: lambda = " + lambda.toPath  
}

//what about the other typing judgements - often there is more than one
trait Typing extends Feature {
   val hastype : GlobalName 
   def makeStrict(j: objects.Typing) = Inhabitation(j.stack, OMA(OMID(hastype), List(j.tm, j.tp)))
   def makePragmatic(j: Judgement) = j match {
      case Inhabitation(stack, OMA(OMID(this.hastype), List(tm, tp))) => Typing(stack, tm, tp)
      case _ => j
   }
}

abstract class Equality extends Feature {
   val logrel : MPath 
}