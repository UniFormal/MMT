package info.kwarc.mmt.api.pragmatics

import info.kwarc.mmt.api._
import objects._
import modules._
import frontend._
import libraries._

class Pragmatics(controller: Controller) {
   private val ps = controller.extman.pragmaticStore
   private lazy val lup = controller.globalLookup // must be lazy due to order of class initialization
   def strictApplication(theory: MPath, fun: Term, args: List[Term]) : Term = {
      lup.getTheory(theory) match {
         case d: DeclaredTheory => d.meta match {
            case None => OMA(fun, args)
            case Some(meta) =>
               ps.getApplication(meta) match {
                  case None =>
                     strictApplication(meta, fun, args)
                  case Some(a) =>
                     strictApplication(meta, OMID(a.apply), fun :: args)
               }
         }
         case d: DefinedTheory => OMA(fun, args) //TODO what to do here?
      }
   }
   def strictBinding(theory: MPath, binder: Term, context: Context, scope: Term): Term = {
      lup.getTheory(theory) match {
         case d: DeclaredTheory => d.meta match {
            case None => OMBIND(binder, context, scope)
            case Some(meta) =>
               ps.getHOAS(meta) match {
                  case None =>
                     strictBinding(meta, binder, context, scope)
                  case Some(h) =>
                     val arg = strictBinding(meta, OMID(h.lambda), context, scope)
                     strictApplication(meta, OMID(h.apply), List(arg))
               }
         }
         case d: DefinedTheory => OMBIND(binder, context, scope) //TODO what to do here?
      }
   }
   def pragmaticHead(t: Term) : Term = t match {
      case OMA(OMS(apply @ OMMOD(meta) % _), fun :: args) =>
         ps.getApplication(meta) match {
            case Some(a) =>
               if (a.apply == apply) {
                  val prag = args match {                  
                     case List(OMBIND(OMS(lambda @ OMMOD(meta2) % _), context, scope)) =>
                        ps.getHOAS(meta2) match {
                           case Some(h) =>
                              if (h.apply == apply && h.lambda == lambda)
                                 OMBIND(fun, context, scope)
                              else
                                 OMA(fun, args)
                           case None =>
                                 OMA(fun, args)
                        }
                     case _ =>   OMA(fun, args)
                  }
                  pragmaticHead(prag)
               } else
                  t
            case _ => t
         }
      case _ => t
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
abstract class Typing extends Feature {
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