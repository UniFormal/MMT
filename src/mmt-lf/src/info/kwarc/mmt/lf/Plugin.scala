package info.kwarc.mmt.lf

import info.kwarc.mmt.api._
import frontend._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.refactoring.Preprocessor

class Plugin extends frontend.Plugin {
   val theory = LF.theoryPath
   val dependencies = List("info.kwarc.mmt.moduleexpressions.MorphismPlugin")
   override def start(args: List[String]) {
      val em = controller.extman
      // content enhancers
      em.addExtension(new NotationGenerator)
      em.addExtension(new SimplificationRuleGenerator)
      // build targets
      em.addExtension(new ScalaExporter)
      // Twelf parser
      em.addExtension(new TwelfParser)
   }
}

/** added by rule in LF theory */
object LFHOAS extends notations.HOASNotation(LF.hoas)

class LFClassicHOLPreprocessor(ded : GlobalName, and : GlobalName, not : GlobalName,
                               forall : Option[GlobalName] = None,
                               or : Option[GlobalName] = None,
                               implies : Option[GlobalName] = None,
                               equiv : Option[GlobalName] = None,
                               exists : Option[GlobalName] = None,
                               hoasapply : Option[GlobalName] = None
                              ) extends Preprocessor {

   private object ApplyS {
      def apply(f : Term, args : List[Term]) = if (hoasapply.isDefined) {
         args.foldLeft(f)((nf,a) => ApplySpine(OMS(hoasapply.get), nf,a))
      } else ApplySpine(f,args :_*)
      def unapply(tm : Term) : Option[(Term,List[Term])] = if (hoasapply.isDefined) {
         val hoas = hoasapply.get
         tm match {
            case ApplySpine(OMS(`hoas`),List(nf,a)) => Some(unapply(nf).map(p => (p._1, p._2 ::: a :: Nil)).getOrElse((nf,List(a))))
         }
      } else ApplySpine.unapply(tm)
   }

   private object Ded {
      def apply(f : Term) = ApplySpine(OMS(ded),f)
      def unapply(tm : Term) = tm match {
         case ApplySpine(OMS(`ded`),prop :: Nil) => Some(prop)
         case _ => None
      }
   }

   private object And {
      def apply(a : Term, b : Term) = ApplyS(OMS(and),List(a,b))
      def unapply(tm : Term) : Option[List[Term]] = tm match {
         case ApplyS(OMS(`and`),a :: b :: Nil) => Some(unapply(a).getOrElse(List(a)) ::: unapply(b).getOrElse(List(b)))
         case _ => None
      }
   }

   private object Not {
      def apply(f : Term) = ApplyS(OMS(not),List(f))
      def unapply(tm : Term) = tm match {
         case ApplyS(OMS(`ded`),prop :: Nil) => Some(prop)
         case _ => None
      }
   }

   private object Or {
      def unapply(tm : Term) = (or,tm) match {
         case (Some(s),ApplyS(OMS(st),a :: b :: Nil)) if s == st => Some((a,b))
         case _ => None
      }
   }

   private object Implies {
      def unapply(tm : Term) = (implies,tm) match {
         case (Some(s),ApplyS(OMS(st),a :: b :: Nil)) if s == st => Some((a,b))
         case _ => None
      }
   }

   private object Equiv {
      def unapply(tm : Term) = (equiv,tm) match {
         case (Some(s),ApplyS(OMS(st),a :: b :: Nil)) if s == st => Some((a,b))
         case _ => None
      }
   }
   /*
     private object Equals {
       def unapply(tm : Term) = (equal,tm) match {
         case (Some(s),Apply(OMS(st),a :: b :: Nil)) if s == st => Some((a,b))
         case _ => None
       }
     }
     */

   private object Forall {
      def apply(x : LocalName, tp : Term, bd : Term, btp : Term) =
         ApplySpine(OMS(forall.get),btp,Lambda(x,tp,bd))
      def unapply(tm : Term) = (forall,tm) match {
         case (Some(s),ApplySpine(OMS(st),tp :: Lambda(x,t,b) :: Nil)) if s == st =>
            Some((x,t,b,tp))
         case _ => None
      }
   }

   private object Exists {
      def unapply(tm : Term) = (exists,tm) match {
         case (Some(s),ApplySpine(OMS(st),tp :: Lambda(x,t,b) :: Nil)) if s == st =>
            Some((x,t,b,tp))
         case _ => None
      }
   }

   val traverser = new StatelessTraverser {
      override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
         case Arrow(Ded(a),Ded(c)) =>
            traverse(Ded(a)) match {
               case Ded(And(ls)) =>
                  Arrow(ls.map(i => traverse(Ded(i))),traverse(Ded(c)))
               case r =>
                  Arrow(r,traverse(Ded(c)))
            }
         case Ded(Forall(x,tp,bd,_)) => Pi(x,tp,traverse(Ded(bd)))
         case Ded(Implies(a,b)) =>
            traverse(Ded(a)) match {
               case Ded(And(ls)) =>
                  Arrow(ls.map(Ded.apply),traverse(Ded(b)))
               case r => Arrow(r,traverse(Ded(b)))
            }
         case Exists(x,tp,bd,tt) => traverse(Not(Forall(x,tp,Not(bd),tt)))
         case Implies(a,b) => traverse(Not(And(a,Not(b))))
         case Or(a,b) => traverse(Not(And(Not(a),Not(b))))
         case Not(inner) =>
            traverse(inner) match {
               case Not(ii) => ii
               case r => Not(r)
            }
         case And(ls) =>
            val ret = ls.sortWith(leq).map(traverse)
            ret.tail.foldLeft(ret.head)(And(_,_))
         case _ => Traverser(this,t)
      }
   }

   override protected def doTerm(tm: Term): Term = traverser(tm,Context.empty)

   private def leq(t1 : Term, t2 : Term) : Boolean = t1.subobjects.length <= t2.subobjects.length
}