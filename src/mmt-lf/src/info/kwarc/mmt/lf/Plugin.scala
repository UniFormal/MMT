package info.kwarc.mmt.lf

import info.kwarc.mmt.api._
import frontend._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.refactoring.{Hasher, Preprocessor}
import info.kwarc.mmt.lf.hollight.HOLLight

class Plugin extends frontend.Plugin {
   val theory = LF.theoryPath
   val dependencies = Nil
   override def start(args: List[String]): Unit = {
      val em = controller.extman
      // content enhancers
      em.addExtension(new NotationGenerator)
      em.addExtension(new SimplificationRuleGenerator)
      // build targets
      em.addExtension(new ScalaExporter)
      // Twelf parser
      em.addExtension(new TwelfParser)
      em.addExtension(HOLLight.preproc)
      em.addExtension(new TypedRelationalExtractor)
   }
}

/** added by rule in LF theory */
object LFHOAS extends notations.HOASNotation(LF.hoas)

case class ViewFinderHOAS(tpS : GlobalName, exprS : GlobalName, lambdaS : GlobalName, applyS : GlobalName, arrowS : GlobalName) {
   object Expr {
      def unapply(tm : Term) : Option[Term] = tm match {
         case ApplySpine(OMS(`exprS`),List(tp)) => Some(tp)
         case _ => None
      }
   }

   object Lambda {
      def unapply(tm : Term) : Option[(LocalName,Term,Term,Term)] = tm match {
         case ApplySpine(OMS(`lambdaS`),List(tpA,tpB,info.kwarc.mmt.lf.Lambda(ln,_,bd))) => Some((ln,tpA,tpB,bd))
         case _ => None
      }
   }

   object Apply {
      // def apply(f : Term, args : List[Term]) = args.foldLeft(f)((nf,a) => ApplySpine(OMS(applyS), nf,a))
      def unapply(tm : Term) : Option[(Term,List[Term])] = tm match {
         case ApplySpine(OMS(`applyS`), List(_,_,nf, a)) => Some(unapply(nf).map(p => (p._1, p._2 ::: a :: Nil)).getOrElse((nf, List(a))))
         case _ => None
      }
   }

   object Arrow {
      def unapply(tm : Term) : Option[(Term,Term)] = tm match {
         case ApplySpine(OMS(`arrowS`),List(tpA,tpB)) => Some((tpA,tpB))
         case _ => None
      }
   }
}

case class LFHOASElim(hoas : ViewFinderHOAS) extends Preprocessor {
   override def doTerm(tm: Term): Term = trav(tm,())

   private val trav = new StatelessTraverser {
      override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
         case OMS(hoas.tpS) =>
            OMS(Typed.ktype)
         case hoas.Expr(tm) =>
            traverse(tm)
         case hoas.Lambda(n,tpA,_,bd) =>
            Lambda(n,traverse(tpA),traverse(bd))
         case hoas.Apply(f,args) =>
            ApplySpine(traverse(f),args.map(traverse):_*)
         case hoas.Arrow(tpA,tpB) =>
            Arrow(traverse(tpA),traverse(tpB))
         case _ => Traverser(this,t)
      }
   }
}


case class LFClassicHOLPreprocessor(ded : GlobalName, and : GlobalName, not : GlobalName,
                               forall : Option[GlobalName] = None,
                               or : Option[GlobalName] = None,
                               implies : Option[GlobalName] = None,
                               equiv : Option[GlobalName] = None,
                               exists : Option[GlobalName] = None,
                               equal : Option[GlobalName] = None
                              ) extends Preprocessor {

   private object Ded {
      def apply(f : Term) = ApplySpine(OMS(ded),f)
      def unapply(tm : Term) = tm match {
         case ApplySpine(OMS(`ded`),prop :: Nil) => Some(prop)
         case _ => None
      }
   }

   private object And {
      def apply(a : Term, b : Term) = ApplySpine(OMS(and),a,b)
      def unapply(tm : Term) : Option[List[Term]] = tm match {
         case ApplySpine(OMS(`and`),a :: b :: Nil) => Some(unapply(a).getOrElse(List(a)) ::: unapply(b).getOrElse(List(b)))
         case _ => None
      }
   }

   private object Not {
      def apply(f : Term) = ApplySpine(OMS(not),f)
      def unapply(tm : Term) = tm match {
         case ApplySpine(OMS(`not`),prop :: Nil) => Some(prop)
         case _ => None
      }
   }

   private object Or {
      def unapply(tm : Term) = (or,tm) match {
         case (Some(s),ApplySpine(OMS(st),a :: b :: Nil)) if s == st => Some((a,b))
         case _ => None
      }
   }

   private object Implies {
      def unapply(tm : Term) = (implies,tm) match {
         case (Some(s),ApplySpine(OMS(st),a :: b :: Nil)) if s == st => Some((a,b))
         case _ => None
      }
   }

   private object Equiv {
      def unapply(tm : Term) = (equiv,tm) match {
         case (Some(s),ApplySpine(OMS(st),a :: b :: Nil)) if s == st => Some((a,b))
         case _ => None
      }
   }

     private object Equals {
       def unapply(tm : Term) = (equal,tm) match {
         case (Some(s),ApplySpine(OMS(st),tp :: a :: b :: Nil)) if s == st => Some((tp,a,b))
         case _ => None
       }
        def apply(tp : Term, a : Term, b : Term) = equal match {
           case Some(s) => ApplySpine(OMS(s),tp,a,b)
           case _ => ???
        }
     }


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
         case Ded(Forall(x,_,bd,tp)) =>
            Pi(x,traverse(tp),traverse(Ded(bd)))
         case Ded(Implies(a,b)) =>
            traverse(Ded(a)) match {
               case Ded(And(ls)) =>
                  Arrow(ls.map(Ded.apply),traverse(Ded(b)))
               case r =>
                  Arrow(r,traverse(Ded(b)))
            }
         case Arrow(Ded(a),Ded(c)) =>
            traverse(Ded(a)) match {
               case Ded(And(ls)) =>
                  Arrow(ls.map(i => traverse(Ded(i))),traverse(Ded(c)))
               case r =>
                  Arrow(r,traverse(Ded(c)))
            }
         case Exists(x,tp,bd,tt) =>
            traverse(Not(Forall(x,tp,Not(bd),tt)))
         case Implies(a,b) =>
            traverse(Not(And(a,Not(b))))
         case Or(a,b) =>
            traverse(Not(And(Not(a),Not(b))))
         case Not(inner) =>
            traverse(inner) match {
               case Not(ii) => ii
               case r => Not(r)
            }
         case And(ls) =>
            val ret = ls.map(traverse).sortWith(leq)
            ret.tail.foldLeft(ret.head)(And(_,_))
         case Equals(tp,a,b) =>
            val (na,nb) = (traverse(a),traverse(b))
            if (leq(nb,na)) Equals(Hasher.Complex(tp),nb,na)
            else Equals(Hasher.Complex(tp),na,nb)
         case Pi(x,tp,bd) => Pi(x,traverse(tp),traverse(bd)) // this uncurries
         case _ => Traverser(this,t)
      }
   }

   override protected def doTerm(tm: Term): Term = traverser(tm,Context.empty)

   private def leq(t1 : Term, t2 : Term) : Boolean = t1.subobjects.length <= t2.subobjects.length
}