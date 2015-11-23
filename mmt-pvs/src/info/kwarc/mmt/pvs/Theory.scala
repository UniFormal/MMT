package info.kwarc.mmt.pvs

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.{Constant, DeclaredStructure}
import info.kwarc.mmt.lf.{Lambda, Apply, ApplySpine, Arrow}
import utils._

object PVSTheory {
   val rootdpath = DPath(URI.http colon "pvs.csl.sri.com")
   val thname = "PVS"
   val thpath = rootdpath ? thname
   abstract class sym(s: String) {
      val path = thpath ? s
      val term = OMS(path)
   }

   object expr extends sym("expr")
   object tp extends sym("tp")
   object ofType extends sym("ofType") {
      def apply(tm:Term,tp:Term) = ApplySpine(this.term,tm,tp)
   }
   object subtp extends sym("subtypeof") {
      def apply(sub:Term,sup:Term) = ApplySpine(this.term,sub,sup)
   }
   object arrow extends sym("funtype") {
      def apply(dom:Term,cod:Term) = ApplySpine(this.term,dom,cod)
      def unapply(t:Term) : Option[(Term,Term)] = t match {
         case ApplySpine(this.term,List(dom,cod)) => Some((dom,cod))
         case _ => None
      }
   }

   object tptuple extends sym("tptuple") {
      def apply(a:Term,b:Term) : Term = ApplySpine(this.term,a,b)
      def apply(l:List[Term]) : Term = if (l.length>2) ApplySpine(this.term,l.head,apply(l.tail))
         else apply(l.head,l.tail.head)
   }

   object exprtuple extends sym("exprtuple") {
      def apply(a:Term,b:Term) : Term = ApplySpine(this.term,a,b)
      def apply(l:List[Term]) : Term = if (l.length>2) ApplySpine(this.term,l.head,apply(l.tail))
      else apply(l.head,l.tail.head)
   }

   object lambda extends sym("lambda") {
      def apply(_var:LocalName,tp:Term,bd:Term) = ApplySpine(this.term,tp,Lambda(_var,expr.term,bd))
   }

   object forall extends sym("forall") {
      def apply(_var:LocalName,tp:Term,bd:Term) = ApplySpine(this.term,tp,Lambda(_var,expr.term,bd))
   }

   object PVSapply extends sym("PVSapply") {
      def apply(f:Term,a:Term) : Term = ApplySpine(this.term,f,a)
      def apply(f:Term,l:List[Term]) : Term = l.foldLeft(f)((g,t) => apply(g,t))
   }

   object formula extends sym("formula") {
      def apply(kind:String, phi:Term) = ApplySpine(this.term,OMS(thpath ? kind),phi)
   }

   object Lift {
      object Elem extends sym("Elem") {
         def apply(tp:Term) = OMA(this.term,List(tp))
         def unapply(tp:Term) : Option[Term] = tp match {
            case OMA(this.term,List(tp1)) => Some(tp1)
            case _ => None
         }
      }
      private object elem extends sym("elem") {
         def apply(expr:Term,tp:Term,tprel:Term) = OMA(this.term,List(expr,tp,tprel))
      }
      private object funType_out extends sym("funType_out") {
         def apply(f:Term,tp1:Term,tp2:Term) = OMA(this.term,List(tp1,tp2,tp(arrow(tp1,tp2))))
      }

      def tp(tp:Term) = Elem.apply(tp)

      def expr(expr:Term,tp:Term,tprel:Term) = elem.apply(expr,tp,tprel)

      def funType(f:Term,tp:Term) : Option[Term] = tp match {
         case arrow(dom,cod) => Some(funType_out(f,dom,cod))
         case _ => None
      }
   }

   object Lower {
      private object funType_in extends sym("funType_in") {
         def apply(f:Term,tp1:Term,tp2:Term) = OMA(this.term,List(tp1,tp2,f))
      }

      def funtype(f:Term,tp:Term) = tp match {
         case Arrow(Lift.Elem(tp1),Lift.Elem(tp2)) => Some(funType_in(f,tp1,tp2))
         case _ => None
      }
   }

   def constdecl(th:DeclaredTheory, name:String, tp1:Term, _def:Option[Term])(dotp: Term => Term = x => x) = {
      val s = DeclaredStructure(th.toTerm,LocalName(name+"_INTERNAL"),
         OMID((rootdpath / thname) ? LocalName("constdecl")),false)
      s add Constant(s.toTerm,LocalName("PVStype"),None,Some(dotp(tp.term)),Some(tp1),None)
      if (_def.isDefined)
         s add Constant(s.toTerm,LocalName("def"),None,Some(dotp(expr.term)),_def,None)
      List(s,Constant(th.toTerm,LocalName(name),None,Some(dotp(expr.term)),Some(OMS((th.path / s.name) ? LocalName("def"))),None))
   }

}