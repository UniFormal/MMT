package info.kwarc.mmt.pvs

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.{Constant, DeclaredStructure}
import info.kwarc.mmt.lf.{Lambda, Apply, ApplySpine, Arrow}
import info.kwarc.mmt.pvs.syntax.tuple_type
import utils._

object PVSTheory {
   val rootdpath = DPath(URI.http colon "pvs.csl.sri.com")
   val thname = "PVS"
   val thpath = rootdpath ? thname
   class sym(s: String) {
      val path = thpath ? s
      val term = OMS(path)
   }

   object unknown extends sym("unknown")
   object prop extends sym("prop")

   object tp extends sym("tp")

   object tuple_type extends sym("tuple_type") {
      def apply(l:List[Term]) : Term = {
         require(l.length>=2)
         if (l.length == 2) ApplySpine(this.term,l:_*) else
            ApplySpine(this.term,l.head,apply(l.tail))
      }
      /*
      def unapply(tm:Term) : Option[List[Term]] = tm match {
         case Apply(this.term,t) => Some(List(t))
         case Apply(tuple_type(l1),tuple_type(l2)) => Some(l1:::l2)
         case Apply(tuple_type(l),t) => Some(l:::List(t))
         case _ => None
      }
      */
   }

   object tuple_expr extends sym("tuple_expr") {
      def apply(l:List[(Term,Term)]) : (Term,Term) = {
         require(l.length>=2)
         if (l.length == 2) (ApplySpine(this.term,l.head._2,l(1)._2,l.head._1,l(1)._1),tuple_type(List(l.head._2,l(1)._2)))
         else {
            val (tm,tp) = apply(l.tail)
            (ApplySpine(this.term,l.head._2,tp,l.head._1,tm),tuple_type(List(l.head._2,tp)))
         }
      }
      /*
      def unapply(tm:Term) : Option[List[(Term,Term)]] = tm match {
         case Apply(Apply(Apply(Apply(this.term,tp1),tp2),tm1),tm2) => Some(List((tm1,tp1),(tm2,tp2)))
         case Apply(tuple_expr(l1),tuple_expr(l2)) => Some(l1:::l2)
         case Apply(tuple_expr(l),t) => Some(l:::List(t))
         case _ => None
      }
      */
   }

   object expr extends sym("expr") {
      def apply(t:Term) = Apply(this.term,t)
      def unapply(t:Term) : Option[Term] = t match {
         case Apply(this.term,tm) => Some(tm)
         case _ => None
      }
   }

   object forall extends sym("forall") {
      def apply(con:Context,tm:Term) = con.foldRight(tm)((v,t) => ApplySpine(this.term,v.tp.get,Lambda(v.name,expr(v.tp.get),t)))
   }

   object lambda extends sym("lambda") {
      def apply(con:Context,tm:Term,tp:Term) = con.foldRight((tm,tp))((v,t) =>
         (ApplySpine(this.term,v.tp.get,t._2,Lambda(v.name,expr(v.tp.get),t._1)),fun_type(v.tp.get,t._2)))._1
   }

   object apply extends sym("pvsapply") {
      def apply(f:Term,t:Term)(tpsrc: Term = unknown.term, tptarget : Term = unknown.term)
      = ApplySpine(this.term,tpsrc,tptarget,f,t)
   }

   object fun_type extends sym("fun_type") {
      def apply(a:Term,b:Term) = ApplySpine(this.term,a,b)
      def unapply(t:Term) : Option[(Term,Term)] = t match {
         case Apply(Apply(this.term,a),b) => Some((a,b))
         case _ => None
      }
   }

   object asType extends sym("TypeCast") {
      def apply(tpA : Term, tpB : Term, tmA : Term, tcc : Term) =
      ApplySpine(this.term,tpA,tpB,tmA,tcc)
   }

   object proof extends sym("proof") {
      def apply(s:String, t:Term) = ApplySpine(this.term,new sym(s).term,t)
   }

   object expr_as_type extends sym("expr_as_type") {
      def apply(expr : Term, tp : Term) = ApplySpine(this.term,tp,expr)
   }

   object selection extends sym("selection") {
      def apply(cons : Term, vars : Context, body : Term, constp : Term) =
      ApplySpine(this.term,constp,cons,OMBIND(new sym("selbind").term,
         vars.map(v => VarDecl(v.name,v.tp.map(expr(_)),v.df,v.not)),body))
   }

   object pvsmatch extends sym("match") {
      def apply(tm : Term, tmtp : Term, cases : List[Term], rettp : Term) =
      ApplySpine(this.term, tmtp, rettp, tm, ApplySpine(new sym("caselist").term,cases:_*))
   }

   object subtp extends  sym("subtp") {
      def apply(tp : Term) = Apply(this.term,tp)
   }

   object typJudg extends sym("typing_judgement") {
      def apply(tm : Term, tmtp : Term, restp : Term) = ApplySpine(this.term,tmtp,tm,restp)
   }

   object fieldapp extends sym("fieldapp") {
      def apply(tm : Term, field : String) = ApplySpine(this.term,tm,OML(VarDecl(LocalName(field),None,None,None)))
   }
   /*
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
   */

}