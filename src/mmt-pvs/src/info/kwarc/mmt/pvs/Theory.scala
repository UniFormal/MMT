package info.kwarc.mmt.pvs

import info.kwarc.mmt.LFX.Records.Recexp
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.FinalConstant
import info.kwarc.mmt.api.uom._
import info.kwarc.mmt.lf.{Lambda, Apply, ApplySpine}
import info.kwarc.mmt.pvs.PVSTheory.expr
import utils._
import objects.Conversions._


object NatLiterals extends RepresentedRealizedType(expr(OMS(PVSTheory.thpath ? "NatLit")),StandardNat) {
   val pvstp = OMS(PVSTheory.thpath ? "NatLit")
}
object StringLiterals extends RepresentedRealizedType(expr(OMS(PVSTheory.thpath ? "StringLit")),StandardString) {
   val pvstp = OMS(PVSTheory.thpath ? "StringLit")
}
object RationalLiterals extends RepresentedRealizedType(expr(OMS(PVSTheory.thpath ? "RatLit")),StandardRat) {
   val pvstp = OMS(PVSTheory.thpath ? "RatLit")
}

object PVSTheory {
   val rootdpath = DPath(URI.http colon "pvs.csl.sri.com")
   val thname = "PVS"
   val thpath = rootdpath ? thname//Path.parseM("http://pvs.csl.sri.com/?PVS",NamespaceMap.empty) // rootdpath ? thname
   val preludepath = rootdpath ? "Prelude" //Path.parseM("http://pvs.csl.sri.com/?Prelude",NamespaceMap.empty) //rootdpath ? "Prelude"
   val natlit = thpath ? "NatLit"
   class sym(s: String) {
      val path = thpath ? s
      val term = OMS(path)
   }

   object tp extends sym("tp")
   //object unknown extends sym("unknown_type")

   object expr extends sym("expr") {
      def apply(t:Term) = Apply(this.term,t)
      def unapply(t:Term) : Option[Term] = t match {
         case Apply(this.term,tm) => Some(tm)
         case _ => None
      }
   }

   object subtpissubtype extends sym("subtpissubtype") {
      def apply(tp1 : Term, tp2 : Term, proof : GlobalName) = ApplySpine(this.term,tp1,tp2,OMS(proof))
   }

   object nonempty extends sym("nonempty") {
      def apply(t:Term) = Apply(this.term,t)
   }

   object is_nonempty extends sym("is_nonempty") {
      def apply(tp:Term,expr:Term) = ApplySpine(this.term,tp,expr)
   }

   object bool extends sym("bool") {
      def unapply(t : Term) : Boolean = t match {
         case this.term => true
         case OMS(p) if p == PVSTheory.thpath ? "bool" => true
         case OMS(p) if p == PVSTheory.thpath ? "boolean" => true
         case _ => false
      }
   }

   object proof extends sym("proof") {
      def apply(kind:String,thm:Term) = ApplySpine(this.term,new sym(kind).term,thm)
      def unapply(t:Term) : Option[(Term,Term)] = t match {
         case ApplySpine(this.term,List(a,b)) => Some((a,b))
         case _ => None
      }
   }

   object forall extends sym("forall") {
      def apply(con:Context,tm:Term) = con.foldRight(tm)((v,t) => ApplySpine(this.term,v.tp.get,Lambda(v.name,expr(v.tp.get),t)))
   }

   object exists extends sym("exists") {
      def apply(con:Context,tm:Term) = con.foldRight(tm)((v,t) =>
         ApplySpine(this.term,v.tp.get,Lambda(v.name,expr(v.tp.get),t)))
   }

   object lambda extends sym("lambda") {
      def apply(x : LocalName,tp:Term,tptarget:Term,body:Term) =
         ApplySpine(this.term,tp,Lambda(x,expr(tp),tptarget),Lambda(x,expr(tp),body))
      def unapply(t:Term) : Option[(LocalName,Term,Term,Term)] = t match {
         case ApplySpine(this.term,List(tp1,Lambda(x1,expr(tp2),tptarget),Lambda(x2,expr(tp3),body))) =>
          Some((x1,tp1,tptarget,body))
      }
   }

   object apply extends sym("pvsapply") {
      def apply(f : Term,t:Term,ftp : Term)(state:ImportState) : (Term,Term) = ftp match {
         case pvspi(x,tpx,Lambda(y,btp,rettp)) => (ApplySpine(this.term,tpx,Lambda(y,btp,rettp),f,t),rettp ^? y/t) //Apply(tpf,t))
         case _ if state != null => (ApplySpine(this.term,state.doUnknown,state.doUnknown,f,t),state.doUnknown)
         case _ => throw new Exception("Unknown types in pvsapply")
      }
      def unapply(t : Term) : Option[(Term,Term,Term)] = t match {
         case ApplySpine(this.term,List(tpx,Lambda(y,btp,rettp),f,t2)) =>
          Some(f,t2,rettp ^? y/t2)
      }
   }

   object fun_type extends sym("fun_type") {
      def apply(a:Term,b:Term) = ApplySpine(this.term,a,b)
      def unapply(t:Term) : Option[(Term,Term)] = t match {
         case ApplySpine(this.term,List(a,b)) => Some((a,b))
         case _ => None
      }
   }

   object pvspi extends sym("pvspi") {
      def apply(bound : LocalName, boundtp : Term, rettp : Term) = ApplySpine(this.term,boundtp,Lambda(bound,expr(boundtp),rettp))
      def unapply(t:Term) : Option[(LocalName,Term,Term)] = t match {
         case ApplySpine(this.term,List(boundtp,Lambda(bound,expr(boundtp2),rettp))) =>
            Some(bound,boundtp,Lambda(bound,expr(boundtp),rettp))
         case fun_type(a,b) =>
            val x = Context.pickFresh(t.freeVars.map(VarDecl(_,None,None,None)),LocalName("__"))._1
            Some((x,a,Lambda(x,expr(a),b)))
         case _ => None
      }
   }

   object tuple_type extends sym("tuple_tp") { // TODO needs reworking
      def apply(l:List[Term]) : Term = if (l.length==2) ApplySpine(this.term,l.head,l.tail.head) else
         if (l.length==1) l.head else
         ApplySpine(this.term,l.head,apply(l.tail))
      def unapply(t:Term) : Option[List[Term]] = t match {
         case ApplySpine(this.term,List(a,b)) => Some(a :: unapply(b).getOrElse(List(b)))
         case _ => None
      }
   }

   object pvssigma extends sym("pvssigma") {
      def apply(bound : LocalName, boundtp : Term, rettp : Term) = ApplySpine(this.term,boundtp,
         Lambda(bound,expr(boundtp),rettp))
      def unapply(t:Term) : Option[(LocalName,Term,Term)] = t match {
         case ApplySpine(this.term,List(boundtp,Lambda(bound,boundtp2,rettp))) =>
            Some(bound,boundtp,Lambda(bound,boundtp2,rettp))
         case tuple_type(l) => Some((Context.pickFresh(t.freeVars.map(VarDecl(_,None,None,None)),LocalName("__"))._1,
           l.head,tuple_type(l.tail)))
         case _ => None
      }
   }

   object tuple_expr extends sym("tuple_expr") { // TODO needs reworking
      def apply(l:List[(Term,Term)]) : (Term,Term) =
         if (l.length==2) (ApplySpine(this.term,l.head._2,l.tail.head._2,l.head._1,l.tail.head._1),
           tuple_type(List(l.head._2,l.tail.head._2)))
         else if (l.length == 1) l.head else {
            val ret = apply(l.tail)
            (ApplySpine(this.term,l.head._2,ret._2,l.head._1,ret._1),tuple_type(List(l.head._2,ret._2)))
         }
      def unapply(t : Term) : Option[List[(Term,Term)]] = t match {
         case ApplySpine(this.term,List(tA,tB,a,b)) => Some((a,tA) :: unapply(b).getOrElse(List((b,tB))))
         case _ => None
      }
   }

   object typecast extends sym("TypeCast") {
      def apply(tpA:Term,tpB:Term,e: Term,prf:FinalConstant = null) =
         if (prf!=null) {
            val (kind,form) = prf.tp.get match {
               case proof(a,b) => (a,b)
               case _ =>
                  println("TCC is not a proof! " + prf.tp.get)
                  (new sym("internal_judgement").term,new sym("TRUE").term)
            }
            ApplySpine(this.term,tpA,tpB,kind,form,e,prf.toTerm)
         } else ApplySpine(this.term,tpA,tpB,new sym("internal_judgement").term,new sym("TRUE").term,e,
            new sym("proof_of_TRUE").term)
   }

   object expr_as_type extends sym("expr_as_type") {
      def apply(expr : Term, tp : Term) = ApplySpine(this.term,tp,expr)
   }

   object recursor extends sym("recursor") {
      def apply(tp:Term,name:LocalName,defi:Term) = ApplySpine(this.term,tp,Lambda(name,expr(tp),defi))
   }

   object selection extends sym("selection") {
      def apply(cons : Term, vars : Context, body : Term, constp : Term) =
         ApplySpine(this.term,constp,cons,if (vars.nonEmpty) OMBIND(new sym("selbind").term,
            vars.map(v => VarDecl(v.name,v.tp.map(expr(_)),v.df,v.not)),body) else body)
   }

   object pvsmatch extends sym("match") {
      def apply(tm : Term, tmtp : Term, cases : List[Term], rettp : Term) =
         ApplySpine(this.term, tmtp, rettp, tm, ApplySpine(new sym("caselist").term,cases:_*))
   }

   object subtpjudg extends sym("subtpjudg") {
      def apply(sub:Term,sup:Term) = ApplySpine(this.term,sub,sup)
   }

   object equal extends sym("=") {
      def apply(tp:Term,a:Term,b:Term)(state:ImportState) = PVSTheory.apply(
         Apply(this.term,tp),
         tuple_expr(List((a,tp),(b,tp)))._1,
         fun_type(tuple_type(List(tp,tp)),bool.term))(state)
   }

   object tpjudg extends sym("tpjudg") {
      def apply(tm : Term, tmtp : Term, restp : Term) = ApplySpine(this.term,tmtp,tm,restp)
   }

   object pred {
      val predpaths = List("pred","PRED","predicate","PREDICATE","setof","SETOF").map(s =>
         Path.parseS("http://pvs.csl.sri.com/Prelude?defined_types?"+s,NamespaceMap.empty))
      def unapply(t:Term) : Option[Term] = t match {
         case Apply(OMS(p),a) if predpaths contains p => Some(a)
         case PVSTheory.pvspi(_,a,Lambda(_,_,PVSTheory.bool())) => Some(a)
         case _ => None
      }
   }

   object fieldapp extends sym("fieldapp") {
      def apply(tm : Term, field : String) = ApplySpine(this.term,tm,OML(LocalName(field),None,None))
   }

   object recordexpr extends sym("recordexpr") {
      def apply(nametpdf : (LocalName,Term,Term)*) = {
         val rtp = Recexp(nametpdf map (t => OML(t._1, Some(tp.term), Some(t._2))): _*)
         val rdf = Recexp(nametpdf map (t => OML(t._1, Some(expr(t._2)), Some(t._3))): _*)
         ApplySpine(this.term, rtp,rdf)
      }
   }

   object recordtp extends sym("rectp") {
      def apply(nametp : (LocalName,Term)*) =
         ApplySpine(this.term,Recexp(nametp.map(t => OML(t._1,Some(tp.term),Some(t._2))):_*))
      def unapply(t:Term) : Option[List[(String,Term)]] = t match {
         case ApplySpine(this.term,List(Recexp(l))) => Some(l.map({
            case OML(name,Some(tp.term),Some(df)) => (name.toString,df)
            case tm @ _ => throw new Exception("Invalid Record type element: " + tm)
         }))
         case _ => None
      }
   }

   object setsub extends sym("setsub") {
      def apply(tp:Term,expr:Term) = ApplySpine(this.term,tp,expr)
   }

   object projection extends sym("proj") {
      def apply(tm:Term,tmtp:Term,i:Int)(state : ImportState) = {
         val tp = tmtp match {
            case PVSTheory.tuple_type(l) if i<=l.length => l(i-1)
            case _ => state.doUnknown
         }
         (ApplySpine(this.term,tmtp,tp,tm,OMLIT(BigInt(i),NatLiterals)),tp)
      }
   }

   object enumtype extends sym("enumtype") {
      def apply(l:List[String]) = ApplySpine(this.term,l.map(StringLiterals(_)):_*)
   }

   object update extends sym("update") {
      def apply(e : Term, up : Term) = ApplySpine(this.term,List(e,up):_*)
   }

   object recupdate extends sym("recupdate") {
      def apply(field:String, expr:Term, args : List[Term] = Nil) =
         ApplySpine(this.term,OML(LocalName(field),None,None)::expr::args:_*)
   }

   object funupdate extends sym("funupdate") {
      def apply(arg:Term,value:Term) = ApplySpine(this.term,arg,value)
   }

   object tupleupdate extends sym("tupleupdate") {
      def apply(index:Int,value:Term) = ApplySpine(this.term,OMLIT(BigInt(index),NatLiterals),value)
   }

   object typeext extends sym("type_extension") {
      def apply(tp : Term, by : Term) = ApplySpine(this.term,tp,by)
   }

   object powertp extends sym("powertp") {
      def apply(tp : Term) = Apply(this.term,tp)
   }

   /*
   object subtpJudg extends sym("subtpJudg") {
      def apply(subtp : Term, suptp : Term) = ApplySpine(this.term,subtp,suptp)
   }

   // TODO temporary pseudosolution
   object tccs extends sym("tcclist") {
      def apply(tm:Term*) = if (tm.isEmpty) unknown.term else if (tm.length==1) tm.head else ApplySpine(this.term,tm:_*)
   }





   object constructor extends sym("constructor") {
      def apply(recognizer : String, accessors : List[(String,Term)]) = ApplySpine(this.term,
         OMLIT(recognizer,StringLiterals)::accessors.map(p =>OML(VarDecl(LocalName(p._1),Some(p._2),None,None))):_*)
   }
   */

}