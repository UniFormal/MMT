package info.kwarc.mmt.pvs

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.{Constant, DeclaredStructure}
import info.kwarc.mmt.lf.{Arrow, Lambda, ApplySpine}
import utils._

object PVSTheory {
   val rootdpath = DPath(URI.http colon "pvs.csl.sri.com")
   val thname = "PVS"
   val path = rootdpath ? thname
   def sym(s: String) = path ? s

   def pitp(tp1:Term,v:VarDecl,tp2:Term) = sym("pitp").apply(tp1,Lambda(v,tp2))

   val exprastype = sym("exprastype")
   val predsub = sym("predsub")

   def subtypeof(sub:Term,sup:Term) = ApplySpine(OMS(sym("subtypeof")),sub,sup)

   def numberexpr(i:BigInt) = OML(VarDecl(LocalName(i.toString),Some(OMS(sym("number"))),None,None))

   def makecases(e:Term,sel:List[Term]) = ApplySpine(OMS(sym("makecases")),e,sel.tail.foldRight(sel.head)((t,c) =>
      ApplySpine(OMS(sym("casetuple")),t,c)))
   def casematch(head:Term,exp:Term) = ApplySpine(OMS(sym("casematch")),head,exp)

   val tp = sym("tp")
   val expr = sym("expr")
   def ofType(t1:Term,tp:Term) = ApplySpine(OMS(sym("ofType")),t1,tp)
   def constdecl(name:LocalName,home:MPath) =
      DeclaredStructure(OMID(home),name,OMID(path / LocalName("constdecl")),false)

   def PVSLambda(con:Context,t:Term) = con.foldRight(t)((v,b) => sym("lambda").apply(
      v.tp.get,Lambda(VarDecl(v.name,Some(OMS(expr)),None,None),b)))

   def forall(con:Context,t:Term) = con.foldRight(t)((v,b) => sym("forall").apply(
      v.tp.get,Lambda(VarDecl(v.name,Some(OMS(expr)),None,None),b)))

   def exists(con:Context,t:Term) = con.foldRight(t)((v,b) => sym("exists").apply(
      v.tp.get,Lambda(VarDecl(v.name,Some(OMS(expr)),None,None),b)))

   def casebind(con:Context,t:Term) = con.foldRight(t)((v,b) => sym("casebind").apply(
      v.tp.get,Lambda(VarDecl(v.name,Some(OMS(expr)),None,None),b)))

   val app = sym("app")
   val tpapp = sym("tpapp")
   val formula = sym("formula")
   val axiom = sym("axiom")

   def functype(from:Term,to:Term) = ApplySpine(OMS(sym("functype")),from,to)
   def tptuple(pars:List[Term]) = pars.dropRight(1).foldRight(pars.last)((p,c) => sym("tptuple").apply(p,c))
   def exprtuple(pars:List[Term]) = pars.dropRight(1).foldRight(pars.last)((p,c) => sym("exprtuple").apply(p,c))
   def recordtype(pars:List[Term]) = sym("recordtype").apply(pars.dropRight(1).foldRight(pars.last)((p,c) => sym("recordfieldtuple").apply(p,c)))

   def makerecordfield(name:LocalName,tp:Term) = ApplySpine(OMS(sym("makerecordfield")),
      OML(VarDecl(name,Some(Arrow(OMS(PVSTheory.expr),OMS(PVSTheory.expr))),None,None)),tp)

   def recordexpr(fields: List[(OML,Term)]) : Term = {
      val recflist = fields.map(f => ApplySpine(OMS(sym("definerecordfield")),f._1,f._2))
      sym("recordexpr").apply(recflist.dropRight(1).foldRight(recflist.last.asInstanceOf[Term])((p,c) => sym("recordfieldtuple").apply(p,c)))
   }

   def recdef(name:LocalName,home:MPath) =
      DeclaredStructure(OMID(home),name,OMID(path / LocalName("defdecl")),false)
   //def tpapp(tp:Term,args:List[Term]) = sym("tpapp").apply(tp,exprtuple(args))

   //def tm(t:Term) = sym("tm").apply(t)
   /*
   val functype = sym("functype")
   val tptuple = sym("tptuple")
   val exprtuple = sym("exprtuple")
   val predsub = sym("predsub")
   val union = sym("union")
   val asType = sym("asType")
   val expr = sym("expr")
   val tp = sym("tp")
   val app = sym("app")
   val tpapp = sym("tpapp")

   val fieldapp = sym("fieldapp")

   val ofType = sym("ofType")
   val cases = sym("cases")
   def pvscase(name:Term,expr:Term) = ApplySpine(OMS(sym("case")),name,expr)
   val formula = sym("formula")
   def subtp(th:DeclaredTheory,name:String,of:Term) =
      DeclaredStructure(OMID(th.path), LocalName(name), sym("SubtypeDecl").apply(of), false)
   def ofTypeDecl(th:DeclaredTheory,name:String,of:Term) =
      DeclaredStructure(OMID(th.path), LocalName(name), sym("ofTypeDecl").apply(of), false)
   def defdecl(th:DeclaredTheory, name:String, oftype: Term) =
      DeclaredStructure(OMID(th.path),LocalName(name),sym("defDecl").apply(oftype),false)
   def exprtocase(con:Context,name:Term,expr:Term) = PVSLambda(con,sym("exprtocase").apply(name,expr))
   def casedist(exp:Term, cases:List[Term]) = sym("casedist").apply(exp,cases.tail.foldLeft(cases.head)((c,t) =>
      ApplySpine(OMS(sym("casetuple")),c,t)))

   def forall(con:Context,t:Term) = con.foldRight(t)((v,b) => sym("forall").apply(
      v.tp.get,Lambda(VarDecl(v.name,Some(OMS(expr)),None,None),b)))
      */
}