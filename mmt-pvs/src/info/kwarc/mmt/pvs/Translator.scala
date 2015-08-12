package info.kwarc.mmt.pvs

import info.kwarc.mmt.api.symbols._
import syntax._

import info.kwarc.mmt.api._
import documents._
import modules._
import parser._
import objects._
import utils._
import archives._ 

import info.kwarc.mmt.lf._

import PVSTheory._

class PVSImportTask(bt: BuildTask, index: Document => Unit) {

   def doSourceRef(o: Object, oM: Term) = {
      /*
      val List(bR,bC,eR,eC) = stringToList(o.place, " ").map(_.toInt)
      val reg = SourceRegion(SourcePosition(-1,bR,bC),SourcePosition(-1,eR,eC))
      SourceRef.update(oM, SourceRef(FileURI(bt.inFile), reg))
      */
   }
   
   def doDocument(d: pvs_file) {
      val path = bt.narrationDPath.^!
      println("Document:" +path)
      val modsM = d._modules map doModule(path)
//      val mrefsM = modsM.map(m => MRef(path, m.path))
//      val doc = new Document(path, mrefsM)
//      index(doc)
   }
   
   def doModule(d:DPath)(m: syntax.Module): modules.Module = m match {
      case t: theory =>
         implicit val th = new DeclaredTheory(d,doName(t.named.id),Some(PVSTheory.path))
         // TODO: assuming, exporting_, possibly named stuff?
         t.theory_formals map doFormal
         val decls = t._decls map doDecl
         decls.filter(x => x.isDefined).map(x => th.add(x.get))
         println(" -- Theory: "+th)
         th
      case d:datatype =>
         println(" -- Datatype: "+d.body.named.id)
         null
      case _ =>
         println(" -- OTHER: "+m.getClass)
         null
   }

   def doFormal(f:FormalParameter)(implicit th:DeclaredTheory) = f match {
      case formal_type_decl(named, ne) =>
         th add Constant(OMID(th.path),doName(named.named.id),None,Some(OMID(PVSTheory.tp)),None,None)
      case formal_subtype_decl(named, ne, sup) =>
        /* th add Constant(OMID(th.path),doName(named.named.id),None,Some(
            Apply(OMID(PVSTheory.subtp),doType(sup._internal))
         ),None,None) */
         th add PVSTheory.subtp(th,named.named.id,doType(sup._declared))
      case formal_const_decl(named, tp2) =>
         th add PVSTheory.ofTypeDecl(th,named.named.id,doType(tp2._internal))
      case formal_theory_decl(named, _name) => println("TODO: formal_theory_decl")
   }
   
   def doDecl(d: Decl)(implicit th:DeclaredTheory) : Option[Declaration] = {
      d match {
         case var_decl(id,unnamed,tp2) => None /*
            Constant(OMID(th.path),doName(id),None,Some(
               PVSTheory.variable({
                  val typ = doType(tp2._internal,th)
                  if (typ == null) OMID(PVSTheory.tp) else typ})
            ),None,None) */
         case tcc_decl(named, assertion) => println("TODO: tcc_decl"); None
         case const_decl(named,arg_formals,tp2,_def) =>
            th add PVSTheory.ofTypeDecl(th,named.named.id,doType(tp2._internal))
            None
         case formula_decl(named,assertion) => Some(
            Constant(OMID(th.path),doName(named.named.id),None,Some(
               Apply(OMID(PVSTheory.formula),doExpr(assertion._formula))
            ),None,None)
         )
         case _ => println("TODO: "+d.getClass); None
      }
   }
   
   def doTheoryExpr(t: TheoryExpr): Term = {
      t match {
         case theory_name(place, id, library_id, mappings, targetOpt, actuals) =>
            val p = DPath(URI.empty / library_id) ? id
            val tM = OMMOD(p)
            doSourceRef(t, tM)
            tM
         case _ => println("TODO: "+t.getClass); null
      }
   }
   def doType(t: Type)(implicit th:DeclaredTheory): Term = {

      val tM: Term = t match {
         case type_name(place, tname, _res) =>
            val libraryname = if (_res.isDefined && _res.get._theory.library_id!="")
               PVSTheory.dpath / _res.get._theory.library_id
            else th.parent
            val theoryname = libraryname ? (if (_res.isDefined) doName(_res.get._theory.id) else th.name)
            if(!(th.getIncludes.contains(theoryname) || theoryname==th.path)) th add PlainInclude(theoryname,th.path)
            ApplySpine(OMID(theoryname ? doName(tname.id)), tname.actuals map doObject :_*)
         case type_application(place, tp, args) =>
            val tpM = doType(tp)
            val argsM = args map doExpr
            ApplySpine(tpM, argsM :_*)
         case function_type(place, _from, _to) =>
            Arrow(doDomain(_from),doType(_to))
         case tuple_type(place, doms) =>
            ApplySpine(OMID(PVSTheory.tuple),doms map doDomain:_* )
         case setsubtype(place, of, by) =>
            ApplySpine(OMID(PVSTheory.predsub),doType(of), doExpr(by))
         case cotuple_type(place, args) =>
            ApplySpine(OMID(PVSTheory.union),args map doType:_*)
         case record_type(place, _fields) => println("TODO: record_type"); OMID(PVSTheory.tp)
         case expr_as_type(place, expr, tp) =>
            Apply(OMID(asType),doExpr(expr))
      }
      doSourceRef(t, tM)
      tM

   }
   def doExpr(e: Expr)(implicit th:DeclaredTheory): Term = {
      val eM: Term = e match {
         case varname_expr(place, id, _) =>
            OMV(id)
         case lambda_expr(place, bindings,body) => Lambda(
            bindings map(b => VarDecl(doName(b.id),Some(doType(b._type)),None,None)
         ),doExpr(body))
         case forall_expr(place,bindings,body) => Pi(
            bindings map(b => VarDecl(doName(b.id),Some(doType(b._type)),None,None)),
            doExpr(body)
         ) // TODO - is it smart to use Pi for Forall?
         case application(place,funct,arg,infix) => arg match {
            case tuple_expr(place,args) => ApplySpine(OMS(PVSTheory.app),(funct::args) map doExpr :_*)
            case _ => ApplySpine(OMS(PVSTheory.app),doExpr(funct),doExpr(arg))
         }
         case name_expr(place, name, tp, res) =>
            val libraryname = if (res._theory.library_id!="")
               PVSTheory.dpath / res._theory.library_id
            else th.parent
            val theoryname = libraryname ? (if (res._theory.id!="") doName(res._theory.id) else
               th.name)
            if(!(th.getIncludes.contains(theoryname) || theoryname==th.path)) th add PlainInclude(theoryname,th.path)
            OMS(theoryname ? doName(name.id))
         case tuple_expr(place,args) =>
            ApplySpine(OMID(PVSTheory.tuple),args map doExpr :_*)
         case _ => println("TODO: "+e.getClass); OMID(PVSTheory.tp)
      }
      doSourceRef(e, eM)
      eM
   }

   def doObject(o:Object)(implicit th:DeclaredTheory) : Term = o match {
      case tp: Type => doType(tp)
      case e: Expr => doExpr(e)
      case te: TheoryExpr => doTheoryExpr(te)
   }

   def doDomain(d:domain)(implicit th:DeclaredTheory) : Term = d match {
      case tp: Type => doType(tp)
      case b: binding => println("TODO: Binding"); null

   }

   def doName(s:String) : LocalName = LocalName(s)
}