package info.kwarc.mmt.pvs

import syntax._

import info.kwarc.mmt.api._
import documents._
import modules._
import parser._
import objects._
import utils._
import archives._ 

import PVSTheory._

class PVSImportTask(bt: BuildTask, index: Document => Unit) {
   def doSourceRef(o: Object, oM: Term) = {
      val List(bR,bC,eR,eC) = stringToList(o.place, " ").map(_.toInt)
      val reg = SourceRegion(SourcePosition(-1,bR,bC),SourcePosition(-1,eR,eC))
      SourceRef.update(oM, SourceRef(FileURI(bt.inFile), reg))
   }
   
   def doDocument(d: pvs_file) {
      val modsM = d._modules map doModule
      val path = bt.narrationDPath
      val mrefsM = modsM.map(m => MRef(path, m.path))
      val doc = new Document(path, mrefsM)
      index(doc)
   }
   
   def doModule(m: syntax.Module): modules.Module = m match {
      case t: theory => null
   }
   
   def doDecl(d: Decl) {}
   
   def doTheoryExpr(t: TheoryExpr): Term = {
      t match {
         case theory_name(place, id, library_id, mappings, targetOpt, actuals) =>
            val p = DPath(URI.empty / library_id) ? id
            val tM = OMMOD(p)
            doSourceRef(t, tM)
            tM
      }
   }
   def doType(t: Type): Term = {
      val tM: Term = t match {
         case type_name(place, name, _res) => ???
         case type_application(place, tp, args) =>
            val tpM = doType(tp)
            val argsM = args map doExpr
            OMA(tpM, argsM)
         case function_type(place, _from, _to) =>
            ???
         case tuple_type(place, doms) =>
            ???
         case setsubtype(place, of, by) =>
            ??? // predsub(doType(of), doExpr(by))
         case cotuple_type(place, args) =>
            union(args map doType)
         case record_type(place, _fields) => ???
         case expr_as_type(place, expr, tp) =>
            asType(doExpr(expr))
      }
      doSourceRef(t, tM)
      tM
   }
   def doExpr(e: Expr): Term = {
      val eM: Term = e match {
         case varname_expr(place, id, _) =>
            OMV(id)
      }
      doSourceRef(e, eM)
      eM
   }
}