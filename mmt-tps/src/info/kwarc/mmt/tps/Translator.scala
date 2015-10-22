package info.kwarc.mmt.tps

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.metadata.MetaDatum
import info.kwarc.mmt.api.notations._
import info.kwarc.mmt.api.symbols.{Constant, PlainInclude}
import syntax._

import info.kwarc.mmt.api._
import documents._
import modules._
import parser._
import objects._
import utils._
import archives._

import info.kwarc.mmt.lf._

class TPSImportTask(controller: Controller, bt: BuildTask, index: Document => Unit) {
   var path : DPath = bt.narrationDPath
   var symbols : List[(String,Term)] = Nil
   var defs : List[(String,Term)] = Nil
   var nots : List[(String,NotationContainer)] = Nil

   def doDocument(d: omdoc) {
      path = DPath(((URI.http colon "gl.mathhub.info") / "tps") )// / d._meta._metas.collectFirst{case title(s) => s}.getOrElse(bt.inFile.name))
      println("Document:" +path)
      val modsM = d._modules map doModule(path)
      val mrefsM = modsM.map(m => {controller.add(m) ;MRef(bt.narrationDPath, m.path,true)})
      val doc = new Document(bt.narrationDPath, mrefsM)
      // d._meta._metas.foreach(x => new MetaDatum(GlobalName(,LocalName(x.getClass.getSimpleName)),x._s))
      index(doc)
   }

   def doName(s:String) : LocalName = LocalName(s)

   def doTheoryName(s:String) : MPath = {
      //println("Regex match! "+s)
      val doctheory = """(.+).omdoc#(.+)""".r
      val docdoc = """logics/(.+)""".r
      s match {
         case doctheory(doc,th) => if (doc==th) path ? th else doc match {
            case docdoc(th2) => (path / LocalName("tpslogic")) ? th2
         }
         case _ =>
            println(" regex fail!")
            sys.exit
      }
   }

   def NewName(s:String,start:Int = 1)(implicit th:DeclaredTheory) : LocalName = {
      if (!th.declares(doName(s))) doName(s)
      else if (!th.declares(doName(s + "_" + start))) doName(s + "_" + start)
      else NewName(s, start + 1)
   }

   def doModule(d:DPath)(m: syntax.Module): modules.Module = m match {
      case t: theory =>
         val cont = Nil // (t.theory_formals map doFormalPars) collect {case Some(v) => v}
      implicit val th = new DeclaredTheory(path,doName(t.id),Some(TPSTheory.thpath),cont)
         // TODO: assuming, exporting_, possibly named stuff?
         t._decls map doDecl
         symbols map (c => {
            val name = doName(c._1)
            val tp = c._2
            val OptDef = defs.collectFirst{case (n,d) if n==c._1 => d}
            val OptNot = nots.collectFirst{case (n,d) if n==c._1 => d}
            th add Constant(th.toTerm,name,None,Some(tp),OptDef,None,OptNot.getOrElse(NotationContainer()))
         })

         println(" -- Theory: "+th)
         th
      case _ =>
         println(" -- OTHER: "+m.getClass)
         sys.exit
   }

   def doDecl(d:Declaration)(implicit th : DeclaredTheory): Unit = d match {
      case imports(from) => th add PlainInclude(doTheoryName(from),th.path)
      case symbol(name,meta,tp) => // TODO metadata
         symbols::=(name,doObject(tp._OMOBJ._obj))
      case definition(tp,id,for_,obj) => defs::=(for_,doObject(obj._obj))
      case notation(proto,rendering) => proto match {
         case syntax.OMS(_,p) => nots::=(p,doNotation(rendering))
         case syntax.OMA(syntax.OMS(_,p),_) => nots::=(p,doNotation(rendering))
         case _ =>
      }
      case assertion(id,tp,cmp,_def) => th add Constant(th.toTerm,doName(id),None,Some(
      Apply(TPSTheory.proof.term,doObject(_def._obj))
      ),None,None)
      case _ => println("TODO Decl: "+d.getClass.getSimpleName)
               sys.exit
   }

   def doObject(o: omobject) : Term = o match {
      case syntax.OMA(head,pars) => ApplySpine(doObject(head),pars map doObject :_*)
      case syntax.OMS(cd, name) => objects.OMS((path ? cd) ? doName(name))
      case syntax.OMV(name) => objects.OMV(doName(name))
      case syntax.OMBIND(s,vars,pars) =>
         val con : Context = vars map (v => v match {
            case syntax.OMV(name) => VarDecl(doName(name),None,None,None)
            case syntax.OMATTR(atp,v) => VarDecl(doName(v.name),Some(doObject(atp._par)),None,None)
         })
         Apply(doObject(s),Lambda(con,doObject(pars)))
      case _ => println("TODO Object: "+o.getClass.getSimpleName)
         sys.exit
   }

   def doNotation(r:rendering) : NotationContainer = {
      val prec = r.prec
      val pattern : List[Marker] = r._some match {
         case syntax.mo(s) => List(WordMarker(s))
         case mrow(ls) => ls map (_ match {
            case syntax.mo(s) => WordMarker(s)
            case s: syntax.render => Arg(s.num.getOrElse(throw new Exception("Error during notation parsing")))
         })}
      val t=TextNotation(Precedence.integer(prec),None)(pattern :_*)
      NotationContainer(t)
   }

/*

   def doDocument(d: pvs_file) {
      println("Document:" +bt.narrationDPath)
      val modsM = d._modules map doModule(path)
      val mrefsM = modsM.map(m => {controller.add(m) ;MRef(bt.narrationDPath, m.path,true)})
      val doc = new Document(bt.narrationDPath, mrefsM)
      index(doc)
   }


*/

}
