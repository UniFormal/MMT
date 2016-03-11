package info.kwarc.mmt.tps

import java.io.{FileWriter, PrintWriter}

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
   var vars : List[LocalName] = Nil
   var boundvars : List[LocalName] = Nil

   def doDocument(d: omdoc) : BuildResult = {
      path = DPath((URI.http colon "gl.mathhub.info") / "tps" )// / d._meta._metas.collectFirst{case title(s) => s}.getOrElse(bt.inFile.name))
      val modsM = d._modules map doModule(path)
      val mrefsM = modsM.map(m => {controller.add(m) ;MRef(bt.narrationDPath, m.path)})
      val doc = new Document(bt.narrationDPath, true)
      // d._meta._metas.foreach(x => new MetaDatum(GlobalName(,LocalName(x.getClass.getSimpleName)),x._s))
      index(doc)
      BuildResult.empty
   }

   def doName(s:String) : LocalName = LocalName(s.replace("-","_"))

   def doTheoryName(s:String) : MPath = {
      //println("Regex match! "+s)
      val doctheory = """(.+).omdoc#(.+)""".r
      val docdoc = """logics/(.+)""".r
      s match {
         case doctheory(doc,th) => if (doc==th) path ? th else doc match {
            case docdoc(th2) =>
               println("TH: "+(path / LocalName("tpslogic")) ? th2)
               (path / LocalName("tpslogic")) ? th2
         }
         case _ =>
            val ths = List("simpletypes","truthval","lambda-calc","pl0","ind","sthold","sthol")
            if(ths.contains(s)) (path / LocalName("tpslogic")) ? s
            else if (s.startsWith("tps."))  path ? s
            else {
               println("Theory Name: "+s)
               sys.exit
               // (path / LocalName("tpslogic")) ? s
            }
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
         t._decls foreach doDecl
         symbols foreach (c => {
            val name = doName(c._1)
            val tp = c._2
            val OptDef = defs.collectFirst{case (n,d) if n==c._1 => d}
            val OptNot = nots.collectFirst{case (n,d) if n==c._1 => d}
            val c2 = Constant(th.toTerm,name,Nil,Some(tp),OptDef,None,OptNot.getOrElse(NotationContainer()))
            vars = Nil
            println(c2)
            th add c2
         })

         println(" -- Theory: \n"+th)
         th
      case _ =>
         println(" -- OTHER: "+m.getClass)
         sys.exit
   }

   def doDecl(d:Declaration)(implicit th : DeclaredTheory): Unit = d match {
       /*
      case assertion(id,tp,cmp,_def) => th add Constant(th.toTerm,doName(id),None,Some(
      Apply(TPSTheory.proof.term,doObject(_def._obj))
      ),None,None)
      */
      case symbol(name,meta,tp) => // TODO metadata
         val tpterm = Apply(TPSTheory.tm.term,doObject(tp._OMOBJ._obj))
         val newtp = vars.distinct.foldLeft(tpterm.asInstanceOf[Term])((t,s) => Pi(s,TPSTheory.tp.term,t))
         symbols::=(name,newtp)
         boundvars = Nil
      case definition(tp,id,for_,obj) =>
         val dfterm = doObject(obj._obj)
         val newdf = vars.distinct.foldLeft(dfterm.asInstanceOf[Term])((t,s) => Lambda(s,TPSTheory.tp.term,t))
         defs::=(for_,newdf)
         boundvars = Nil
      case imports(from) => th add PlainInclude(doTheoryName(from),th.path)
      case notation(proto,rendering) => proto match {
         case syntax.OMS(_,p) => nots::=(p,doNotation(rendering))
         case syntax.OMA(syntax.OMS(_,p),_) => nots::=(p,doNotation(rendering))
         case _ =>
      }
      case _ => println("TODO Decl: "+d.getClass.getSimpleName + "\n" + d)
               sys.exit
   }

   def doObject(o: omobject) : Term = o match {


      case syntax.OMBIND(s,lvars,pars) =>
         val con : Context = lvars map (_ match {
            case syntax.OMV(name) =>
               val realname = doName(name)
               // boundvars::=realname
               VarDecl(realname,None,None,None)
            case syntax.OMATTR(atp,v) =>
               val realname = doName(v.name)
               // boundvars::=realname
               VarDecl(realname,Some(Apply(TPSTheory.tm.term,doObject(atp._par))),None,None)
         })
         Apply(doObject(s),Lambda(con,doObject(pars)))

      case syntax.OMA(head,pars) => head match {
         case syntax.OMS("simpletypes","funtype") => ApplySpine(doObject(head),pars map doObject :_*)
         case _ =>
            println("TODO Object Application : " + head + " to " + pars)
            sys.exit
      }
      case syntax.OMS(cd, name) =>
         println(doTheoryName(cd) ? doName(name))
         objects.OMS(doTheoryName(cd) ? doName(name))
      case syntax.OMV(name) =>
         val realname = doName(name)
         if (!(boundvars contains realname)) vars::=realname
         objects.OMV(realname)
      case _ => println("TODO Object: "+o.getClass.getSimpleName + "\n" + o)
         sys.exit
   }

   def doNotation(r:rendering) : NotationContainer = {
      val prec = r.prec
      val pattern : List[Marker] = r._some match {
         case syntax.mo(s) => List(WordMarker(s))
         case mrow(ls) => ls map (_ match {
            case syntax.mo(s) => WordMarker(s)
            case s: syntax.render => SimpArg(s.num.getOrElse(throw new Exception("Error during notation parsing")))
         })}
      val t=TextNotation(Mixfix(pattern),Precedence.integer(prec),None)
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
