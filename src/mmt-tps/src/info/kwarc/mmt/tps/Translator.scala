package info.kwarc.mmt.tps

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.notations._
import info.kwarc.mmt.api.symbols.{Constant, ContextContainer, PlainInclude}
import info.kwarc.mmt.tps.syntax._
import info.kwarc.mmt.api._
import documents._
import modules._
import utils._
import archives._
import info.kwarc.mmt.api.checking.{Checker, CheckingEnvironment, RelationHandler}
import info.kwarc.mmt.api.objects.{Context, Term, VarDecl}
import info.kwarc.mmt.api.opaque.{OpaqueText, StringFragment}
import info.kwarc.mmt.lf._

class TPSImportTask(controller: Controller, bt: BuildTask, index: Document => Unit) extends MMTTask {
   var path : DPath = bt.narrationDPath
   var imports : List[String] = List("simpletypes")
   var symbols : List[(String,Term)] = Nil
   var defs : List[(String,Term)] = Nil
   var nots : List[(String,NotationContainer)] = Nil
   var vars : List[LocalName] = Nil
   var boundvars : List[LocalName] = Nil

   def doDocument(d: omdoc) : BuildResult = {
      path = DPath((URI.http colon "gl.mathhub.info") / "tps" )// / d._meta._metas.collectFirst{case title(s) => s}.getOrElse(bt.inFile.name))
      val doc = new Document(bt.narrationDPath)
      controller add doc
      val modsM = d._modules map doModule(path)
      modsM.foreach(m => {controller add MRef(bt.narrationDPath, m.path)})
      // d._meta._metas.foreach(x => new MetaDatum(GlobalName(,LocalName(x.getClass.getSimpleName)),x._s))
      val checker = controller.extman.get(classOf[Checker], "mmt").getOrElse {
         throw GeneralError(s"no MMT checker found")
      }
      modsM foreach (t => checker(t)(new CheckingEnvironment(controller.simplifier, new ErrorLogger(controller.report), RelationHandler.ignore,this)))
      index(doc)
      BuildResult.empty
   }

   def doName(s:String) : LocalName = LocalName(s.replace("-","_"))

   def doTheoryName(s:String)(implicit th: Theory) : MPath = {
      //println("Regex match! "+s)
      val doctheory = """(.+).omdoc#(.+)""".r
      val docdoc = """logics/(.+)""".r
      s match {
         case doctheory(doc,th) => if (doc==th) path ? th else doc match {
            case docdoc(th2) =>
               //println("Theory: "+(path / LocalName("tpslogic")) ? th2)
               (path / LocalName("tpslogic")) ? th2
         }
         case _ =>
            val ths = List("simpletypes","truthval","lambda-calc","pl0","ind","sthold","sthol")
            if(ths.contains(s)) {
               if (!imports.contains(s)) {
                  imports ::= s
                  controller add PlainInclude((path / LocalName("tpslogic")) ? s,th.path)
               }
               (path / LocalName("tpslogic")) ? s
            }
            else if (s.startsWith("tps."))  path ? s
            else {
               println("Theory Name: "+s)
               sys.exit()
               // (path / LocalName("tpslogic")) ? s
            }
      }
   }

   def NewName(s:String,start:Int = 1)(implicit th: Theory) : LocalName = {
      if (!th.declares(doName(s))) doName(s)
      else if (!th.declares(doName(s + "_" + start))) doName(s + "_" + start)
      else NewName(s, start + 1)
   }

   def doModule(d:DPath)(m: info.kwarc.mmt.tps.syntax.Module): modules.Module = m match {
      case t: theory =>
         val cont = Nil // (t.theory_formals map doFormalPars) collect {case Some(v) => v}
      implicit val th = new Theory(path,doName(t.id),Some(TPSTheory.thpath),ContextContainer(cont), Theory.noBase)
         // TODO: assuming, exporting_, possibly named stuff?
         // TODO theory context
         controller add th
         t._decls foreach doDecl
         symbols foreach (c => {
            val name = doName(c._1)
            val tp = c._2
            val OptDef = defs.collectFirst{case (n,d) if n==c._1 => d}
            val OptNot = nots.collectFirst{case (n,d) if n==c._1 => d}
            val c2 = Constant(th.toTerm,name,Nil,Some(tp),OptDef,None,OptNot.getOrElse(NotationContainer()))
            vars = Nil
            //println(c2)
            controller add c2
         })

         // println(" -- Theory: \n"+th)
         th
      case _ =>
         println(" -- OTHER: "+m.getClass)
         sys.exit()
   }

   def doDecl(d:Declaration)(implicit th: Theory): Unit = d match {
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
         val descr = meta._metas collect {case description(s) => s}
         if (descr.nonEmpty) controller add new OpaqueText(th.path.toDPath,OpaqueText.defaultFormat, StringFragment(descr.head))

      case definition(tp,id,for_,obj) =>
         val dfterm = doObject(obj._obj)
         val newdf = vars.distinct.foldLeft(dfterm.asInstanceOf[Term])((t,s) => Lambda(s,TPSTheory.tp.term,t))
         defs::=(for_,newdf)
         boundvars = Nil
      case imports(from) =>
         val fullpath = doTheoryName(from)
         if (!(path / LocalName("tpslogic") <= fullpath)) controller add PlainInclude(fullpath,th.path)
      case notation(proto,rendering) => proto match {
         case syntax.OMS(_,p) => nots::=(p,doNotation(rendering))
         case syntax.OMA(syntax.OMS(_,p),_) => nots::=(p,doNotation(rendering))
         case _ =>
      }
      case assertion(id,tp,cmp,fmp) =>
         if (!imports.contains("pl0")) {
            imports ::= "pl0"
            controller add PlainInclude((path / LocalName("tpslogic")) ? "pl0",th.path)
         }
         val tpterm = vars.distinct.foldLeft[Term](Apply(TPSTheory.ded.term,doObject(fmp._obj)))((t,s)
            => Pi(s,TPSTheory.tp.term,t))
         controller add new OpaqueText(th.path.toDPath, OpaqueText.defaultFormat, StringFragment(cmp))
         symbols ::= ((id,tpterm))
         boundvars = Nil
      case _ => println("TODO Decl: "+d.getClass.getSimpleName + "\n" + d)
               sys.exit()
   }

   def doObject(o: omobject)(implicit th: Theory) : Term = o match {
      case syntax.OMBIND(s,lvars,pars) =>
         val con : Context = lvars map (_ match {
            case syntax.OMV(name) =>
               val realname = doName(name)
               // boundvars::=realname
               VarDecl(realname)
            case syntax.OMATTR(atp,v) =>
               val realname = doName(v.name)
               // boundvars::=realname
               VarDecl(realname, Apply(TPSTheory.tm.term,doObject(atp._par)))
         })
         Apply(doObject(s),Lambda(con,doObject(pars)))
         /*
      case syntax.OMA(head,pars) => head match {
         case syntax.OMS("simpletypes","funtype") => ApplySpine(doObject(head),pars map doObject :_*)
         case _ =>
            println("TODO Object Application : " + head + " to " + pars)
            sys.exit
      }
      */
      case syntax.OMS(cd, name) =>
         //println(doTheoryName(cd) ? doName(name))
         objects.OMS(doTheoryName(cd) ? doName(name))
      case syntax.OMV(name) =>
         val realname = doName(name)
         if (!(boundvars contains realname)) vars::=realname
         objects.OMV(realname)
      case syntax.OMA(head, pars) =>
         ApplySpine(doObject(head),pars.map(doObject):_*)
      case _ => println("TODO Object: "+o.getClass.getSimpleName + "\n" + o)
         sys.exit()
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
