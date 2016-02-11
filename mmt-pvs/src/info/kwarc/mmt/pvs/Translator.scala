package info.kwarc.mmt.pvs

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.presentation.MMTSyntaxPresenter
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

class PVSImportTask(controller: Controller, bt: BuildTask, index: Document => Unit) {

   val path = bt.narrationDPath.^!.^!
   /*
   implicit val sb = new presentation.StringBuilder
   val presenter = new MMTSyntaxPresenter
   controller.extman.addExtension(presenter)
   */
   object State {

      private var includes : Set[MPath] = Set()
      private var pars : List[(LocalName,Term,Boolean)] = Nil
      private var vardecls : List[(LocalName,Term)] = Nil
      private var tccs : List[Term] = Nil
      private var conversions : List[(String,Term)] = Nil

      var th : DeclaredTheory = null
      var usedvars : List[LocalName] = Nil
      var boundvars : List[LocalName] = Nil

      var rectp : Term = null

      def reset(t:DeclaredTheory) = {
         includes = Set()
         pars = Nil
         vardecls = Nil
         tccs = Nil
         usedvars = Nil
         boundvars = Nil
         conversions = Nil
         th = t
      }

      def parameters = pars.map(p => (p._1,p._2)):::vardecls

      def addinclude(path : MPath) = if (!th.getIncludes.contains(path) && th.path!=path) {
         includes+=path
         // println("Include: " + path)
         th add PlainInclude(path,th.path)
      }
      def addparameter(name : LocalName, tp : Term, isLF : Boolean = false) = {
         // println("Parameter: " + name + ": " + tp)
         pars ::=(name, tp, isLF)
      }
      def addvardecl(name : LocalName, tp : Term) = {
         // println("Variable: " + name + ": " + tp)
         vardecls::=(name,tp)
      }

      def addtcc(t : Term) = {
         // println("TCC: " + controller.presenter.asString(t))// .objectLevel.asString(t))
         tccs::=t
      }

      def addconversion(kind:String,expr:Term) = conversions::=(kind,expr) // TODO and then?

      def addconstant(name : LocalName, args: List[(LocalName,Term)], giventype : Term, inferredType : Option[Term],
                      defOpt : Option[Term]): Unit = {

         var actualdef = if (inferredType.isDefined && inferredType.get!=giventype) defOpt.map(
            d => PVSTheory.asType(inferredType.get,giventype,d,tccs.headOption.getOrElse(PVSTheory.unknown.term))
         ) else defOpt
         if (tccs.length>1) {
            println("Too many tccs in " + name + ": " + tccs.length)
            sys.exit
         }

         usedvars = usedvars.filter(!boundvars.contains(_))
         val localvars = usedvars.map(v => vardecls.find(p => p._1==v)).collect(
            {case Some(p) => p}).sortBy(vardecls.indexOf(_))

         boundvars = Nil
         usedvars = Nil
         tccs = Nil

         if(localvars.nonEmpty) actualdef =
           actualdef.map(PVSTheory.lambda(localvars.map(p => VarDecl(p._1,Some(p._2),None,None)),_,giventype))
         var actualtype = giventype
         if (localvars.nonEmpty) actualtype = localvars.foldRight(actualtype)((p,t) => PVSTheory.fun_type(p._2,t))

         if (args.nonEmpty) {
            actualdef = actualdef.map(PVSTheory.lambda(args.map(p => VarDecl(p._1,Some(p._2),None,None)),_,giventype))
            actualtype = args.foldRight(actualtype)((p,tp) => PVSTheory.fun_type(p._2,tp))
         }

         actualtype = PVSTheory.expr(actualtype)

         if (pars.nonEmpty) {
            actualdef = actualdef.map(Lambda(pars.map(p => VarDecl(p._1,Some(if(p._3) p._2 else PVSTheory.expr(p._2)),None,None)),_))
            actualtype = Pi(pars.map(p => VarDecl(p._1,Some(if(p._3) p._2 else PVSTheory.expr(p._2)),None,None)),actualtype)
         }

         th add Constant(th.toTerm,name,None,Some(actualtype),actualdef,None)
         // println(th.getConstants.head)
      }

      def addprop(name : LocalName, formula : Term,kind : String): Unit = {

         usedvars = usedvars.filter(!boundvars.contains(_))
         val localvars = usedvars.map(v => vardecls.find(p => p._1==v)).collect(
            {case Some(p) => p}).sortBy(vardecls.indexOf(_))

         if (tccs.nonEmpty) {
           // println("TCC for Formula: " + tccs.length)
         }

         boundvars = Nil
         usedvars = Nil
         tccs = Nil // TODO tccs in formulae?

         var actualtype = formula
         if (localvars.nonEmpty) actualtype = PVSTheory.forall(localvars.map(p => VarDecl(p._1,Some(p._2),None,None)),actualtype)


         actualtype = PVSTheory.proof(kind,actualtype)

         if (pars.nonEmpty) {
            actualtype = Pi(pars.map(p => VarDecl(p._1,Some(if(p._3) p._2 else PVSTheory.expr(p._2)),None,None)),actualtype)
         }

         th add Constant(th.toTerm,name,None,Some(actualtype),None,None)
      }
   }

   def doSourceRef(o: Object, oM: Term) = {
      if (o.place!="") {
         val List(bR, bC, eR, eC) = stringToList(o.place, " ").map(_.toInt)
         val reg = SourceRegion(SourcePosition(-1, bR, bC), SourcePosition(-1, eR, eC))
         SourceRef.update(oM, SourceRef(FileURI(bt.inFile), reg))
      }
   }

   def doDocument(d: pvs_file) {
      // println("Document:" +bt.narrationDPath)
      val modsM = d._modules map doModule(path)
      val mrefsM = modsM.map(m => {controller.add(m) ;MRef(bt.narrationDPath, m.path)})
      // val doc = new Document(bt.narrationDPath, true, mrefsM)
      // index(doc)
   }

   def doModule(d:DPath)(m: syntax.Module): modules.Module = m match {
      case t: theory =>
         val cont = Context() // (t.theory_formals map doFormalPars) collect {case Some(v) => v}
         implicit val th = new DeclaredTheory(path,doName(t.named.id),Some(PVSTheory.thpath),cont)

         State.reset(th)
         t.theory_formals foreach doFormal
         t.assuming foreach doAssumption
         // TODO: assuming, exporting_, possibly named stuff?
         t._decls foreach doDecl

         // println(" -- Theory")

         // presenter(th)
         // println(sb.get)

         th
      case d:datatype =>
         println(" -- Datatype: "+d.body.named.id)
         implicit val th = new DeclaredTheory(path,doName(d.body.named.id),Some(PVSTheory.thpath))
         println("TODO: Datatypes!")
         sys.exit
         // TODO !
      case _ =>
         println(" -- OTHER: "+m.getClass)
         sys.exit
   }

   def doAssumption (ad:AssumingDecl) : Unit = ad match {
      case _ => println("TODO Assumption: "+ad.getClass); sys.exit
   }

   def doFormal(f:FormalParameter) = f match {
      case formal_type_decl(named,ne) => State.addparameter(newName(named.named.id),PVSTheory.tp.term,true)
      case formal_subtype_decl(named,_,sup) => State.addparameter(newName(named.named.id),
         PVSTheory.subtp(doType(sup._internal)),true)
         // TODO how do I introduce subtyping conditions?
      case _ => println("TODO Formal: " + f.getClass + ": " + f); sys.exit
   }

   // TODO: add parameters everywhere!

   def doDecl(d: Decl) {
      // println(d)
      val ret = d match {
         case var_decl(id,unnamed,tp) => State.addvardecl(doName(id),doType(tp._internal))
         case tcc_decl(named,assertion) => State.addtcc(doExpr(assertion._formula)._1)
         case const_decl(named,arg_formals,tp,optdef) =>
            // println(d)
            val name = newName(named.named.id)
            State.rectp = doType(tp._declared)
            val (defi,fulltp) = if (optdef.isDefined) {val (a,b) = doExpr(optdef.get);(Some(a),Some(b))} else (None,None)

            State.addconstant(
               name,
               arg_formals.flatMap(_._bindings.map(b => (newName(b.id),doType(b._type)))),
               State.rectp,
               fulltp,
               defi
            )
         case formula_decl(named,assertion) =>
            State.addprop(newName(named.named.id),doExpr(assertion._formula)._1,assertion.kind)
         case conversion_decl(_,kind,expr) => State.addconversion(kind,doExpr(expr)._1)
         case def_decl(named,arg_formals,tp,df,optMeasure,optOrder) =>
            val name = newName(named.named.id)
            val rettype = doType(tp._declared)
            val (defi,fulltp) = doExpr(df)
            // TODO optMeasure, optOrder
            State.addconstant(
               name,
               arg_formals.flatMap(_._bindings.map(b => (newName(b.id),doType(b._type)))),
               rettype,
               Some(fulltp),
               Some(defi)
            )
         case application_judgement(named,nameexpr,args,tp) =>
            val name = newName(named.id.getOrElse("App_Judgment"))
            var (exp,restp) = doExpr(args.flatMap(_._bindings).foldLeft(nameexpr.asInstanceOf[Expr])((e,b) =>
               application("",e,varname_expr("",b.id,b._type),false)))
            restp = PVSTheory.forall(args.flatMap(_._bindings).map(b =>
               VarDecl(doName(b.id),Some(doType(b._type)),None,None)),
               PVSTheory.typJudg(exp,restp,doType(tp._internal)))
            State.boundvars :::= args.flatMap(_._bindings).map(b => doName(b.id))
            State.addprop(name,restp,"internal_judgment")

          /*
         case application_judgement(named,nameexpr,argformals,tp) =>
            val name = newName(named.id.getOrElse("app_judgement"))
            val fun = doExpr(nameexpr)
            val returntype = doType(tp._internal)
            val pars = argformals.flatMap(_._bindings.map(b => (newName(b.id),doType(b._type))))
            vars++= pars
            List(Constant(th.toTerm,name,None,Some(parameters.universalizetp(
               PVSTheory.subtp(PVSTheory.PVSapply(fun,pars.map(p => OMV(p._1))),returntype))),None,None))
         */

         case _ => println("TODO Decl: " + d.getClass + ": " + d); sys.exit
      }
      // TODO : tccs
      //ret.foreach(th add _)
   }

   def doType(t: Type): Term = {
      val tM: Term = t match {
         case type_name(_,name,res) => doPath(name,res)
         case tuple_type(_,doms:List[Type]) => PVSTheory.tuple_type(doms map doType)
         case function_type(_,from,to) =>
            from match {
               case binding(id,named,tp) => ???
               case t: Type => PVSTheory.fun_type(doType(t),doType(to))
            }
         case expr_as_type(_,expr,optType) =>
            val (e,tp) = doExpr(expr)
            PVSTheory.expr_as_type(e,tp)// Not quite sure about this...
         case _ => println("TODO Type: " + t.getClass + ": " + t); sys.exit
      }
      doSourceRef(t, tM)
      tM
   }
   // Tries to do type reconstruction as well
   def doExpr(e: Expr): (Term,Term) = {
      val eM: (Term,Term) = e match {
         case forall_expr(_,bindings,body) =>
            val bd = doExpr(body)._1
            val con = Context(bindings.map(b => VarDecl(newName(b.id),Some(doType(b._type)),None,None)):_*)
            State.boundvars:::=con.map(_.name)
            (PVSTheory.forall(con,bd),PVSTheory.prop.term)
         case application(_,f,arg,_) =>
            val (tmf,tpf) = doExpr(f)
            val (tmarg,tparg) = doExpr(arg)
            val tptarget = tpf match {
               case PVSTheory.fun_type(a,b) => b
               case _ => PVSTheory.unknown.term // TODO Unkown Type!
            }
            (PVSTheory.apply(tmf,tmarg)(tparg,tptarget),tptarget)
         case name_expr(_,name,optp,res) =>
            (doPath(name,Some(res)),optp.map(doType).getOrElse(PVSTheory.unknown.term))
         case lambda_expr(_,bindings,body) =>
            val (bd,tptarget) = doExpr(body)
            val con = Context(bindings.map(b => VarDecl(doName(b.id),Some(doType(b._type)),None,None)):_*)
            State.boundvars:::=con.map(_.name)
            (PVSTheory.lambda(con,bd,tptarget),con.foldRight(tptarget)((v,t) => PVSTheory.fun_type(v.tp.get,t)))
         case tuple_expr(_,args) => PVSTheory.tuple_expr(args map doExpr)
         case varname_expr(_,id,tp) =>
            State.usedvars ::= newName(id)
            (OMV(newName(id)),doType(tp))
         case cases_expr(_,expr,sels) =>
            val (t,tp) = doExpr(expr)
            val cases = sels.map(s => {
               val (cons,casetp) = doExpr(s._cons)
               PVSTheory.selection(
                  cons,
                  s.bindings.map(b => VarDecl(doName(b.id),Some(doType(b._type)),None,None)),
                  doExpr(s._expr)._1,
                  casetp
               )
            })
            (PVSTheory.pvsmatch(t,tp,cases,State.rectp),State.rectp)
         case field_appl_expr(_,id,expr) =>
            (PVSTheory.fieldapp(doExpr(expr)._1,id),PVSTheory.unknown.term) // TODO Unknown Type!
         case _ => println("TODO Expr: " + e.getClass + ": " + e); sys.exit
      }
      doSourceRef(e, eM._1)
      eM
   }

   def doObject(o:Object) : Term = o match {
      case tp: Type => doType(tp)
      case e: Expr => doExpr(e)._1
   }

   def doName(s:String) : LocalName = LocalName(s)

   def newName(s:String,start:Int = 1) : LocalName = {
      if (!State.th.declares(doName(s))) doName(s)
      else if (!State.th.declares(doName(s + "_" + start))) doName(s + "_" + start)
      else newName(s, start + 1)
   }

   def doPath(n:name,res:Option[resolution]) : Term = {
      val (id,thid,library_id,mappings,opttarget,allactuals) = (n,res) match {
         case (name(id1,_,_,_,_,_,_),Some(resolution(theory_name(_,thid1,library_id1,mappings1,opttarget1,actuals,dactuals),ind))) =>
            (id1+(if(ind>0) "_"+ind else ""),thid1,library_id1,mappings1,opttarget1,actuals:::dactuals)
         case (name(id1,thid1,library_id1,mappings1,opttarget1,actuals,dactuals),None) =>
            (id1,thid1,library_id1,mappings1,opttarget1,actuals:::dactuals)
      }
      /*
      println("id        : "+id)
      println("thid      : "+thid)
      println("library id: "+library_id)
      println("path      : "+path)
      println("mappings  : "+mappings)
      println("target    : "+opttarget)
      println("actuals   : - "+allactuals.head)
      allactuals.tail.foreach( a =>
         println("            - "+a)
      )
      */
      val doc = DPath((URI.http colon "pvs.csl.sri.com") / (if (library_id=="") "Prelude" else  library_id))
      if(mappings.nonEmpty) {
         println("Found mappings in doPath")
         sys.exit
      }
      if(opttarget.isDefined) {
         println("Found target in doPath")
         sys.exit
      }
      if(doc ? thid == State.th.path && State.parameters.exists(p => p._1==LocalName(id))) OMV(id)
      else {
         State.addinclude(doc ? thid)
         val sym = OMS((doc ? thid) ? id)
         // TODO apply theory parameters ?
         if (allactuals.nonEmpty) ApplySpine(sym, allactuals map (a => doObject(a)): _*) else sym
      }
   }

}
