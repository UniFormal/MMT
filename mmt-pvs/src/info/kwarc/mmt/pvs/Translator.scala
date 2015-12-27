package info.kwarc.mmt.pvs

import info.kwarc.mmt.api.frontend.Controller
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

   // var tempcont: Context = Context()
   // var counter = 0
   var includes : List[MPath] = Nil
   var tccs : List[tcc_decl] = Nil
   var vars : Set[(LocalName,Term)] = Set()

   val path = bt.narrationDPath.^!.^!

   case class thassignment(p:MPath,l:List[Term])

   object parameters {
      var pars : List[(LocalName,Term,Boolean)] = Nil
      // var theoryassignments : List[thassignment] = Nil

      // adds a new parameter of the current theory. isLFType is true, if the object is an LF type (as opposed to a PVS type)
      def add(name:LocalName,tp:Term,isLFType:Boolean) = pars = pars:::List((name,tp,isLFType)).distinct
      // def addtheory(t:MPath,l:List[Term]) = theoryassignments = (thassignment(t,l)::theoryassignments).distinct

      // def apply(t:thassignment,tm:Term) = ApplySpine(tm, t.l :_*)

      // def apply(t:Term) = ApplySpine(t,pars map (p => OMV(p._1)) :_*)

      def clear = pars = Nil

      def universalizetp(t:Term) : Term = if (pars.nonEmpty) {
         Pi(pars.map(p => VarDecl(p._1,Some(
            if (p._3) p._2 else
            OMS(if (p._2==PVSTheory.tp.term) PVSTheory.tp.path else PVSTheory.expr.path)
         ),None,None)),
         {
            val nontps = pars.filter(p => p._2!=PVSTheory.tp.term && !p._3)
            if (nontps.isEmpty) t else Arrow(nontps.map(p => PVSTheory.ofType(OMV(p._1),p._2)),t)
         } )
      } else t

      def universalizeexpr(t:Term) : Term = if (pars.nonEmpty) {
         Lambda(pars.map(p => VarDecl(p._1,Some(
            if (p._3) p._2 else
            OMS(if (p._2==PVSTheory.tp.term) PVSTheory.tp.path else PVSTheory.expr.path)
         ),None,None)),
         {
            val nontps = pars.filter(p => p._2!=PVSTheory.tp.term && !p._3)
            if (nontps.isEmpty) t else Arrow(nontps.map(p => PVSTheory.ofType(OMV(p._1),p._2)),t)
         } )
      } else t

   }


   def doSourceRef(o: Object, oM: Term) = {
      if (o.place!="") {
         val List(bR, bC, eR, eC) = stringToList(o.place, " ").map(_.toInt)
         val reg = SourceRegion(SourcePosition(-1, bR, bC), SourcePosition(-1, eR, eC))
         SourceRef.update(oM, SourceRef(FileURI(bt.inFile), reg))
      }
   }

   def doDocument(d: pvs_file) {
      println("Document:" +bt.narrationDPath)
      val modsM = d._modules map doModule(path)
      val mrefsM = modsM.map(m => {controller.add(m) ;MRef(bt.narrationDPath, m.path)})
      // val doc = new Document(bt.narrationDPath, true, mrefsM)
      // index(doc)
   }

   def doModule(d:DPath)(m: syntax.Module): modules.Module = m match {
      case t: theory =>
         // counter = 0
         // parameters.clear
         val cont = Nil // (t.theory_formals map doFormalPars) collect {case Some(v) => v}
         implicit val th = new DeclaredTheory(path,doName(t.named.id),Some(PVSTheory.thpath),cont)
         includes::=th.path
         t. theory_formals map doFormal
         t.assuming map doAssumption
         // TODO: assuming, exporting_, possibly named stuff?
         t._decls map doDecl

         println(" -- Theory: "+th)
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

   def doAssumption (ad:AssumingDecl)(implicit th:DeclaredTheory) : Unit = ad match {
      case _ => println("TODO Assumption: "+ad.getClass); sys.exit
   }

   def doFormal(f:FormalParameter)(implicit th:DeclaredTheory) = f match {
      case formal_type_decl(named,ne) => parameters.add(newName(named.named.id),PVSTheory.tp.term,false)
      case formal_subtype_decl(named,_,sup) => parameters.add(newName(named.named.id),PVSTheory.tp.term,false)
         // TODO how do I introduce subtyping conditions?
      case _ => println("TODO Formal: "+f.getClass); sys.exit
   }

   // TODO: add parameters everywhere!

   def doDecl(d: Decl)(implicit th:DeclaredTheory) : Unit = {
      val ret = d match {
         case var_decl(id,unnamed,tp) => Nil
         case tcc:tcc_decl => tccs::=tcc ; Nil
         case const_decl(named,argformals,tp,defOpt) =>
            val name = newName(named.named.id)
            val formals = argformals.flatMap(_._bindings)
            val returntype = doType(tp._internal)
            val donedef = defOpt.map(doExpr(_))
            vars++= formals.map(b => (newName(b.id),doType(b._type)))
            val actualType = parameters.universalizeexpr(vars.foldRight(returntype)((v,t) => PVSTheory.arrow(v._2,t))) // I do currying here - should I?
            val actualdef = donedef.map(exp => parameters.universalizeexpr(vars.foldRight(exp)((v,t) => PVSTheory.lambda(v._1,v._2,t))))
            vars = Set()
            PVSTheory.constdecl(th,name.toString,actualType,actualdef)(parameters.universalizetp)
         case formula_decl(named,ass) =>
            val thm = PVSTheory.formula(ass.kind,doExpr(ass._formula))
            // TODO: add variables and stuff?
            List(Constant(th.toTerm,newName(named.named.id),None,Some(parameters.universalizetp(thm)),None,None))
         case conversion_decl(unnamed,kind,expr) => Nil
            // TODO: do something with those?
         case def_decl(named,arg_formals,tp,_def,optmeasure,optorder) => Nil // Definitely TODO!
         case application_judgement(named,nameexpr,argformals,tp) =>
            val name = newName(named.id.getOrElse("app_judgement"))
            val fun = doExpr(nameexpr)
            val returntype = doType(tp._internal)
            val pars = argformals.flatMap(_._bindings.map(b => (newName(b.id),doType(b._type))))
            vars++= pars
            List(Constant(th.toTerm,name,None,Some(parameters.universalizetp(
               PVSTheory.subtp(PVSTheory.PVSapply(fun,pars.map(p => OMV(p._1))),returntype))),None,None))

         case _ => println("TODO Decl: "+d.getClass); sys.exit
      }
      // TODO : tccs
      ret.foreach(th add _)
   }

   def doType(t: Type)(implicit th:DeclaredTheory): Term = {
      val tM: Term = t match {
         case function_type(_,from,to) =>
            from match {
               case binding(id,named,tp) => ???
               case t: Type => PVSTheory.arrow(doType(t),doType(to))
            }
         case type_name(_,name1,res) => doPath(name1,res)
         case tuple_type(_,doms:List[Type]) => PVSTheory.tptuple(doms map doType)

         case _ => println("TODO Type: "+t.getClass); sys.exit
      }
      doSourceRef(t, tM)
      tM
   }

   def doExpr(e: Expr)(implicit th:DeclaredTheory): Term = {
      val eM: Term = e match {
         case lambda_expr(_,bindings,body) =>
            val bd = doExpr(body)
            val usedvars = bindings.map(b => (newName(b.id),doType(b._type))).toSet
            vars = vars diff usedvars
            usedvars.foldRight(bd)((v,t) => PVSTheory.lambda(v._1,v._2,t))
         case forall_expr(_,bindings,body) =>
            val bd = doExpr(body)
            val usedvars = bindings.map(b => (newName(b.id),doType(b._type))).toSet
            vars = vars diff usedvars
            usedvars.foldRight(bd)((v,t) => PVSTheory.forall(v._1,v._2,t))
         case application(_,f,arg,_) => PVSTheory.PVSapply(doExpr(f),doExpr(arg))
         case name_expr(_,name,_,res) => doPath(name,Some(res)) // should I use the type for something?
         case tuple_expr(_,args) => PVSTheory.exprtuple(args map doExpr)
         case varname_expr(_,id,tp) =>
            vars+= ((newName(id),doType(tp)))
            OMV(newName(id))
         case cases_expr(_,expr,selections) => doExpr(expr) // Definitely TODO!
         case _ => println("TODO Expr: " + e.getClass); sys.exit
      }
      doSourceRef(e, eM)
      eM
   }

   def doObject(o:Object)(implicit th:DeclaredTheory) : Term = o match {
      case tp: Type => doType(tp)
      case e: Expr => doExpr(e)
   }

   def doName(s:String) : LocalName = LocalName(s)

   def newName(s:String,start:Int = 1)(implicit th:DeclaredTheory) : LocalName = {
      if (!th.declares(doName(s))) doName(s)
      else if (!th.declares(doName(s + "_" + start))) doName(s + "_" + start)
      else newName(s, start + 1)
   }

   def doPath(n:name,res:Option[resolution])(implicit th:DeclaredTheory) : Term = {
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
      if(doc ? thid == th.path && parameters.pars.exists(p => p._1==LocalName(id))) return OMV(id)
      if (!includes.contains(doc ? thid)) {
         includes ::=(doc ? thid)
         th add PlainInclude(doc ? thid,th.path)
      }
      val sym = OMS((doc ? thid) ? id)
      if (allactuals.nonEmpty) ApplySpine(sym,allactuals map (a => doObject(a)) :_*) else sym
   }

   /*


   def doAssumption(ad:AssumingDecl)(implicit th:DeclaredTheory) = ad match {
      case assumption(named,assert) => th add Constant(th.toTerm,NewName("ASSUME_"+named.named.id),None,
         Some(parameters.universalizetp(PVSTheory.formula.apply(doExpr(assert._formula)))),None,None)
         // TODO apply to everything?
      case d:Decl => doDecl(d)
   }

   def doFormal(f:FormalParameter)(implicit th:DeclaredTheory) = f match {
      case formal_type_decl(named,ne) => parameters.add(doName(named.named.id),OMS(PVSTheory.tp),false)
      case formal_subtype_decl(named,ne,sup) => parameters.add(doName(named.named.id),OMS(PVSTheory.tp),false)
         parameters.add(doName("FORMAL_SUBTYPE_DECL_"+counter),PVSTheory.ofType(OMV(doName(named.named.id)),doType(sup._declared)),true)
         counter+=1
        // TODO only here ._declared, because export wrong

      case formal_const_decl(named,tp) => parameters.add(doName(named.named.id),doType(tp._internal),false)

      case _ => println("TODO: Formal "+f.getClass); sys.exit
   }

   def doDecl(d: Decl)(implicit th:DeclaredTheory) : Unit = {
      d match {
         case var_decl(id,unnamed,tp2) => null
         case tcc_decl(named,assertion) => null

         case const_decl(named,arg_formals,typ,_def) =>
            val actualname = NewName(named.named.id)
            val args = arg_formals.flatMap(_._bindings.map(b => (doName(b.id),b._type)))
            val realtype = //parameters.universalizeexpr(functype(arg_formals.flatMap(bs => bs._bindings.map(b => doType(b._type))),doType(typ._internal)))
               parameters.universalizeexpr(doType(typ._internal))
            val s = constdecl(doName(actualname+"_INTERNAL"),th.path)
            s add Constant(s.toTerm,doName("PVSType"),None,Some(
               parameters.universalizetp(OMS(PVSTheory.tp))
            ),Some(realtype),None)
            var deftp : Term = parameters.universalizetp(OMS(PVSTheory.expr))
            if (_def.isDefined) {
               val cont = doFormals(arg_formals)
               val realdef = parameters.universalizeexpr(
                  if (cont.nonEmpty) PVSLambda(cont,doExpr(_def.get)) else doExpr(_def.get))
               s add Constant(s.toTerm,doName("def"),None,Some(deftp),Some(realdef),None)
            }
            th add s
            val defname : Term = OMS((th.path / s.name) ? doName("def"))
            th add Constant(th.toTerm,actualname,None,Some(deftp),Some(defname),None)

         case formula_decl(named,assertion) =>
            val form = doExpr(assertion._formula)
            val c = Constant(OMID(th.path),NewName(named.named.id),None,Some(parameters.universalizetp(Apply(OMID(PVSTheory.formula),
                  if (tempcont.nonEmpty) forall(tempcont.distinct,form) else form
               ))),None,None)
            tempcont = Context()
            th add c

         case conversion_decl(unnamed,kind,_expr) => println("TODO conversion_decl!")

         case def_decl(named,arg_formals,tp1,_def,measure,order) =>
            val actualname = NewName(named.named.id)
            val argcont = arg_formals.flatMap(_._bindings.map(b => VarDecl(doName(b.id),Some(doType(b._type)),None,None)))
            val s = PVSTheory.recdef(NewName(actualname+"_INTERNAL"),th.path)
            s add Constant(s.toTerm,doName("PVStype"),None,Some(parameters.universalizetp(OMS(PVSTheory.tp))),
               Some(parameters.universalizeexpr(doType(tp1._internal))),None)
            if(measure.isDefined) s add Constant(s.toTerm,doName("measure"),None,Some(parameters.universalizetp(OMS(PVSTheory.expr))),
               Some(parameters.universalizeexpr(doExpr(measure.get))),None)
            val defterm = parameters.universalizeexpr(PVSLambda((argcont:::tempcont).distinct,doExpr(_def)))
            tempcont = Context()
            s add Constant(s.toTerm,doName("def"),None,Some(parameters.universalizetp(OMS(PVSTheory.expr))),Some(defterm),None)
            th add s
            th add Constant(th.toTerm,actualname,None,Some(parameters.universalizetp(OMS(PVSTheory.expr))),
            Some(OMS((th.path / s.name) ? doName("def"))),None)

         case application_judgement(named,name,arg_formals,tp1) =>
            val truename : String = if (named.id.isDefined && named.id.get!="") named.id.get else { counter+=1 ; "UNNAMED_"+counter}
            val contp = arg_formals.flatMap(bs => bs._bindings.map(v => (doName(v.id),doType(v._type))))
            val appl = contp.foldLeft(doExpr(name))((e,v) => ApplySpine(OMS(PVSTheory.app),e,OMV(v._1)))
            val judg = Pi(contp map (p => VarDecl(p._1,Some(OMS(PVSTheory.expr)),None,None)),
            Arrow(contp map (p => PVSTheory.ofType(OMV(p._1),p._2)),PVSTheory.ofType(appl,doType(tp1._internal))))
            th add Constant(th.toTerm,NewName(truename),None,Some(parameters.universalizetp(judg)),None,None)

         case type_def_decl(named,ne,arg_formals,df) =>
            val truename = NewName(named.id)
            th add Constant(th.toTerm,truename,None,Some(parameters.universalizetp(OMS(PVSTheory.tp))),
               Some(parameters.universalizeexpr(doType(df._declared))),None)
               // TODO: Also declared, because export wrong?

         case type_decl(named,ne) =>
            th add Constant(th.toTerm,NewName(named.named.id),None,Some(parameters.universalizetp(OMS(PVSTheory.tp))),None,None)

         case axiom_decl(named,assertion) =>
            val form = doExpr(assertion._formula)
            val c = Constant(OMID(th.path),NewName(named.named.id),None,Some(parameters.universalizetp(Apply(OMID(PVSTheory.axiom),
               if (tempcont.nonEmpty) forall(tempcont.distinct,form) else form
            ))),None,None)
            tempcont = Context()
            th add c

         case subtype_judgement(named,sub,sup) =>
            th add Constant(OMID(th.path),NewName(named.id.getOrElse("SUBTYPE_JUDGEMENT")),None,
               Some(PVSTheory.subtypeof(doType(sub._internal),doType(sup._internal))),None,None)

         case name_judgement(named,nameexpr,tp1) =>
            th add Constant(OMID(th.path),NewName(named.id.getOrElse("NAME_JUDGEMENT")),None,
               Some(PVSTheory.ofType(doExpr(nameexpr),doType(tp1._internal))),None,None)

         case macro_decl(cdecl) => doDecl(cdecl)

         case type_from_decl(named,ne,tp1) =>
            val name = NewName(named.named.id)
            val c = Constant(OMID(th.path),name,None,Some(OMS(PVSTheory.tp)),None,None)
            th add c
            th add Constant(OMID(th.path),LocalName(name+"_SUBTYPE_OF"),None,
               Some(PVSTheory.subtypeof(OMS(c.path),doType(tp1._internal))),
               None,None)

         case _ => println("TODO Decl: "+d.getClass); sys.exit
      }
   }

   def doFormals(af:List[bindings])(implicit th:DeclaredTheory) : Context =
      af.flatMap(bs => bs._bindings.map(b => VarDecl(doName(b.id),Some(doType(b._type)),None,None)))

   def doTheoryExpr(t: TheoryExpr)(implicit th:DeclaredTheory): (MPath,Option[thassignment]) = {
      t match {
         case theory_name(place,id,library_id,mappings,target, actuals, dactuals) =>
            val tpath = (if (library_id=="") path else path / library_id) ? doName(id)
            if (tpath!=th.path && !th.getIncludes.contains(tpath)) th add PlainInclude(tpath,th.path)
            val allact = actuals:::dactuals
            val ass = if (allact.nonEmpty) {
               val h = thassignment(tpath, allact map doObject)
               parameters.addtheory(h.p,h.l)
               Some(h)
            } else None
            (tpath, ass)

         case _ => println("TODO TheoryExpr: "+t.getClass); null
      }
   }



   def doType(t: Type)(implicit th:DeclaredTheory): Term = {
      val tM: Term = t match {
         case type_name(place,tpname,res) =>
            val (thpath,ass) = if (res.isEmpty) (th.path,None) else doTheoryExpr(res.get._theory)
            if (thpath == th.path) {
               if (parameters.pars.contains((doName(tpname.id),OMS(PVSTheory.tp),false)))
                  OMV(doName(tpname.id))
                else parameters.apply(OMS(th.path ? doName(tpname.id)))
            } else {
               val rettp = OMS(thpath ? doName(tpname.id + (if (res.get.index==0) "" else "_"+res.get.index)))
               if (ass.isDefined) parameters.apply(ass.get,rettp) else rettp
            }

         case function_type(place, _from, _to) => _from match {
            case tp1:Type => PVSTheory.functype(doType(tp1), doType(_to))
            case binding(id,named,tp1) => PVSTheory.pitp(doType(tp1),VarDecl(doName(id),Some(OMS(PVSTheory.expr)),None,None),doType(_to))
         }

         case tuple_type(place, doms) =>
            val (bs,tps) = (doms collect {case b:binding => (b.id,b.named,b._type)}, doms collect {case t:Type => doType(t)})
            if (bs.isEmpty) PVSTheory.tptuple(tps)
            else bs.tail.foldRight(
                  PVSTheory.pitp(doType(bs.head._3),VarDecl(doName(bs.head._1),Some(OMS(PVSTheory.expr)),None,None),PVSTheory.tptuple(tps))
               )((triple,c) =>
               PVSTheory.pitp(doType(triple._3),VarDecl(doName(triple._1),Some(OMS(PVSTheory.expr)),None,None),c)
              )

         case record_type(place,fields) => PVSTheory.recordtype(fields map (field => {
            val (named,tp1) = (doName(field.named.id),doType(field._type))
            PVSTheory.makerecordfield(named,tp1)
         }))

         case expr_as_type(_,expr1,tp1) => ApplySpine(OMS(PVSTheory.exprastype),doExpr(expr1),
            if (tp1.isDefined) doType(tp1.get) else OMS(sym("notype")))

         case setsubtype(_,_type,_expr) => ApplySpine(OMS(PVSTheory.predsub),doType(_type),doExpr(_expr))

         case type_application(_,tp1,args) => ApplySpine(OMS(PVSTheory.tpapp),doType(tp1),
            if(args.length==1) doExpr(args.head) else doExpr(tuple_expr("",args))
         )

         case _ => println("TODO: Type "+t.getClass); sys.exit
      }
      doSourceRef(t, tM)
      tM

   }
   def doExpr(e: Expr)(implicit th:DeclaredTheory): Term = {
      val eM: Term = e match {
         case lambda_expr(place,bindings,body) => PVSLambda(
            bindings map(b => VarDecl(doName(b.id),Some(doType(b._type)),None,None)
              ),doExpr(body))

         case forall_expr(place,bindings,body) => PVSTheory.forall(
            bindings map(b => VarDecl(doName(b.id),Some(doType(b._type)),None,None)
              ),doExpr(body))

         case exists_expr(place,bindings,body) => PVSTheory.exists(
            bindings map(b => VarDecl(doName(b.id),Some(doType(b._type)),None,None)
              ),doExpr(body))

         case application(place,funct,arg,infix) => ApplySpine(OMS(PVSTheory.app),doExpr(funct),doExpr(arg))

         case name_expr(place, name1, tp1, res) =>
            val (thpath,ass) = doTheoryExpr(res._theory)
            if (thpath == th.path) {
               if (parameters.pars.contains((doName(name1.id),doType(tp1.get),false)))
                  OMV(doName(name1.id))
               else parameters.apply(OMS(th.path ? doName(name1.id)))
            } else {
               val retexp = OMS(thpath ? doName(name1.id + (if (res.index==0) "" else "_"+res.index)))
               if (ass.isDefined) parameters.apply(ass.get,retexp) else retexp
            }

         case tuple_expr(place,args) => PVSTheory.exprtuple(args map doExpr)

         case varname_expr(place, id, typ) =>
            tempcont = tempcont++VarDecl(doName(id),Some(doType(typ)),None,None)
            OMV(id)

         case cases_expr(place,exp,selections) => PVSTheory.makecases(doExpr(exp),selections map ( sel => {
            val (cons, bindings, _expr) = (sel._cons,sel.bindings,sel._expr)
            val cont = bindings.map(b => VarDecl(doName(b.id),Some(doType(b._type)),None,None))
            if (cont.nonEmpty)
               casematch(casebind(cont,doExpr(cons)),doExpr(_expr))
            else casematch(doExpr(cons),doExpr(_expr))
         }))

         case field_appl_expr(_,id,_expr) =>
            Apply(OML(VarDecl(doName(id),Some(Arrow(OMS(PVSTheory.expr),OMS(PVSTheory.expr))),None,None)),doExpr(_expr))

         case record_expr(_,_ass) =>
            val assignments = _ass.map(ass => {
               val (assignment_args, _expr) = (ass.assignment_args, ass._expr)
               if (assignment_args.length>1) {println("record_expr: more than one assignment_arg!"); sys.exit}
               val field = assignment_args.head match {
                  case field_assign(_,id) => OML(VarDecl(doName(id),Some(Arrow(OMS(PVSTheory.expr),OMS(PVSTheory.expr))),None,None))
                  case _ => println("record_expr: assignment_arg is not field_assign"); sys.exit
               }
               (field,doExpr(_expr))
            })
            PVSTheory.recordexpr(assignments)

         case proj_appl_expr(_,_expr,index) =>
            Apply(OML(VarDecl(doName("PROJ_"+index),Some(Arrow(OMS(PVSTheory.expr),OMS(PVSTheory.expr))),None,None)),doExpr(_expr))

         case number_expr(_,i) => PVSTheory.numberexpr(i)

         case _ => println("TODO Expr: "+e.getClass); sys.exit
      }
      doSourceRef(e, eM)
      eM
   }


   def doObject(o:Object)(implicit th:DeclaredTheory) : Term = o match {
      case tp: Type => doType(tp)
      case e: Expr => doExpr(e)
   }

   */
}
