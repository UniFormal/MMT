package info.kwarc.mmt.pvs

import info.kwarc.mmt.api.frontend.Controller
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

class PVSImportTask(controller: Controller, bt: BuildTask, index: Document => Unit) {
   var tempcont: Context = Context()
   var counter = 0
   val path = bt.narrationDPath.^!.^!

   case class thassignment(p:MPath,l:List[Term])

   object parameters {
      var pars : List[(LocalName,Term,Boolean)] = Nil
      var theoryassignments : List[thassignment] = Nil

      def add(name:LocalName,tp:Term,isLFType:Boolean) = pars = pars:::List((name,tp,isLFType)).distinct
      def addtheory(t:MPath,l:List[Term]) = theoryassignments = (thassignment(t,l)::theoryassignments).distinct

      def apply(t:thassignment,tm:Term) = ApplySpine(tm, t.l :_*)

      def apply(t:Term) = ApplySpine(t,pars map (p => OMV(p._1)) :_*)

      def clear = pars = Nil

      def universalizetp(t:Term) : Term = if (pars.nonEmpty) {
         Pi(pars.map(p => VarDecl(p._1,Some(
            if (p._3) p._2 else
            OMS(if (p._2==OMS(PVSTheory.tp)) PVSTheory.tp else PVSTheory.expr)
         ),None,None)),
         {
            val nontps = pars.filter(p => p._2!=OMS(PVSTheory.tp) && !p._3)
            if (nontps.isEmpty) t else Arrow(nontps.map(p => PVSTheory.ofType(OMV(p._1),p._2)),t)
         } )
      } else t

      def universalizeexpr(t:Term) : Term = if (pars.nonEmpty) {
         Lambda(pars.map(p => VarDecl(p._1,Some(
            if (p._3) p._2 else
            OMS(if (p._2==OMS(PVSTheory.tp)) PVSTheory.tp else PVSTheory.expr)
         ),None,None)),
         {
            val nontps = pars.filter(p => p._2!=OMS(PVSTheory.tp) && !p._3)
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
      val mrefsM = modsM.map(m => {controller.add(m) ;MRef(bt.narrationDPath, m.path,true)})
      val doc = new Document(bt.narrationDPath, mrefsM)
      index(doc)
   }

   def doModule(d:DPath)(m: syntax.Module): modules.Module = m match {
      case t: theory =>
         counter = 0
         parameters.clear
         val cont = Nil // (t.theory_formals map doFormalPars) collect {case Some(v) => v}
         implicit val th = new DeclaredTheory(path,doName(t.named.id),Some(PVSTheory.path),cont)
         t. theory_formals map doFormal
         // TODO: assuming, exporting_, possibly named stuff?
         t._decls map doDecl

         println(" -- Theory: "+th)
         th
      case d:datatype =>
         println(" -- Datatype: "+d.body.named.id)
         sys.exit
      case _ =>
         println(" -- OTHER: "+m.getClass)
         sys.exit
   }

   /*
   def doFormalPars(f:FormalParameter) : Option[VarDecl] = f match {

      case formal_type_decl(named,ne) => Some(VarDecl(doName(named.named.id),Some(OMS(PVSTheory.tp)),None,None))
      case _ =>
   }
   */

   def doFormal(f:FormalParameter)(implicit th:DeclaredTheory) = f match {
       /*
      case formal_type_decl(named, ne) =>
         th add Constant(OMID(th.path),doName(named.named.id),None,Some(OMID(PVSTheory.tp)),None,None)
      case formal_subtype_decl(named, ne, sup) =>
         th add PVSTheory.subtp(th,named.named.id,doType(sup._declared))
      case formal_const_decl(named, tp2) =>
         th add PVSTheory.ofTypeDecl(th,named.named.id,doType(tp2._internal))
      case formal_theory_decl(named, _name) =>
      */
      case formal_type_decl(named,ne) => parameters.add(doName(named.named.id),OMS(PVSTheory.tp),false)
      case formal_subtype_decl(named,ne,sup) => parameters.add(doName(named.named.id),OMS(PVSTheory.tp),false)
         parameters.add(doName("FORMAL_SUBTYPE_DECL_"+counter),PVSTheory.ofType(OMV(doName(named.named.id)),doType(sup._declared)),true)
         counter+=1
        // TODO only here ._declared, because export wrong

      case _ => println("TODO: Formal "+f.getClass); sys.exit
   }

   def doDecl(d: Decl)(implicit th:DeclaredTheory) = {
      d match {
          /*
         case tcc_decl(named, assertion) => None
         case const_decl(named,arg_formals,tp2,_def) =>
            val s = PVSTheory.ofTypeDecl(th,named.named.id,doType(tp2._internal))
            val cont =
               arg_formals.flatMap(_._bindings).map(v => VarDecl(doName(v.id),Some(doType(v._type)),None,None))
            val ndef = _def.map(x => if (cont.nonEmpty) PVSLambda(cont,doExpr(x)) else doExpr(x))
            s add Constant(s.toTerm,LocalName("def"),None,Some(OMS(PVSTheory.expr)),ndef,None)
            th add s
            tempcont = Context()
            None
         case formula_decl(named,assertion) =>
            val form = doExpr(assertion._formula)
            val c = Some(
            Constant(OMID(th.path),doName(named.named.id),None,Some(Apply(OMID(PVSTheory.formula),
               if (tempcont.nonEmpty) forall(tempcont.distinct,form) else  form
            )),None,None))
            tempcont = Context()
            c
         case conversion_decl(unnamed, kind, expr) =>
         case def_decl(named, arg_formals, otp, _def, _measure, _order) =>
            val s = PVSTheory.defdecl(th,named.named.id, doType(otp._internal))
            val df = doExpr(_def)
            val ndf = if (tempcont.nonEmpty) forall(tempcont.distinct,df) else df
            tempcont = Context()
            s add Constant(s.toTerm,LocalName("function"),None,Some(OMS(PVSTheory.expr)),Some(ndf),None)
            th add s
            None
         case application_judgement(named, _name, arg_formals, tp) =>
            val s = PVSTheory.ofTypeDecl(th,named.id.getOrElse({counter+=1; "Judgment_"+(counter-1)}),doType(tp._internal))
            val cont = arg_formals.flatMap(b => b._bindings.map(v => VarDecl(doName(v.id),Some(doType(v._type)),None,None)))
            val df = PVSLambda(cont,ApplySpine(OMS(PVSTheory.app),doExpr(_name),
               cont.tail.foldLeft(cont.head.toTerm.asInstanceOf[Term])((s,t) => ApplySpine(OMS(PVSTheory.exprtuple),s,t.toTerm))))
            s add Constant(s.toTerm,LocalName("def"),None,Some(OMS(PVSTheory.expr)),Some(df),None)
            th add s
            None
            */
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

         case _ => println("TODO Decl: "+d.getClass); sys.exit
      }
   }

   def doFormals(af:List[bindings])(implicit th:DeclaredTheory) : Context =
      af.flatMap(bs => bs._bindings.map(b => VarDecl(doName(b.id),Some(doType(b._type)),None,None)))

   def doTheoryExpr(t: TheoryExpr)(implicit th:DeclaredTheory): (MPath,Option[thassignment]) = {
      t match {
          /*
         case theory_name(place, id, library_id, mappings, targetOpt, actuals, Nil) =>
            val p = DPath(URI.empty / library_id) ? id
            val tM = OMMOD(p)
            doSourceRef(t, tM)
            tM
            */
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
          /*
         case type_name(place, tname, _res) =>
            val libraryname = if (_res.isDefined && _res.get._theory.library_id!="")
               PVSTheory.dpath / _res.get._theory.library_id
            else th.parent
            val theoryname = libraryname ? (if (_res.isDefined) doName(_res.get._theory.id) else th.name)
            if(!(th.getIncludes.contains(theoryname) || theoryname==th.path)) th add PlainInclude(theoryname,th.path)
            if (tname.actuals.isEmpty) OMS(theoryname ? doName(tname.id)) else
               ApplySpine(OMID(theoryname ? doName(tname.id)), tname.actuals map doObject :_*)
         case type_application(place, tp, args) =>
            val tpM = doType(tp)
            val exps = args map doExpr
            val arg = exps.tail.foldLeft(exps.head)((s,t) => ApplySpine(OMS(PVSTheory.exprtuple),s,t))
            ApplySpine(OMS(PVSTheory.tpapp),tpM, arg)
         case function_type(place, _from, _to) => ApplySpine(OMS(PVSTheory.functype),doDomain(_from),doType(_to))
         case tuple_type(place, doms) =>
            val types = doms collect {case tp:Type => doType(tp)
               case _ => ???}
            types.tail.foldLeft(types.head)((s,t) => ApplySpine(OMS(PVSTheory.tptuple),s,t))
         case setsubtype(place, of, by) =>
            ApplySpine(OMID(PVSTheory.predsub),doType(of), doExpr(by))
         case cotuple_type(place, args) =>
            val types = args map doType
            types.tail.foldLeft(types.head)((s,t) => ApplySpine(OMS(PVSTheory.union),s,t))
         case record_type(place, _fields) =>
         case expr_as_type(place, expr, tp) =>
            Apply(OMID(PVSTheory.asType),doExpr(expr))
            */
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
            case tp1:Type => PVSTheory.functype(doDomain(_from), doType(_to))
            case binding(id,named,tp1) => PVSTheory.pitp(doType(tp1),VarDecl(doName(id),Some(OMS(PVSTheory.expr)),None,None),doType(_to))
         }

         case tuple_type(place, doms) => PVSTheory.tptuple(doms map doDomain)

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
          /*
         case varname_expr(place, id, typ) =>
            tempcont = tempcont++VarDecl(doName(id),Some(doType(typ)),None,None)
            OMV(id)
         case application(place,funct,arg,infix) => ApplySpine(OMS(PVSTheory.app),doExpr(funct),doExpr(arg))
         case name_expr(place, name, tp, res) =>
            val libraryname = if (res._theory.library_id!="")
               PVSTheory.dpath / res._theory.library_id
            else th.parent
            val theoryname = libraryname ? (if (res._theory.id!="") doName(res._theory.id) else
               th.name)
            if(!(th.getIncludes.contains(theoryname) || theoryname==th.path)) th add PlainInclude(theoryname,th.path)
            OMS(theoryname ? doName(name.id))
         case tuple_expr(place,args) =>
            val exps = args map doExpr
            exps.tail.foldLeft(exps.head)((s,t) => ApplySpine(OMS(PVSTheory.exprtuple),s,t))
         case lambda_expr(place,bindings,body) => PVSLambda(
            bindings map(b => VarDecl(doName(b.id),Some(doType(b._type)),None,None)
              ),doExpr(body))
         case forall_expr(place,bindings,body) => PVSTheory.forall(
            bindings map(b => VarDecl(doName(b.id),Some(doType(b._type)),None,None)
              ),doExpr(body))
         case cases_expr(place,exp,sel) =>
            val caselist = sel.map(s =>exprtocase(
               s.bindings.map(v => VarDecl(doName(v.id),Some(doType(v._type)),None,None)),doExpr(s._cons),doExpr(s._expr)
            ))
            PVSTheory.casedist(doExpr(exp),caselist)
         case field_appl_expr(place,id,_expr) => OMS(PVSTheory.fieldapp)
         case record_expr(place, _ass) => OMS(PVSTheory.fieldapp)
         */

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
     // case te: TheoryExpr => doTheoryExpr(te)
   }

   def doDomain(d:domain)(implicit th:DeclaredTheory) : Term = d match {
      case tp: Type => doType(tp)
      case binding(id,named,tp1) => println("Binding in Domain!"); sys.exit//doType(tp1)

   }

   def doName(s:String) : LocalName = LocalName(s)

   def NewName(s:String,start:Int = 1)(implicit th:DeclaredTheory) : LocalName = {
      if (!th.declares(doName(s))) doName(s)
      else if (!th.declares(doName(s + "_" + start))) doName(s + "_" + start)
      else NewName(s, start + 1)
   }
}
