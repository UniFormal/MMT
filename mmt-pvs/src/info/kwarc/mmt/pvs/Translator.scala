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

   val path = bt.narrationDPath.^!.^!

   object State {

      private var includes : Set[MPath] = Set()
      private var pars : List[(LocalName,Term,Boolean)] = Nil
      private var vardecls : List[(LocalName,Term)] = Nil
      private var tccs : List[Term] = Nil

      var th : DeclaredTheory = null
      var usedvars : List[LocalName] = Nil
      var boundvars : List[LocalName] = Nil

      def reset(t:DeclaredTheory) = {
         includes = Set()
         pars = Nil
         vardecls = Nil
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

      def addconstant(name : LocalName, args: List[(LocalName,Term)], giventype : Term, inferredType : Option[Term],
                      defOpt : Option[Term]): Unit = {

         var actualdef = if (inferredType.isDefined && inferredType.get!=giventype) defOpt.map(
            d => PVSTheory.asType(inferredType.get,giventype,d,tccs.head)
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

         if (pars.nonEmpty) {
            actualdef = actualdef.map(Lambda(pars.map(p => VarDecl(p._1,Some(if(p._3) p._2 else PVSTheory.expr(p._2)),None,None)),_))
            actualtype = Pi(pars.map(p => VarDecl(p._1,Some(if(p._3) p._2 else PVSTheory.expr(p._2)),None,None)),actualtype)
         }

         th add Constant(th.toTerm,name,None,Some(actualtype),actualdef,None)
         // println(th.getConstants.head)
      }
   }
   /*
   var tccs : List[tcc_decl] = Nil
   var vars : Set[(LocalName,Term)] = Set()


   case class thassignment(p:MPath,l:List[Term])

   object parameters {
      var pars : List[(LocalName,Term,Boolean)] = Nil
      // var theoryassignments : List[thassignment] = Nil

      // adds a new parameter of the current theory. isLFType is true,
      // if the object is an LF type (as opposed to a PVS type)
      def add(name:LocalName,tp:Term,isLFType:Boolean) = pars = pars:::List((name,tp,isLFType)).distinct
      // def addtheory(t:MPath,l:List[Term]) = theoryassignments = (thassignment(t,l)::theoryassignments).distinct

      // def apply(t:thassignment,tm:Term) = ApplySpine(tm, t.l :_*)

      // def apply(t:Term) = ApplySpine(t,pars map (p => OMV(p._1)) :_*)

      def clear = pars = Nil

      def universalizetp(t:Term) : Term = t /* if (pars.nonEmpty) {
         Pi(pars.map(p => VarDecl(p._1,Some(
            if (p._3) p._2 else
            OMS(if (p._2==PVSTheory.tp.term) PVSTheory.tp.path else PVSTheory.expr.path)
         ),None,None)),
         {
            val nontps = pars.filter(p => p._2!=PVSTheory.tp.term && !p._3)
            if (nontps.isEmpty) t else Arrow(nontps.map(p => PVSTheory.ofType(OMV(p._1),p._2)),t)
         } )
      } else t */

      def universalizeexpr(t:Term) : Term = t /* if (pars.nonEmpty) {
         Lambda(pars.map(p => VarDecl(p._1,Some(
            if (p._3) p._2 else
            OMS(if (p._2==PVSTheory.tp.term) PVSTheory.tp.path else PVSTheory.expr.path)
         ),None,None)),
         {
            val nontps = pars.filter(p => p._2!=PVSTheory.tp.term && !p._3)
            if (nontps.isEmpty) t else Arrow(nontps.map(p => PVSTheory.ofType(OMV(p._1),p._2)),t)
         } )
      } else t */

   }
   */

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
         val cont = Context() // (t.theory_formals map doFormalPars) collect {case Some(v) => v}
         implicit val th = new DeclaredTheory(path,doName(t.named.id),Some(PVSTheory.thpath),cont)
         State.reset(th)
         t.theory_formals map doFormal
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

   def doAssumption (ad:AssumingDecl) : Unit = ad match {
      case _ => println("TODO Assumption: "+ad.getClass); sys.exit
   }

   def doFormal(f:FormalParameter) = f match {
      case formal_type_decl(named,ne) => State.addparameter(newName(named.named.id),PVSTheory.tp.term,true)
       /*
      case formal_subtype_decl(named,_,sup) => parameters.add(newName(named.named.id),PVSTheory.tp.term,false)
         // TODO how do I introduce subtyping conditions?
         */
      case _ => println("TODO Formal: " + f.getClass + ": " + f); sys.exit
   }

   // TODO: add parameters everywhere!

   def doDecl(d: Decl) {
      val ret = d match {
         case var_decl(id,unnamed,tp) => State.addvardecl(doName(id),doType(tp._internal))
         case tcc_decl(named,assertion) => State.addtcc(doExpr(assertion._formula)._1)
         case const_decl(named,arg_formals,tp,optdef) =>
            // println(d)
            val name = newName(named.named.id)
            val rettype = doType(tp._declared)
            val (defi,fulltp) = if (optdef.isDefined) {val (a,b) = doExpr(optdef.get);(Some(a),Some(b))} else (None,None)

            State.addconstant(
               name,
               arg_formals.flatMap(_._bindings.map(b => (newName(b.id),doType(b._type)))),
               rettype,
               fulltp,
               defi
            )
          /*
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
          /*
         case function_type(_,from,to) =>

         */
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
               case _ => PVSTheory.unknown.term
            }
            (PVSTheory.apply(tmf,tmarg)(tparg,tptarget),tptarget)
         case name_expr(_,name,optp,res) =>
            (doPath(name,Some(res)),optp.map(doType).getOrElse(PVSTheory.unknown.term))
            // val usedvars = bindings.map(b => (newName(b.id),doType(b._type))).toSet
            // vars = vars diff usedvars
            // usedvars.foldRight(bd)((v,t) => PVSTheory.forall(v._1,v._2,t))
         case lambda_expr(_,bindings,body) =>
            val (bd,tptarget) = doExpr(body)
            val con = Context(bindings.map(b => VarDecl(doName(b.id),Some(doType(b._type)),None,None)):_*)
            State.boundvars:::=con.map(_.name)
            (PVSTheory.lambda(con,bd,tptarget),con.foldRight(tptarget)((v,t) => PVSTheory.fun_type(v.tp.get,t)))
         case tuple_expr(_,args) => PVSTheory.tuple_expr(args map doExpr)
         case varname_expr(_,id,tp) =>
            //vars+= ((newName(id),doType(tp)))
            State.usedvars ::= newName(id)
            (OMV(newName(id)),doType(tp))
          /*
         case lambda_expr(_,bindings,body) =>
            val bd = doExpr(body)
            val usedvars = bindings.map(b => (newName(b.id),doType(b._type))).toSet
            vars = vars diff usedvars
            usedvars.foldRight(bd)((v,t) => PVSTheory.lambda(v._1,v._2,t))

         case application(_,f,arg,_) => PVSTheory.PVSapply(doExpr(f),doExpr(arg))
         case name_expr(_,name,_,res) => doPath(name,Some(res)) // should I use the type for something?
         case tuple_expr(_,args) => PVSTheory.exprtuple(args map doExpr)
         case varname_expr(_,id,tp) =>
            vars+= ((newName(id),doType(tp)))
            OMV(newName(id))
         case cases_expr(_,expr,selections) => doExpr(expr) // Definitely TODO!
         */
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
         // apply theory parameters
         if (allactuals.nonEmpty) ApplySpine(sym, allactuals map (a => doObject(a)): _*) else sym
      }
   }

}
