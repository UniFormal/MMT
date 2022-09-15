package info.kwarc.mmt.pvs

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.{DPath, LocalName, MPath}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.parser.{SourcePosition, SourceRef, SourceRegion}
import info.kwarc.mmt.api.uom.StandardRat
import info.kwarc.mmt.api.utils.{FileURI, URI, _}
import info.kwarc.mmt.lf.ApplySpine
import info.kwarc.mmt.pvs.syntax.{DeclaredType, Expr, Object, Type, application, binding, cases_expr, exists_expr, expr_as_type, field_appl_expr, field_assign, forall_expr, function_type, lambda_expr, name, name_expr, number_expr, proj_appl_expr, proj_assign, rational_expr, record_expr, record_type, resolution, setsubtype, string_expr, theory_name, tuple_expr, tuple_type, type_application, type_extension, type_name, update_assignment, update_expr, varname_expr}

case class ObjectLevelTranslator(state : TranslationState,controller : Controller) {

  def doObject(o:Object) : Term = o match {
    case tp: Type => doType(tp)
    case e: Expr => doExpr(e)._1
    case _ =>
      println("TODO Object: " + o.getClass + ": " + o)
      sys.exit()
  }

  def doType(t: Type): Term = {
    val tM: Term = t match {
      case type_name(_,name,res) =>
        doPath(name,res)
      case function_type(_,from,to) =>
        from match {
          case binding(id,named,tp) =>
            PVSTheory.pvspi(LocalName(id),doType(tp),doType(to))
          case t: Type => PVSTheory.fun_type(doType(t),doType(to))
        }
      case tuple_type(_,doms) =>
        val last = doms.reverse.head match {
          case tp : Type => doType(tp)
          case tp : DeclaredType => doType(tp._internal)
          case binding(id,named,tp) => doType(tp)
          case _ => throw new Exception("Last element of tuple is not independently typed")
        }
        val rest = doms.dropRight(1)
        rest.foldRight(last)((d,tm) => d match {
          case tp : Type => PVSTheory.tuple_type(List(doType(tp),tm))
          case tp : DeclaredType => PVSTheory.tuple_type(List(doType(tp._internal),tm))
          case binding(id,named,tp) => PVSTheory.pvssigma(LocalName(id),doType(tp),tm)
        })
      case expr_as_type(_,expr,optType) =>
        val (e,tp) = doExpr(expr)
        val tp2 = optType.map(doType)/*
        println(controller.presenter.asString(e))
        println(controller.presenter.asString(tp))
        if (optType.isDefined) {
          println(" = " + controller.presenter.asString(doType(optType.get)))
        }
        readLine()
        */

        PVSTheory.expr_as_type(e,tp2.getOrElse(tp))(state)
      case record_type(_,fields) =>
        PVSTheory.recordtp(fields.map(f => (LocalName(f.named.id),doType(f._type))):_*)
      case setsubtype(_,tp,exp) =>
        PVSTheory.setsub(doType(tp),doExpr(exp)._1)
      case type_application(_,tpname,args) =>
        ApplySpine(doType(tpname),args.map(doExpr(_)._1):_*)
      case type_extension(_,tp,by) =>
        PVSTheory.typeext(doType(tp),doType(by))
      case _ =>
        println("TODO Type: " + t.getClass + ": " + t)
        sys.exit()
    }
    state.doSourceRef(t, tM)
    tM
  }

  def doExprAs(e : Expr,t : Term) : Term = {
    /*
    def equal(t1:Term,t2:Term) : Boolean = {
      if (t1==t2) true else (t1,t2) match {
        case (PVSTheory.pvspi(_,a,Lambda(_,_,b)),PVSTheory.pvspi(_,a2,Lambda(_,_,b2))) => equal(a,a2) && equal(b,b2)
        case (PVSTheory.tuple_type(l),PVSTheory.tuple_type(r)) => l.length==r.length && l.indices.forall(i => equal(l(i),r(i)))
        case (PVSTheory.pred(a),PVSTheory.pred(b)) => equal(a,b)
        case (PVSTheory.bool(),PVSTheory.bool()) => true
        case _ => false
      }
    }
    */
    val (tm,tp) = doExpr(e)
    /*
    if (!equal(tp,t)) {
      PVSTheory.typecast(tp,t,tm,if (state.tcc.isDefined) state.tcc.get else {
        null
      })
    } else */ tm
  }

  def doExpr(e: Expr): (Term,Term) = {
    val eM: (Term,Term) = e match {
      case name_expr(_,name,optp,res) =>
        (doPath(name,Some(res)),optp.map(doType).getOrElse(state.doUnknown))

      case forall_expr(_,bindings,body) => // TODO make this sequence compatible
        val vars = state.vars
        state.vars = Nil
        val bd = doExpr(body)._1
        val con = Context(bindings.map(b =>
        {
          val name = LocalName(b.id)
          val typ = doType(b._type)
          VarDecl(name,typ)
        }):_*)
        bindings foreach (b=> state.vars = state.vars.variables.filter(_.name.toString != b.id.toString).toList)
        state.vars = (state.vars ::: vars).distinct
        (PVSTheory.forall(con,bd),PVSTheory.bool.term)

      case exists_expr(_,bindings,body) =>
        val vars = state.vars
        state.vars = Nil
        val bd = doExpr(body)._1
        val con = Context(bindings.map(b => {
          val name = LocalName(b.id)
          val typ = doType(b._type)
          VarDecl(name, typ)
        }):_*)
        bindings foreach (b=> state.vars = state.vars.variables.filter(_.name.toString != b.id.toString).toList)
        state.vars = (state.vars ::: vars).distinct
        (PVSTheory.exists(con,bd),PVSTheory.bool.term)

      case application(_,f,arg,_) =>
        val (tmf,tpf) = doExpr(f)
        val (tmarg,tparg) = doExpr(arg)
        PVSTheory.pvsapply(tmf,tmarg,tpf)(state)
      case lambda_expr(_,bindings,body) =>
        val vars = state.vars
        state.vars = Nil
        val (bd,tptarget) = doExpr(body)
        val ret = bindings.foldRight((bd,tptarget))((b,pair) => {
          val tp = doType(b._type)
          val name = LocalName(b.id)
          (PVSTheory.pvslambda(name, tp, pair._2, pair._1),
            PVSTheory.pvspi(name, tp, pair._2))
        })
        bindings foreach (b=> state.vars = state.vars.variables.filter(_.name.toString != b.id.toString).toList)
        state.vars = (state.vars ::: vars).distinct
        ret

      case tuple_expr(_,args) =>
        val tms = args.map(doExpr)
        PVSTheory.tuple_expr(tms)

      case varname_expr(_,id,tp) =>
        val name = LocalName(id)
        val typ = doType(tp)
        state.vars ::= VarDecl(name, typ)
        (OMV(name),typ)

      case cases_expr(_,expr,sels) =>
        // variables here?
        val (t,tp) = doExpr(expr)
        val cases = sels.map(s => {
          val (cons,casetp) = doExpr(s._cons)
          PVSTheory.selection(
            cons,
            s.bindings.map(b => VarDecl(LocalName(b.id), doType(b._type))),
            doExpr(s._expr)._1,
            casetp
          )
        })
        (PVSTheory.pvsmatch(t,tp,cases,state.doUnknown),state.doUnknown)

      case field_appl_expr(_,id,expr) =>
        val (tm,tmtp) = doExpr(expr)
        (PVSTheory.fieldapp(tm,id),tmtp match {
          case PVSTheory.recordtp(ls) => ls.find(p => p._1==id).getOrElse(("",state.doUnknown))._2
          case _ => state.doUnknown
        })

      case record_expr(_,asslist) =>
        // println("RECORD IN " + state.path)
        val list = asslist.map(a => {
          val (df,tp) = doExpr(a._expr)
          (a.assignment_args match {
            case List(field_assign(_,id)) => LocalName(id)
            case _ => throw new Exception("field_assign expected!")
          },tp,df)
        })
        (PVSTheory.recordexpr(list:_*),PVSTheory.recordtp(list.map(t => (t._1,t._2)):_*))

      case number_expr(_,j) => (OMLIT(j,NatLiterals),NatLiterals.pvstp)

      case rational_expr(_,s) =>
        val rat = StandardRat.fromString(s.replace(" ","/"))// s.replace(" ","/")
        (OMLIT(rat,RationalLiterals),RationalLiterals.pvstp)

      case proj_appl_expr(_,expr,i) =>
        val (tm,tp) = doExpr(expr)
        PVSTheory.projection(tm,tp,i)(state)

      case update_expr(_,expr,assignlist) =>
        // variables here?
        val (tm,tp) = doExpr(expr)
        def doUpdateAssignment(ex : Term,ass : update_assignment) : Term = {
          val (asstm,asstp) = doExpr(ass._expr)
          PVSTheory.update(ex,ass.assignment_args match {
            case field_assign(_,id)::args =>
              val realargs = args.map(x => doExpr(x.asInstanceOf[Expr])._1)
              PVSTheory.recupdate(id,asstm,realargs)
            case List(proj_assign(_,i)) =>
              PVSTheory.tupleupdate(i,asstm)
            case ls : List[Expr]@unchecked if ls.forall(_.isInstanceOf[Expr]) =>
              val expr = PVSTheory.tuple_expr(ls.map(doExpr))
              PVSTheory.funupdate(expr._1,asstm)
            case x =>
              println("TODO update_expr assignment arg of type " + x.head.getClass)
              println("Update " + ex)
              println(" - " + asstm)
              println(" - " + asstp)
              println(" - " + x)
              println(" in " + tm + " : " + tp)
              sys.exit()
          })
        }
        (assignlist.foldLeft(tm)((t,ass) => doUpdateAssignment(t,ass)),tp)

      case string_expr(_,str) => (OMLIT(str,StringLiterals),StringLiterals.synType)

      case _ =>
        println("TODO Expr: " + e.getClass + ": " + e)
        sys.exit()
    }
    state.doSourceRef(e, eM._1)
    eM
  }

  def getLibrary(thid : String, library_id : String) : MPath = library_id match {
    case "finite_sets" => (PVSTheory.rootdpath / "finite_sets") ? thid
    case "bitvectors" => (PVSTheory.rootdpath / "bitvectors") ? thid
    case "" => controller.getO((PVSTheory.rootdpath / "prelude") ? thid) match {
      case Some(se) => (PVSTheory.rootdpath / "prelude") ? thid
      case None => state.path.parent ? thid
    }
    case s => (state.path.parent.^! / s) ? thid
  }

  def doMPath(thname : theory_name) : MPath = {
    val mp = getLibrary(thname.id,thname.library_id)
    if (mp.parent.toString == "http://pvs.csl.sri.com/prelude" && (thname.id == "booleans" || thname.id == "equalities")) {
      // println("Yields: " + OMS(PVSTheory.thpath ? id))
      PVSTheory.thpath
    } else mp
  }

  def doPath(n:name,res:Option[resolution]) : Term = {
    var (id,thid,library_id,mappings,opttarget,allactuals) = (n,res) match {
      case (n@name(id1,thid1,library_id1,mappings1,opttarget1,actuals1,dactuals1),Some(resolution(theory_name(_,thid2,library_id2,mappings2,opttarget2,actuals2,dactuals2),ind))) =>
        (n.id_proper+(if(ind>0) "_"+ind else ""),
          if (thid2 == "") thid1 else thid2,
          if (library_id2 == "") library_id1 else library_id2,
          mappings2,opttarget2,actuals2:::dactuals2)
      case (n@name(id1,thid1,library_id1,mappings1,opttarget1,actuals,dactuals),None) =>
        (n.id_proper,thid1,library_id1,mappings1,opttarget1,actuals:::dactuals)
    }

    val mpath = doMPath(theory_name("",thid,library_id,mappings,opttarget,allactuals,Nil))

    if(mappings.nonEmpty) {
      println("Found mappings in doPath")
      sys.exit()
    }
    if(opttarget.isDefined) {
      println("Found target in doPath")
      sys.exit()
    }

    if (mpath == state.path) state.getLocal(LocalName(id))
    else if (mpath == PVSTheory.thpath) {
      val sym = OMS(mpath ? id)
      if (allactuals.nonEmpty) ApplySpine(sym, allactuals map (a => doObject(a)): _*) else sym
    } else {
      if (state.isPrelude) state.addinclude(mpath) // should be unnecessary ouside of Prelude
      val sym =
      /*
      if (state.isPrelude || !((PVSTheory.rootdpath / "prelude") <= mpath)) {
        if (state.th.includes.inPars(mpath) contains true) OMV(LocalName(mpath) / id) else
        //OMS(state.th.path ? (LocalName(mpath) / id))
          OMS(mpath ? id)
      }
      else // OMS(PVSTheory.preludepath ? (LocalName(mpath) / id)) */
        OMS(mpath ? id)
      // apply theory parameters
      if (allactuals.nonEmpty) PVSTheory.parambind(sym,allactuals map doObject) else sym//ApplySpine(sym, allactuals map (a => doObject(a)): _*) else sym
      // println("Yields: " + ret)
    }
  }

  /*
  def doPath(n:name,res:Option[resolution]) : Term = {
    var (id,thid,library_id,mappings,opttarget,allactuals) = (n,res) match {
      case (name(id1,thid1,library_id1,mappings1,opttarget1,actuals1,dactuals1),Some(resolution(theory_name(_,thid2,library_id2,mappings2,opttarget2,actuals2,dactuals2),ind))) =>
        (id1+(if(ind>0) "_"+ind else ""),
          if (thid2 == "") thid1 else thid2,
          if (library_id2 == "") library_id1 else library_id2,
          mappings2,opttarget2,actuals2:::dactuals2)
      case (name(id1,thid1,library_id1,mappings1,opttarget1,actuals,dactuals),None) =>
        (id1,thid1,library_id1,mappings1,opttarget1,actuals:::dactuals)
    }

    val mpath = doMPath(theory_name("",thid,library_id,mappings,opttarget,allactuals,Nil))

    if(mappings.nonEmpty) {
      println("Found mappings in doPath")
      sys.exit
    }
    if(opttarget.isDefined) {
      println("Found target in doPath")
      sys.exit
    }

    if (mpath == state.th.path && (state.th.parameters.exists(v => v.name==LocalName(id)) || state.inductive.contains(mpath ? id))) {
      // if (state.vars.exists(v => v.name==LocalName(id))) println("Yields: " + OMV(id))
      OMV(id)
    } else if (mpath.toString == state.th.path.toString) {
      OMS(state.th.path ? id)
    } else if (mpath == PVSTheory.thpath) {
      val sym = OMS(mpath ? id)
      if (allactuals.nonEmpty) ApplySpine(sym, allactuals map (a => doObject(a)): _*) else sym
    } else {
      if (state.isPrelude) state.addinclude(mpath)
      // should be unnecessary ouside of Prelude
      val sym =
      if (state.isPrelude || !((PVSTheory.rootdpath / "Prelude") <= mpath)) {
        if (state.th.includes.inPars(mpath) contains true) OMV(LocalName(mpath) / id) else
        //OMS(state.th.path ? (LocalName(mpath) / id))
          OMS(mpath ? id)
      }
      else // OMS(PVSTheory.preludepath ? (LocalName(mpath) / id))
        OMS(mpath ? id)
      // apply theory parameters
      if (allactuals.nonEmpty) PVSTheory.parambind(sym,allactuals map doObject) else sym//ApplySpine(sym, allactuals map (a => doObject(a)): _*) else sym
      // println("Yields: " + ret)
    }
  }
  */

  /* library_id match {

  }

  def doMPath(thname : theory_name, isimport : Boolean = false) : MPath = {
    var (thid,library_id,mappings,opttarget,allactuals) =
      (thname.id,thname.library_id,thname.mappings,thname.target,thname.actuals ::: thname.dactuals)
    val doc =
      if (library_id=="") {
        if (thid == state.th.name.toString) path
        else if (isimport) {
          val prel = controller.getO(DPath((URI.http colon "pvs.csl.sri.com") / "Prelude") ? thid)
          if (prel.isDefined) DPath((URI.http colon "pvs.csl.sri.com") / "Prelude")
          else state.th.path.^^
        }
        else {
          val optth = state.th.includes.find(p => p.^^ == state.th.path.^^ && p.name.toString == thid)
          if (optth.isDefined) optth.get.^^
          else DPath((URI.http colon "pvs.csl.sri.com") / "Prelude")
        }
      }
      else {
        DPath(state.th.path.parent.uri.resolve(library_id))
      }
    // DPath((URI.http colon "pvs.csl.sri.com") / (if (library_id=="") "Prelude" else  library_id))

    // redirect booleans and equalities
    if (doc.toString == "http://pvs.csl.sri.com/Prelude" && (thid == "booleans" || thid == "equalities")) {
      // println("Yields: " + OMS(PVSTheory.thpath ? id))
      return PVSTheory.thpath
    }
    // if ((doc ? thid)!=state.th.path) println("PATH TO: " + (doc ? thid))
    doc ? thid
  } */
}
