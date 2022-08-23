package info.kwarc.mmt.pvs

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.opaque._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.checking._
import documents._
import modules._
import parser._
import info.kwarc.mmt.api.objects._
import utils._
import archives._
import syntax._
import info.kwarc.mmt.lf._
import info.kwarc.mmt.api.uom.StandardRat

class TheoryState(val parent : DPath, val name : LocalName, val meta : MPath) {
  val path : MPath = parent ? name
  var parameters : Context= Context.empty
  protected var decls : List[Declaration] = Nil
  def add(d : Declaration) = decls ::= d
  def getDeclarations = decls.reverse
  def toTerm = OMMOD(path)
  def declares(n : LocalName) : Boolean = decls.exists(d => d.name == n)
  object includes {
    var stored : List[(MPath,Boolean)] = Nil
    def contains(p : MPath) = stored.exists(a => a._1 == p)
    def ::=(p : MPath, par : Boolean = false) = {
      deps ::= p
      stored ::= (p,par)
    }
    def find(cond : MPath => Boolean) : Option[MPath] = stored.map(_._1).find(cond)
    def inPars(p : MPath) : Option[Boolean] = stored.find(q => q._1 ==p).map(_._2)
  }
  var deps : List[MPath] = Nil
}

abstract class TranslationState {
  protected var unknowns = 0
  var vars : Context = Context.empty
  protected def doiName(i : Int, istp : Boolean) = LocalName("") / {if (istp) LocalName("I") else LocalName("i")} / i.toString
  def doUnknown = OMV(doiName({unknowns+=1;unknowns-1},false))

  def newName(s:String,start:Int = 1,ln : Option[LocalName] = None) : LocalName
  val isPrelude : Boolean
  def doSourceRef(o: Object, oM: Term) : Unit
  val path : MPath
  def getLocal(ln : LocalName) : Term
  def addinclude(p : MPath) : Unit
}

class ImportState(bt:BuildTask, val th : TheoryState, val isPrelude : Boolean) extends TranslationState {
  // val isPrelude : Boolean
  var inFormals = true
  var tcc : Option[FinalConstant] = None
  var inductive : Option[GlobalName] = None

  val path = th.path


  def getLocal(ln : LocalName) : Term = if (th.parameters.exists(v => v.name==ln || inductive.contains(path ? ln))) OMV(ln)
  else OMS(path ? ln)

  def doSourceRef(o: Object, oM: Term) = {
    if (o.place!="") {
      val List(bR, bC, eR, eC) = stringToList(o.place, " ").map(_.toInt)
      val reg = SourceRegion(SourcePosition(-1, bR, bC), SourcePosition(-1, eR, eC))
      SourceRef.update(oM, SourceRef(FileURI(bt.inFile), reg))
    }
  }

  def newName(s:String,start:Int = 1,ln : Option[LocalName] = None) : LocalName = {
    val n = ln.getOrElse(LocalName(s))
    if (!th.declares(n) && !th.parameters.exists(v => v.name==n)) n
    else {
      val newn = LocalName(s + "_" + start)
      newName(s, start + 1,Some(newn))
    }
  }

  private def bindUnknowns(t : Term) = {
    val symbs = t.freeVars.collect{
      case ln if ln.toString.startsWith("""/i/""") => ln
    }
    val cont = symbs.flatMap(n => {
      val i = (0 until unknowns).find(j => n == doiName(j,false))/*.getOrElse(
          throw new Exception("Wrong free Variable: " + n + " in " + t)
        )*/
      if (i.isDefined)
        List(VarDecl(doiName(i.get,true), OMS(Typed.ktype)), VarDecl(n, OMV(doiName(i.get,true))))
      else throw GeneralError("No unknown " + n)
    })
    if (unknowns > 0 && cont.nonEmpty) OMBIND(OMS(Path.parseS("http://cds.omdoc.org/mmt?mmt?unknown", NamespaceMap.empty)),
      cont,
      t)
    else t
  }
  def reset = {
    unknowns = 0
    vars = Context.empty
  }
  def bind(t : Term) : Term = {
    val allvars = t.freeVars.collect{case v if vars.exists(_.name == v) && !th.parameters.exists(_.name == v)=> vars.find(_.name == v).get}
    bindUnknowns(if (allvars.nonEmpty) Pi(allvars.distinct.map{vd => vd.copy(tp = vd.tp.map {x => PVSTheory.expr(x)})},t) else t)
  }
  //def addinclude(p : MPath) =

  def addinclude(p : MPath) : Unit =
    if (inFormals && !th.includes.contains(p)) {
      th.parameters = th.parameters ++ IncludeVarDecl(p,Nil)//DerivedVarDeclFeature(LocalName(p), BoundInclude.feature, OMMOD(p))
      th.includes ::= (p,true)
    } else if (!th.includes.contains(p)) {
      th add PlainInclude(p,th.path)// th add BoundInclude(th.path,p)
      th.includes ::= p
    }

}

case class Dependency(p : MPath) extends Exception

class PVSImportTask(val controller: Controller, bt: BuildTask, index: Document => Unit) extends Logger with MMTTask {
  def logPrefix = "pvs-omdoc"
  protected def report = controller.report

  var state : ImportState = null
  def objectLevel = ObjectLevelTranslator(state,controller)

  def doDocument(d: pvs_file) : BuildResult = {
    try {
      val modsM = d._modules map doModule
        modsM.flatMap(_.deps).distinct.foreach(d => {
          controller.getO(d) match {
            case Some(th: Theory) =>
            case _ =>
              log("Unresolved dependency: " + d + " in " +modsM.head.path)
              throw Dependency(modsM.head.deps.head)
          }//.asInstanceOf[DeclaredTheory]
        })
      log("Translated: " + state.th.name)

      val doc = new Document(bt.narrationDPath, FileLevel)
      val ths = modsM map (m => {
        val theory = new Theory(m.parent,m.name,Some(m.meta), Theory.noParams, Theory.noBase)//,m.parameters)
        if (m.parameters.nonEmpty) {
          log(m.parameters.toString)
          theory.paramC.set(m.parameters)
        }
        controller add theory
        m.getDeclarations foreach { d =>
          controller.add(d)
        }
        // controller simplifier theory
        doc add MRef(bt.narrationDPath, theory.path)
        theory
      })
      controller.add(doc)
      log("Dependencies: " + state.th.deps.mkString(", "))
      log("Checking:")
      logGroup {
        val checker = controller.extman.get(classOf[Checker], "mmt").getOrElse {
          throw GeneralError(s"no checker $id found")
        }.asInstanceOf[MMTStructureChecker]
        ths foreach {p =>
          val ce = new CheckingEnvironment(controller.simplifier, new ErrorLogger(report), RelationHandler.ignore, this)
          // checker.applyWithTimeout(p,Some(30000))(ce)
        }
      }
      index(doc)
      BuildSuccess(modsM.flatMap(_.deps).map(LogicalDependency),modsM.map(m => LogicalDependency(m.path)))
    } catch {
      case Dependency(p) =>
        state.th.deps = (p::state.th.deps).distinct
        log("FAIL: " + state.th.name + " depends on " + state.th.deps)
        //sys.exit
        MissingDependency(state.th.deps.map(LogicalDependency),List(LogicalDependency(state.th.path)),state.th.deps.map(LogicalDependency))
      case t : Exception =>
        log("Exception: " + t.getMessage)
        t.printStackTrace()
        sys.exit()
    }
  }

  def doModule(m: syntax.Module): TheoryState = m match {
    case theory(named, theory_formals, assuming, exporting, decls) =>
      val path = bt.narrationDPath.^!.^!
      val isPrel = DPath(URI.http colon "pvs.csl.sri.com") / "prelude" <= path //path.toString == "http://pvs.csl.sri.com/prelude"
      val meta = if (isPrel) PVSTheory.thpath else PVSTheory.preludepath
      val theory = new TheoryState(path, LocalName(named.id_proper), meta)
      state = new ImportState(bt,theory,isPrel) /* {
        val isPrelude = isPrel
        val th = theory
      } */
      theory.deps ::= meta
      if (isPrel && named.id_proper == "booleans") {
        log("Skipped Booleans")
        // theory add new OpaqueText(path,List(StringFragment(""))).
        return theory
      }
      if (isPrel && named.id_proper == "equalities") {
        log("Skipped Equalities")
        return theory
      }

      log("Doing " + theory.path)

      theory_formals foreach doFormal
      assuming foreach doAssumption
      decls foreach (doDecl(_)(false))
      state.th

    case datatype(TopDatatypeBody(named, theory_formals, importings, constructors)) =>
      // println(" -- Datatype: " + named.id_proper)
      val path = bt.narrationDPath.^!.^!
      val isPrel = path.toString == "http://pvs.csl.sri.com/prelude"
      val meta = if (isPrel) PVSTheory.thpath else PVSTheory.preludepath
      val theory = new TheoryState(path, LocalName(named.id_proper /* + "_adt" */ ), meta)
      state = new ImportState(bt,theory,isPrel) /*(this) {
        val isPrelude = isPrel
        val th = theory
      } */
      theory.deps::=meta

      log("Doing " + theory.path)

      importings foreach doFormal
      theory_formals foreach doFormal
      val datatp = Constant(state.th.toTerm, LocalName(named.id_proper), Nil, Some(PVSTheory.tp.term), None, None)
      state.th add datatp

      constructors.foreach(doDatatypeConstructor(_,datatp)) // TODO subtype_id_proper ?
      state.th

    case _ =>
      println(" -- OTHER: " + m.getClass)
      sys.exit()
  }

  def doDatatypeConstructor(con:constructor,datatp:FinalConstant) = {
    val conname = LocalName(con.named.id_proper)
    state.reset
    val accs = con.accessors.map(a => {
      val tp = objectLevel.doType(a._type)
      (LocalName(a.named.id_proper), tp)})
    val contp = if (accs.isEmpty) datatp.toTerm
    else if (accs.length == 1) PVSTheory.fun_type(accs.head._2, datatp.toTerm)
    else PVSTheory.fun_type(PVSTheory.tuple_type(accs.map(_._2)), datatp.toTerm)
    val const = Constant(state.th.toTerm, conname, Nil, Some(state.bind(PVSTheory.expr(contp))), None, Some("Constructor"))
    val reco = Constant(state.th.toTerm, LocalName(con.recognizer), Nil, Some(
      state.bind(PVSTheory.expr(PVSTheory.fun_type(datatp.toTerm, PVSTheory.bool.term)))), None, Some("Recognizer"))
    state.th add reco
    accs.foreach(ac =>
      state.th add Constant(state.th.toTerm, ac._1, Nil, Some(
        state.bind(PVSTheory.expr(PVSTheory.fun_type(PVSTheory.setsub(datatp.toTerm, reco.toTerm), ac._2)))
      ), None, Some("Accessor"))
    )
  }

  def doAssumption (ad:AssumingDecl) : Unit = ad match {
    case assumption(named,assertion) => doDecl(axiom_decl(named,assertion))(true)
    case d : Decl => doDecl(d)(true)
    case _ =>
      println("TODO Assumption: "+ad.getClass)
      sys.exit()
  }


  def doFormal(f:FormalParameter) : Unit = {
    state.inFormals = true
    f match {
      case formal_type_decl(named,nonempty) =>
        state.reset
        val v = VarDecl(
          LocalName(named.named.id_proper),
          if (nonempty.nonempty_p) PVSTheory.nonempty.tps else PVSTheory.tp.term)
        state.th.parameters = state.th.parameters ++ v
        state.inFormals = false
        /*
        if (nonempty.nonempty_p && nonempty.contains.isDefined) {
          state.th.add(Constant(state.th.toTerm,newName("INTERNAL_Assumption"),Nil,Some(
            state.bind(PVSTheory.is_nonempty(OMV(v.name),doExprAs(nonempty.contains.get,OMV(v.name))))),
            None,Some("Assumption")))
        } else if (nonempty.nonempty_p) {
          state.th.add(Constant(state.th.toTerm,newName("INTERNAL_Assumption"),Nil,Some(
            PVSTheory.nonempty(OMV(v.name))),
            None,Some("Assumption")))
        }
        */
      case formal_subtype_decl(ChainedDecl(nd @ NamedDecl(_,_,_),_,_),nonempty,sup,pred) =>
        val name = LocalName(nd.id_proper)
        state.reset
        /* Prelude */ // val actsup = objectLevel.doType(sup._declared)
        /* NASA */ val actsup = objectLevel.doType(sup)
        val v = VarDecl(name, state.bind(PVSTheory.powertp(actsup)))
        state.th.parameters = state.th.parameters ++ v
        state.inFormals = false
        /*
        if (nonempty.nonempty_p && nonempty.contains.isDefined) {
          state.th.add(Constant(state.th.toTerm,newName("INTERNAL_Assumption"),Nil,Some(
            state.bind(PVSTheory.is_nonempty(OMV(v.name),doExprAs(nonempty.contains.get,OMV(v.name))))),
            None,Some("Assumption")))
        } else if (nonempty.nonempty_p) {
          state.th.add(Constant(state.th.toTerm,newName("INTERNAL_Assumption"),Nil,Some(
            PVSTheory.nonempty(OMV(v.name))),
            None,Some("Assumption")))
        }
        */
        doDecl(pred)(true)

      case formal_const_decl(ChainedDecl(nd @ NamedDecl(_,_,_),_,_),tp) =>
        state.reset
        val fulltp = objectLevel.doType(tp._internal)
        val v = VarDecl(LocalName(nd.id_proper), state.bind(PVSTheory.expr(fulltp)))
        state.th.parameters = state.th.parameters ++ v
        state.inFormals = false
      case importing(un,n) =>
      case _ =>
        println("TODO Formal: " + f.getClass + ": " + f)
        sys.exit()
    }
  }

  def doDecl(d: Decl)(isAss : Boolean = false) : Unit = {
    //    println(d)
    state.inFormals = false
    d match {
      case md : macro_decl => doDecl(md.decl)(isAss)

      case var_decl(id, unnamed, tp) => // Not needed

      case tcc_decl(ChainedDecl(nd @ NamedDecl(_, _, _), _, _), Assertion(kind, formula)) =>
        state.reset
        val name = LocalName(nd.id_proper)//if (id_proper.dropRightUntil().endsWith("TCC")) newName(id_proper) else newName(id_proper + "_TCC")
        val c = Constant(state.th.toTerm, name, Nil, Some(state.bind(PVSTheory.proof(kind, objectLevel.doExprAs(formula, PVSTheory.bool.term)))), None,
          Some(if (isAss) "Assumption_TCC" else "TCC"))
        state.th add c
        state.tcc = Some(c)

      case const_decl(ChainedDecl(nd @ NamedDecl(_, _, _), _, _), arg_formals, tp, defOpt) =>
        state.reset
        val tptm = objectLevel.doType(tp._declared)
        val exptp = arg_formals.flatMap(_._bindings).foldRight(tptm)((b, t) =>
          PVSTheory.pvspi(LocalName(b.id), objectLevel.doType(b._type), t)
        )
        val defi = defOpt.map(d => arg_formals.flatMap(_._bindings).foldRight(objectLevel.doExprAs(d, tptm), tptm)((b, t) =>
          (PVSTheory.pvslambda(LocalName(b.id), objectLevel.doType(b._type), t._2, t._1),
            PVSTheory.pvspi(LocalName(b.id), objectLevel.doType(b._type), t._2)
            ))._1)
        state.th add Constant(state.th.toTerm, state.newName(nd.id_proper), Nil, Some(state.bind(PVSTheory.expr(exptp))), defi.map(state.bind),
          if (isAss) Some("Assumption") else None)

      case formula_decl(ChainedDecl(nd @ NamedDecl(_, _, _), _, _), Assertion(kind, form)) =>
        state.reset
        val phi = objectLevel.doExprAs(form, PVSTheory.bool.term)
        state.th add Constant(state.th.toTerm, state.newName(nd.id_proper), Nil, Some(state.bind(PVSTheory.proof(kind, phi))), None, if (isAss) Some("Assumption") else None)

      case conversion_decl(UnnamedDecl(_, _, _), kind, expr) => // TODO

      case auto_rewrite(UnnamedDecl(_, _, _), key, kind, namelist) => // TODO

      case def_decl(named, arg_formals, tp, df, optMeasure, optOrder) =>
        state.reset
        val tptm = objectLevel.doType(tp._internal)
        val name = state.newName(named.named.id_proper)
        val exptp = arg_formals.flatMap(_._bindings).foldRight(tptm)((b, t) =>
          PVSTheory.pvspi(LocalName(b.id), objectLevel.doType(b._type), t)
        )
        state.inductive = Some(state.th.path ? name)
        //state.vars = state.vars++VarDecl(name,Some(exptp),None,None)
        val (defi, _) = arg_formals.flatMap(_._bindings).foldRight(objectLevel.doExprAs(df, tptm), tptm)((b, t) =>
          (PVSTheory.pvslambda(LocalName(b.id), objectLevel.doType(b._type), t._2, t._1),
            PVSTheory.pvspi(LocalName(b.id), objectLevel.doType(b._type), t._2)
            ))
        //state.vars = Context.empty
        state.inductive = None
        state.th add Constant(state.th.toTerm, name, Nil, Some(state.bind(PVSTheory.expr(exptp))),
          Some(state.bind(PVSTheory.recursor(exptp, name, defi))), if (isAss) Some("Assumption") else None)

      case ind_decl(named, arg_formals, tp, df) =>
        //doDecl(def_decl(ChainedDecl(named,false,false),arg_formals,tp,df,None,None))(isAss) // TODO?
        state.reset
        val tptm = objectLevel.doType(tp._internal)
        val name = state.newName(named.id_proper)
        val exptp = arg_formals.flatMap(_._bindings).foldRight(tptm)((b, t) =>
          PVSTheory.pvspi(LocalName(b.id), objectLevel.doType(b._type), t)
        )
        state.inductive = Some(state.th.path ? name)
        //state.vars = state.vars++VarDecl(name,Some(exptp),None,None)
        val defi = PVSTheory.recursor(exptp, name, arg_formals.flatMap(_._bindings).foldRight(objectLevel.doExprAs(df, tptm), tptm)((b, t) =>
          (PVSTheory.pvslambda(LocalName(b.id), objectLevel.doType(b._type), t._2, t._1),
            PVSTheory.pvspi(LocalName(b.id), objectLevel.doType(b._type), t._2)
            ))._1)
        state.inductive = None
        //state.vars = Context.empty
        state.th add Constant(state.th.toTerm, name, Nil, Some(state.bind(PVSTheory.expr(exptp))),
          Some(state.bind(defi)), if (isAss) Some("Assumption") else None)

      case application_judgement(ond, nameexpr, arg_formals, tp) =>
        val name = state.newName(ond.id_proper.getOrElse("App_Judgment"))
        state.reset
        val (exp, restp) = objectLevel.doExpr(arg_formals.flatMap(_._bindings).foldLeft(nameexpr.asInstanceOf[Expr])((e, b) =>
          application("", e, varname_expr("", b.id, b._type), false)))
        val fulltp = Pi(arg_formals.flatMap(_._bindings).map(b => VarDecl(LocalName(b.id),
          PVSTheory.expr(objectLevel.doType(b._type)))),
          PVSTheory.tpjudg(exp, restp, objectLevel.doType(tp._internal)))
        state.th add Constant(state.th.toTerm, name, Nil, Some(state.bind(fulltp)), None,
          if (isAss) Some("Assumption") else None)

      case type_def_decl(nd @ NamedDecl(_, _, _), NonEmptiness(neb, exprOpt), arg_formals, df) =>
        state.reset
        val bindings = arg_formals.flatMap(_._bindings).map(b => (LocalName(b.id), objectLevel.doType(b._type)))
        val defi = bindings.foldRight[Term](objectLevel.doType(df._declared))((b, t) =>
          Lambda(b._1, PVSTheory.expr(b._2), t))
        val tp = bindings.foldRight[Term](PVSTheory.tp.term)((b, t) =>
          Pi(b._1, PVSTheory.expr(b._2), t))
        val name = state.newName(nd.id_proper)
        val c = Constant(
          state.th.toTerm,
          name,
          Nil,
          Some(if (neb) state.bind(PVSTheory.nonempty.tps) else state.bind(tp)), Some(state.bind(defi)),
          if (isAss) Some("Assumption") else None)
        state.th add c

      case axiom_decl(ChainedDecl(nd @ NamedDecl(_, _, _), _, _), Assertion(kind, form)) =>
        state.reset
        val phi = objectLevel.doExprAs(form, PVSTheory.bool.term)
        state.th add Constant(state.th.toTerm, state.newName(nd.id_proper), Nil, Some(state.bind(PVSTheory.proof(kind, phi))), None,
          if (isAss) Some("Assumption") else None)

      case subtype_judgement(nd @ OptNamedDecl(_, _, _, _), sub, sup) =>
        state.reset
        val subtp = objectLevel.doType(sub._internal)
        val suptp = objectLevel.doType(sup._internal)
        val c = Constant(state.th.toTerm, state.newName(nd.id_proper.getOrElse("INTERNAL_Judgment")), Nil, Some(
          state.bind(PVSTheory.subtpjudg(subtp, suptp))
        ), None, None)
        state.th add c
        state.th add Constant(state.th.toTerm, state.newName(nd.id_proper.getOrElse("INTERNAL_Assumption")), Nil,
          Some(state.bind(LFX.subtypeJudg(PVSTheory.expr(subtp), PVSTheory.expr(suptp)))),
          Some(state.bind(PVSTheory.subtpissubtype(subtp, suptp, c.path))), Some("Assumption"))

      case type_from_decl(ChainedDecl(nd @ NamedDecl(_, _, _), _, _), nonempty, tp) =>
        state.reset
        val name = state.newName(nd.id_proper)
        val realtp = objectLevel.doType(tp._declared)
        val c = Constant(state.th.toTerm, name, Nil, Some(PVSTheory.tp.term), None, None)
        state.th add c
        val d = Constant(state.th.toTerm, state.newName("INTERNAL_Assumption"), Nil,
          Some(state.bind(PVSTheory.subtpjudg(OMS(c.path), realtp))), None, Some("Assumption"))
        state.th add d
        state.th add Constant(state.th.toTerm, state.newName("INTERNAL_Assumption"), Nil,
          Some(state.bind(LFX.subtypeJudg(PVSTheory.expr(OMS(c.path)), PVSTheory.expr(realtp)))),
          Some(state.bind(PVSTheory.subtpissubtype(OMS(c.path), realtp, d.path))), Some("Assumption"))
        state.th add Constant(state.th.toTerm, state.newName(nd.id_proper + "_pred"), Nil,
          Some(state.bind(PVSTheory.expr(PVSTheory.fun_type(realtp, PVSTheory.bool.term)))), None, Some("Assumption"))
      // TODO add type equality name = setsubtype(name_pred,actsup)

      case type_decl(ChainedDecl(nd @ NamedDecl(_, _, _), _, _), nonempty) =>
        // if (state.th.path != (PVSTheory.rootdpath / "Prelude") ? "numbers" && id_proper!="number") {
        val c = Constant(state.th.toTerm, state.newName(nd.id_proper), Nil, Some(PVSTheory.tp.term), None, None)
        state.th add c
        if (nonempty) {
          state.th add Constant(state.th.toTerm, state.newName("INTERNAL_Assumption"), Nil, Some(
            PVSTheory.nonempty(c.toTerm)),
            None, if (isAss) Some("Assumption") else None)
        }

      case name_judgement(ond @ OptNamedDecl(_, _, _, _), nameexpr, tp) =>
        val name = state.newName(ond.id_proper.getOrElse("Name_Judgment"))
        state.reset
        val (exp, restp) = objectLevel.doExpr(nameexpr)
        val fulltp = PVSTheory.tpjudg(exp, restp, objectLevel.doType(tp._internal))
        state.th add Constant(state.th.toTerm, name, Nil, Some(state.bind(fulltp)), None, if (isAss) Some("Assumption") else None)

      case enumtype_decl(nd @ NamedDecl(_, _, _), enum_elts) =>
        state.th add Constant(state.th.toTerm, state.newName(nd.id_proper), Nil, Some(PVSTheory.tp.term),
          Some(PVSTheory.enumtype(enum_elts.map(_._id))), None)

      case importing(un, namedec) =>
        val path = objectLevel.doMPath(namedec)
        if (!(DPath((URI.http colon "pvs.csl.sri.com") / "prelude") <= path)) {
          state.addinclude(path)
        } // TODO fix

      case inline_datatype(InlineDatatypeBody(nd @ NamedDecl(_, _, _), arg_formals, constructors)) =>
        // TODO check if right
        val datatp = Constant(state.th.toTerm, LocalName(nd.id_proper), Nil, Some(PVSTheory.tp.term), None, None)
        state.th add datatp
        constructors foreach (doDatatypeConstructor(_, datatp))

      case theory_decl(ChainedDecl(nd @ NamedDecl(_, _, _), _, _), domain) =>
        val p = objectLevel.doMPath(domain)
        state.addinclude(p)
        //TODO : this should be named etc.

      case e@expr_judgement(optNamed, bindings, expr, tp) =>
        val name = state.newName(optNamed.id_proper.getOrElse("Name_Judgment"))
        state.reset
        val (exp, restp) = objectLevel.doExpr(expr)
        val fulltp = PVSTheory.tpjudg(exp, restp, objectLevel.doType(tp._internal))
        //println(exp)
        if (bindings.nonEmpty) {
          println("bindings in Decl expr_judgement: " + bindings)
          println(e)
          sys.exit()
        }
        state.th add Constant(state.th.toTerm, name, Nil, Some(state.bind(fulltp)), None, if (isAss) Some("Assumption") else None)
      case _ =>
        println("TODO Decl: " + d.getClass + ": " + d)
        sys.exit()
    }
  }
}
