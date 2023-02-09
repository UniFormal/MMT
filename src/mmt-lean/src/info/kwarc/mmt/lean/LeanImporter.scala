package info.kwarc.mmt.lean

import info.kwarc.mmt._
import api._
import utils._
import archives._
import objects._
import symbols._
import modules._
import documents._
import notations._
import lf._

/** This importer for lean libraries expects a single file in lean's low-level text export format.
  * It uses the trepplein checker for lean to read the file.
  *
  * To produce the necessary output files, run (for the standard library that is part of lean)
  * cd lean
  * cd lib\lean\library
  * ..\..\..\bin\lean.exe --export=..\..\..\library.out --recursive
  * and accordingly for other libraries. Instead of "--recursive", a specific file path may be given.
  *
  * Then move the file library.out into the source folder of an archive and run this importer.
  * It will produce a single theory for the entire export.
  */
class LeanImporter extends NonTraversingImporter {
   def key = "lean-omdoc"

   def clean(a: Archive,in: FilePath) {}//TODO

   def build(a: Archive, which: Build, in: FilePath, errorCont: Option[ErrorHandler]) {
     val ln = LocalName(in.stripExtension.segments:_*)
     val thy = Theory(Lean.leanBase, ln, Some(Lean.leanThy))
     val thyP = thy.path
     val doc = new Document(DPath(a.narrationBase) / ln)
     controller.add(doc)
     controller.add(thy)
     controller.add(MRef(doc.path, thyP))
     val ll = new LeanToLF(thyP)
     var anonymousDeclCounter = 0
     // read Lean's export file
     val file = a/source/in
     val exportedCommands = TextExportParser.parseFile(file.toString)
     val modifications = exportedCommands.collect {case ExportedModification(mod) => mod}
     // reverse ensures the unicode notation comes first
     val notations = Map() ++ exportedCommands.collect {case ExportedNotation(not) => not.fn -> not}.reverse
     // translate all modifications into MMT declarations and add them
     def add(d: Declaration) = {
       // log("adding " + d.name) // controller.presenter.asString(d))
       controller.add(d)
     }
     // the Lean theory, needed so that the compilation methods can do some checks
     var preenv: PreEnvironment = Environment.default
     var progress = 0
     val numMods = modifications.length
     log(numMods + " declarations found")
     modifications.foreach {m =>
       progress += 1
       if (progress % 100 == 0) log(progress + " declarations processed")
       val mC = m.compile(preenv) // needed for inductive types etc.
       preenv = preenv.addNow(mC) // maintain the lean environment
       m match {
         case d: AxiomMod =>
           add(ll.constant(thyP,d.name,d.univParams,d.ty,None,notations.get(d.name)))
         case d: DefMod =>
           add(ll.constant(thyP,d.name,d.univParams,d.ty,Some(d.value),notations.get(d.name)))
         case _ =>
           // other declarations are elaborated
           mC.decls.foreach {d =>
             val dM = ll.constant(thyP,d.name,d.univParams,d.ty,None,None)
             add(dM)
           }
           mC.rules.foreach {r =>
             val name = r.lhs match {
               case Apps(Const(n,_),_) =>
                 val nr = ll(n) / "reduce"
                 var i = 0
                 while (thy.declares(nr / i.toString)) {
                   i += 1
                 }
                 nr / i.toString
               case _ =>
                 // probably impossible
                 anonymousDeclCounter += 1
                 LocalName("_","reduce") / anonymousDeclCounter.toString
             }
             val rM = ll.rule(thyP,name,r)
             add(rM)
           }
       }
     }
     if (anonymousDeclCounter > 0) {
       log("generated names for " + anonymousDeclCounter.toString + " anonymous reduction rules")
     }
     importDocument(a, doc)
   }
}

object Lean {
  val leanBase = DPath(URI.scheme("latin") !/ "lean")
  val leanThy = leanBase ? "Lean"
  def cic(s: String) = OMS(leanThy ? s)
  def cicA(s: String)(args: Term*) = ApplySpine(cic(s), args:_*)
}
import Lean._

object LeanHOAS extends ChurchNestedHOASNotation(HOAS(leanThy ? "App", leanThy ? "Lam"), LF.hoas)

/** straightforward translations of Lean to LF expressions */
class LeanToLF(lib: MPath) {
  def apply(n: Name): LocalName = n match {
    case Name.Str(p,n) => apply(p) / n
    case Name.Num(p,n) => apply(p) / n.toString
    case Name.Anon => LocalName(Nil)
  }

  import info.kwarc.mmt.lean.Level._

  def apply(l: Level): Term = l match {
    case Param(n) => OMV(apply(n))
    case Zero => cic("Zero")
    case Succ(l) => cicA("Succ")(apply(l))
    case Max(l,m) => cicA("Max")(apply(l),apply(m))
    case IMax(l,m) => cicA("IMax")(apply(l),apply(m))
  }

  def apply(e: Expr)(implicit vars: List[Binding]): Term = e match {
    case Const(n,ls) =>
      ApplyGeneral(OMS(lib ? apply(n)),ls.toList map apply)
    case lean.Var(i) =>
      val b = vars(i)
      OMV(apply(b.prettyName))
    case LocalConst(of,_) =>
      OMV(apply(of.prettyName))
    case Sort(l) => cicA("Sort")(apply(l))
    case App(a,b) =>
      cicA("App")(apply(a),apply(b))
    case Lam(bind,bod) =>
      val bindFresh = makeFresh(bind)
      cicA("Lambda")(apply(bind.ty),Lambda(apply(bindFresh.prettyName),cic("expr"),apply(bod)(bindFresh :: vars)))
    case info.kwarc.mmt.lean.Pi(bind,bod) =>
      val tyT = apply(bind.ty)
      if (bind.prettyName == Name.Anon) {
        cicA("arrow")(tyT,apply(bod)(bind::vars)) // bind is not used but needed to make the indices fit
      } else {
        val bindFresh = makeFresh(bind)
        val bdT = apply(bod)(bindFresh::vars)
        cicA("Pi")(tyT, Lambda(apply(bindFresh.prettyName),cic("expr"), bdT))
      }
    case Let(bind,df,bod) =>
      val bindFresh = makeFresh(bind)
      cicA("Let")(apply(bind.ty),apply(df),Lambda(apply(bindFresh.prettyName),cic("expr"),apply(bod)(bindFresh :: vars)))
  }
  // lean uses de-Bruijn indices and names are just suggestions; so we pick fresh names to avoid shadowing
  def makeFresh(b: Binding)(implicit vars: List[Binding]) = {
    val names = vars.map(_.prettyName)
    val n = b.prettyName
    val fresh = if (!(names contains n)) n else {
      var i = 1
      while (names contains (Name.Num(n,i))) {
        i += 1
      }
      Name.Num(n,i)
    }
    b.copy(prettyName = fresh)
  }

  def levelDecl(n: LocalName) = VarDecl(n, cic("level"))

  // name: {params} ty = [params] vl
  def constant(mod: MPath, name: Name, params: Vector[Param], ty: Expr, vl: Option[Expr], nota: Option[lean.Notation]): Constant = {
    val ctx = Context(params.map {p => levelDecl(apply(p.param))} :_*)
    val tp = lf.Pi.applyOrBody(ctx, cicA("Expr")(apply(ty)(Nil)))
    val df = vl map {e => Lambda.applyOrBody(ctx,apply(e)(Nil))}
    val nt = nota map {n =>
      val delim = Delim(n.op)
      val Pis(args,_) = ty
      val total = params.length + args.length
      // implicit: universe params and leading Lean-implicit (= non-default) arguments
      val impl = params.length + args.takeWhile(_.of.info != BinderInfo.Default).length
      val expl = total - impl
      val fix = n match {
        case _:lean.Prefix => Prefix(delim, impl, expl)
        case _:lean.Postfix => Postfix(delim, impl, expl)
        case _:lean.Infix => Infix(delim, impl, expl, None)
      }
      TextNotation(fix, Precedence.integer(n.prio),None)
    }
    Constant(OMMOD(mod),apply(name),Nil,Some(tp),df, None, NotationContainer(nt))
  }

  // c/reduce : {params} {r.ctx} r.constraints --> r.lhs == r.rhs
  def rule(mod: MPath, name: LocalName, r: ReductionRule) = {
    // freshen the unbound variables in the context
    implicit var ctxFresh: List[Binding] = Nil
    r.ctx.toList.map {b =>
      val bF = makeFresh(b)
      ctxFresh :+= bF
      bF
    }
    // we do not need the types of the variables if we assume we'll only reduce well-formed terms
    val ctxT = Context(ctxFresh.map(b => VarDecl(apply(b.prettyName), cic("expr"))) :_*)
    val constraints = r.defEqConstraints.map {case (x,y) => cic("equal")(apply(x), apply(y))}
    val conc = cic("equal")(apply(r.lhs), apply(r.rhs))
    val tp1 = lf.Pi.applyOrBody(ctxT, Arrow(constraints, conc))
    // ReductionRule does not have universe params. We should really collect them from its components.
    // But we lazily bind all free variables as levels (which may mask implementation errors).
    val params = tp1.freeVars.map(levelDecl)
    val tp = lf.Pi.applyOrBody(Context(params:_*), cicA("Expr")(tp1))
    Constant(OMMOD(mod), name, Nil, Some(tp), None, None)
  }

}