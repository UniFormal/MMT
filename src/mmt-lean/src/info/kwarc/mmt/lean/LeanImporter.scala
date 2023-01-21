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

class LeanImporter extends NonTraversingImporter {
   def key = "lean-omdoc"

   def clean(a: Archive,in: FilePath) {}//TODO

   def build(a: Archive, which: Build, in: FilePath, errorCont: Option[ErrorHandler]) {
     val thy = Theory(DPath(a.narrationBase), LocalName(in.segments:_*), Some(Lean.leanThy))
     val thyP = thy.path
     val doc = new Document(thy.parent)
     controller.add(thy)
     controller.add(MRef(doc.path, thyP))
     val ll = new LeanToLF(thyP)
     // read Lean's export file
     val file = a/source/in
     val exportedCommands = TextExportParser.parseFile(file.toString)
     val modifications = exportedCommands.collect {case ExportedModification(mod) => mod}
     // reverse ensures the unicode notation comes first
     val notations = Map() ++ exportedCommands.collect {case ExportedNotation(not) => not.fn -> not}.reverse
     // translate all modifications into MMT declarations and add them
     def add(d: Declaration) = controller.add(d)
     modifications.foreach {
       case d: AxiomMod =>
         add(ll.constant(thyP, d.name, d.univParams, d.ty, None, notations.get(d.name)))
       case d: DefMod =>
         add(ll.constant(thyP, d.name, d.univParams, d.ty, Some(d.value), notations.get(d.name)))
       case d =>
         // other declarations are elaborated
         val iC = d.compile(Environment.default) // environment only needed for checking, which we won't do
         iC.decls.foreach {d =>
           val dM = ll.constant(thyP, d.name, d.univParams, d.ty, None, None)
           add(dM)
         }
         iC.rules.foreach {r =>
           val rM = ll.rule(thyP, r)
           add(rM)
         }
     }
     importDocument(a, doc)
   }
}

object Lean {
  val leanThy = DPath(URI("latin") / "lean") ? "Lean"
  def cic(s: String) = OMS(leanThy ? s)
}
import Lean._

object LeanHOAS extends ChurchNestedHOASNotation(HOAS(leanThy ? "App", leanThy ? "Lam"), LF.hoas)

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
    case Succ(l) => cic("Succ")(apply(l))
    case Max(l,m) => cic("Max")(apply(l),apply(m))
    case IMax(l,m) => cic("IMax")(apply(l),apply(m))
  }

  def apply(e: Expr)(implicit vars: List[Binding]): Term = e match {
    case Const(n,ls) =>
      ApplySpine(OMS(lib ? apply(n)),ls map apply: _*)
    case lean.Var(i) =>
      val b = vars(i)
      OMV(apply(b.prettyName))
    case LocalConst(of,_) =>
      OMV(apply(of.prettyName))
    case Sort(l) => cic("Sort")(apply(l))
    case App(a,b) =>
      cic("App")(apply(a),apply(b))
    case Lam(bind,bod) =>
      cic("Lambda")(apply(bind.ty),Lambda(apply(bind.prettyName),cic("expr"),apply(bod)(bind :: vars)))
    case info.kwarc.mmt.lean.Pi(bind,bod) =>
      cic("Pi")(apply(bind.ty),Lambda(apply(bind.prettyName),cic("expr"),apply(bod)(bind :: vars)))
    case Let(bind,df,bod) =>
      cic("Let")(apply(bind.ty),apply(df),Lambda(apply(bind.prettyName),cic("expr"),apply(bod)(bind :: vars)))
  }

  def levelDecl(n: LocalName) = VarDecl(n, cic("level"))

  // name: {params} ty = [params] vl
  def constant(mod: MPath, name: Name, params: Vector[Param], ty: Expr, vl: Option[Expr], nota: Option[lean.Notation]): Constant = {
    val ctx = Context(params.map {p => levelDecl(apply(p.param))} :_*)
    val tp = lf.Pi.applyOrBody(ctx, apply(ty)(Nil))
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
  def rule(mod: MPath, r: ReductionRule) = {
    val name = r.rhs match {
      case App(Const(n,_),_) => apply(n)/"reduce"
      case _ => LocalName(Nil) // probably impossible
    }
    // we do not need the types of the variables if we assume we'll only reduce well-formed terms
    val ctx = Context(r.ctx.map(b => VarDecl(apply(b.prettyName), cic("expr"))) :_*)
    implicit val vars = r.ctx.toList
    val constraints = r.defEqConstraints.map {case (x,y) => cic("equal")(apply(x), apply(y))}
    val conc = cic("equal")(apply(r.lhs), apply(r.rhs))
    val tp1 = lf.Pi.applyOrBody(ctx, Arrow(constraints, conc))
    // ReductionRule does not have universe params. We should really collect them from its components.
    // But we lazily bind all free variables as levels (which may mask implementation errors).
    val params = tp1.freeVars.map(levelDecl)
    val tp = lf.Pi.applyOrBody(Context(params:_*), tp1)
    Constant(OMMOD(mod), name, Nil, Some(tp), None, None)
  }

}