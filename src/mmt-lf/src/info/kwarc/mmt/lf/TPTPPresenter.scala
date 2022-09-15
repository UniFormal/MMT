package info.kwarc.mmt.lf

import info.kwarc.mmt.api._
import archives._
import documents._
import presentation._
import modules._
import symbols._
import objects._
import utils._

object TPTP {
   abstract class TPTPFormula
   case class Builtin(name: String) extends TPTPFormula {
      override def toString = "$" + name
   }
   case class Sym(name: String) extends TPTPFormula {
      override def toString = {
         val escaped = Escape(name, '\\' -> "\\\\", '\'' -> "\\'")
         s"'$escaped'"
      }
   }
   case class Var(name: String) extends TPTPFormula {
      override def toString = {
         val upperFirst = if (!name(0).isUpper) "X" else ""
         val escaped = Escape(name, '\'' -> "prime", '/' -> "slash")
         //if (escaped.exists(c => !c.isLetter && !c.isDigit)) println(name)// test for illegal characters
         upperFirst + escaped
      }
   }
   case class Lit(name: String) extends TPTPFormula {
      override def toString = name
   }
   case class Unary(name: String, first: TPTPFormula) extends TPTPFormula {
      override def toString = s"($name $first)"
   }
   case class Binary(name: String, first: TPTPFormula, second: TPTPFormula) extends TPTPFormula {
      override def toString = s"($first $name $second)"
   }
   case class FOApp(fun: Sym, args: List[TPTPFormula]) extends TPTPFormula {
      override def toString = s"$fun(${args.mkString(",")})"
   }
   case class Bind(name: String, vars: List[(Var, TPTPFormula)], scope: TPTPFormula) extends TPTPFormula {
      override def toString = {
         val varsS = vars.map{case (v,t) => s"$v:$t"}.mkString(",")
         s"$name[$varsS]: ($scope)"
      }
   }
   case class Decl(name: String, role: String, body: TPTPFormula) {
      override def toString = s"thf($name,$role,$body)."
   }
   case class Incl(f: utils.URI) {
      override def toString = s"include(${f.toString})."
   }
}

import TPTP._

object TPTPObjectPresenter extends ObjectPresenter {
   implicit def termToTPTP(t: Term): TPTPFormula = t match {
      case Univ(_) => Builtin("tType")
      case OMV(n) => Var(n.toPath)
      case OMS(p) => Sym(p.toPath) //temporary for debugging, should be p.toPath
      case l: OMLITTrait => Lit(l.toString)
      case Arrow(a,b) => Binary(">", a, b)
      case Pi(x,a,b) => Bind("!>", List((Var(x.toPath),a)), b)
      case Lambda(x,a,t) => Bind("^", List((Var(x.toPath),a)), t)
      //case ApplySpine(OMS(p),args) => FOApp(termToTPTP(OMS(p)).asInstanceOf[Sym], args map termToTPTP)
      case Apply(f,a) => Binary("@", f, a)
      case OMSemiFormal(_) => Builtin("ERROR")
      case ComplexTerm(_,_,_,_) => Builtin("ERROR")
   }
   private def tmOptToTPTP(t: Option[Term], sep:String) = t.map(termToTPTP).map(sep+_).getOrElse("")
   def apply(o: Obj, origin: Option[CPath])(implicit rh : RenderingHandler) = {o match {
      case t: Term => rh(termToTPTP(t).toString)
      case c: Context => c.map {case VarDecl(x,_,t,d,_) =>
         Var(x.toPath).toString + tmOptToTPTP(t, ":") + tmOptToTPTP(d, "=")
      }.mkString(",")
      case s: Substitution => apply(s.asContext, origin)
   }}
}

class TPTPPresenter extends Presenter(TPTPObjectPresenter) {
   val key = "lf-tptp"
   override def outExt = "tptp"
   private implicit def inclToString (i: Incl) = i.toString
   private lazy val lup = controller.globalLookup

   /**
    * @param home a theory
    * @param t a term over home
    * @return list of (meta-theory M, list of (theory T that is imported into M, list of symbols from T in t))
    */
   private def getConstantsGroupedByMetaTheory(home: Term, t: Term): Iterable[(MPath,Iterable[(MPath,List[GlobalName])])] = {
      val cons = Obj.getConstants(t).groupBy(_.module)
      val metas = TheoryExp.metas(home)(lup).reverse ::: List(home.toMPath)
      cons.groupBy {case (thy, ps) =>
         metas.find(m => lup.hasImplicit(OMMOD(thy), OMMOD(m))).getOrElse {
            log("symbols from " + thy + " occurring in " + home + " appear to be invalid")
            //recover by defaulting to the theory itself
            metas.head
         }
      }
   }

   protected def doName(p: GlobalName)(implicit rh : RenderingHandler): Unit = {apply(OMS(p),None)}

   def apply(e : StructuralElement, standalone: Boolean = false)(implicit rh : RenderingHandler): Unit = {e match {
      case d: Document =>
         d.getDeclarations.foreach {i => apply(i)}
      case r: DRef =>
         rh << Incl(r.parent.uri.relativize(r.target.uri.setExtension(outExt)))
      case r: MRef =>
         rh << Incl(r.parent.uri.relativize(r.target.doc.uri)/(r.target.name.toPath+"."+outExt))
      case t: Theory =>
         controller.simplifier(t)
         t.meta.foreach {m =>
            if (m != LF.theoryPath) {
               val i = Include(OMMOD(t.path), m, Nil)
               apply(i)
               rh << "\n"
            }
         }
         val decs = t.getDeclarationsElaborated
         decs.foreach {d =>
            apply(d)
            rh << "\n"
         }
      case c: Constant =>
         rh << "thf("
         doName(c.path)
         val role = "type"
         rh << ","+role+","
         doName(c.path)
         c.tp.foreach {t =>
           rh << " : "
           apply(t, None)
         }
         c.df.foreach {d =>
            // rh << " = "
            // apply(d, None)
            val cons = getConstantsGroupedByMetaTheory(c.home, d) // .init removes framework-level theories
            cons.init.foreach {case (m,tps) =>
               rh << "," + Sym(m.toPath) + ":"
               tps.foreach {case (t,ps) =>
                  ps.foreach {p =>
                     rh << " "
                     doName(p)
                  }
               }
            }
         }
         rh << ")."
      case PlainInclude(q,p) =>
         val f = p.doc.uri.relativize(q.doc.uri)/(q.name.toPath+"."+outExt)
         rh << Incl(f)
      case _: Structure => //ignored because of flattening
      case _: Module =>
      case _: RuleConstant =>
      case _: NestedModule =>
   }}
}
