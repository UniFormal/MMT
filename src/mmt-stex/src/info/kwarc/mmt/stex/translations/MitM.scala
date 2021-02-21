package info.kwarc.mmt.stex.translations

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.{DPath, GlobalName, LocalName}
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.objects.{OMA, OMBIND, OMS, Term}
import info.kwarc.mmt.api.refactoring.{AcrossLibraryTranslation, AcrossLibraryTranslator}
import info.kwarc.mmt.api.symbols.{Constant, PlainInclude}
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.lf.{ApplySpine, Lambda, Typed}
import info.kwarc.mmt.odk.{LFX, NatLiterals}
import info.kwarc.mmt.stex.STeX
import info.kwarc.mmt.stex.xhtml.XHTMLTerm

object MitM {
  import DemoContent._
  import info.kwarc.mmt.MitM.{MitM => OMitM}

  def getTranslator(controller: Controller) = new AcrossLibraryTranslator(controller,translations,Nil,((path: GlobalName, _: Controller) =>
    Typed._base <= path ||
      LFX.ns <= path ||
      OMitM.basepath <= path),false)

  lazy val trl_nat = new AcrossLibraryTranslation {
    val tmm = c_nat.toTerm
    override def applicable(tm: Term)(implicit translator: AcrossLibraryTranslator): Boolean = tm match {
      case `tmm` => true
      case _ => false
    }
    override def apply(tm: Term)(implicit translator: AcrossLibraryTranslator): Term = OMitM.n
  }
  lazy val trl_impl = new AcrossLibraryTranslation {
    val tmm = c_impl.toTerm
    override def applicable(tm: Term)(implicit translator: AcrossLibraryTranslator): Boolean = tm match {
      case OMA(`tmm`,List(_,_)) => true
      case _ => false
    }
    override def apply(tm: Term)(implicit translator: AcrossLibraryTranslator): Term = {
      val OMA(`tmm`,List(a,b)) = tm
      ApplySpine(OMS(OMitM.logic ? "implies"),a,b)
    }
  }
  lazy val trl_even = new AcrossLibraryTranslation {
    val tmm = c_even.toTerm
    val divides = (OMitM.basepath / "core" / "arithmetics") ? "NaturalArithmetics" ? "divides"
    override def applicable(tm: Term)(implicit translator: AcrossLibraryTranslator): Boolean = tm match {
      case OMA(`tmm`,List(_)) => true
      case _ => false
    }
    override def apply(tm: Term)(implicit translator: AcrossLibraryTranslator): Term = {
      val OMA(`tmm`,List(a)) = tm
      ApplySpine(OMS(divides),NatLiterals(2),a)
    }
  }
  lazy val trl_square = new AcrossLibraryTranslation {
    val tmm = c_natexp.toTerm
    val exp = (OMitM.basepath / "core" / "arithmetics") ? "NaturalArithmetics" ? "exponentiation"
    override def applicable(tm: Term)(implicit translator: AcrossLibraryTranslator): Boolean = tm match {
      case OMA(`tmm`,List(_,_)) => true
      case _ => false
    }
    override def apply(tm: Term)(implicit translator: AcrossLibraryTranslator): Term = {
      val OMA(`tmm`,List(a,b)) = tm
      ApplySpine(OMS(exp),a,b)
    }
  }
  lazy val trl_forall = new AcrossLibraryTranslation {
    val tmm = c_forall.toTerm
    override def applicable(tm: Term)(implicit translator: AcrossLibraryTranslator): Boolean = tm match {
      case OMBIND(`tmm`,_,_) => true
      case _ => false
    }
    override def apply(tm: Term)(implicit translator: AcrossLibraryTranslator): Term = {
      val OMBIND(`tmm`,ctx,a) = tm
      ctx.foldRight(a)((vd,t) => ApplySpine(OMS(OMitM.forall),vd.tp.get,Lambda(vd.name,vd.tp.get,t))) // TODO safer
    }
  }
  lazy val trl_exists = new AcrossLibraryTranslation {
    val tmm = c_exists.toTerm
    override def applicable(tm: Term)(implicit translator: AcrossLibraryTranslator): Boolean = tm match {
      case OMBIND(`tmm`,_,_) => true
      case _ => false
    }
    override def apply(tm: Term)(implicit translator: AcrossLibraryTranslator): Term = {
      val OMBIND(`tmm`,ctx,a) = tm
      ctx.foldRight(a)((vd,t) => ApplySpine(OMS(OMitM.exists),Lambda(vd.name,vd.tp.get,t))) // TODO safer
    }
  }
  lazy val translations = List(trl_nat,trl_impl,trl_even,trl_square,trl_forall,trl_exists)
}

object DemoContent {
  lazy val th_set = Theory(STeX.set.doc,STeX.set.module.name,None)
  lazy val c_set = Constant(th_set.toTerm,STeX.set.name,Nil,None,None,None,XHTMLTerm.notation("\\set","Set"))
  lazy val th_nat = Theory(STeX.nat.doc,STeX.nat.module.name,None)
  lazy val c_nat = Constant(th_nat.toTerm,STeX.nat.name,Nil,Some(c_set.toTerm),None,None,XHTMLTerm.notation("\\NaturalNumbers","ℕ"))
  lazy val th_prop = Theory(STeX.prop.doc,STeX.prop.module.name,None)
  lazy val c_prop = Constant(th_prop.toTerm,STeX.prop.name,Nil,Some(c_set.toTerm),None,None,XHTMLTerm.notation("\\prop","prop"))
  lazy val th_fun = Theory(STeX.funtype.doc,STeX.funtype.module.name,None)
  lazy val c_fun = Constant(th_fun.toTerm,STeX.funtype.name,Nil,None,None,None,XHTMLTerm.notation("\\funtype{ 1 }","1⟶… prec -9990"))
  lazy val th_impl = Theory(DPath(URI.http colon "mathhub.info") / "smglom" / "logic",LocalName("Implication"),None)
  lazy val c_impl = Constant(th_impl.toTerm,LocalName("Implication"),Nil,Some(OMA(c_fun.toTerm,List(c_set.toTerm,c_set.toTerm,c_set.toTerm))),None,None,XHTMLTerm.notation("\\implication{ 1 }{ 2 }","1 ⟹ 2 prec 10"))
  lazy val th_div = Theory(DPath(URI.http colon "mathhub.info") / "smglom" / "arithmetics" / "natural_numbers",LocalName("Divisibility"),None)
  lazy val c_even = Constant(th_div.toTerm,LocalName("even"),Nil,Some(OMA(c_fun.toTerm,List(c_nat.toTerm,c_prop.toTerm))),None,None,XHTMLTerm.notation("\\even{ 1 }","even( 1 ) prec 50"))
  lazy val th_exp = Theory(DPath(URI.http colon "mathhub.info") / "smglom" / "arithmetics" / "natural_numbers",LocalName("Exponentiation"),None)
  lazy val c_natexp = Constant(th_exp.toTerm,LocalName("natexp"),Nil,Some(OMA(c_fun.toTerm,List(c_nat.toTerm,c_nat.toTerm))),None,None,XHTMLTerm.notation("\\natpow{ 1 }{ 2 }","1 ^ 2 prec 70"))

  lazy val th_forall = Theory(STeX.Forall.path.module.parent,STeX.Forall.path.module.name,None)
  lazy val c_forall = Constant(th_forall.toTerm,STeX.Forall.path.name,Nil,None,None,None,XHTMLTerm.notation("\\sforall{ V1 }{ 2 }","∀ V1,… . 2 prec -20"))
  lazy val th_exists = Theory(STeX.Exists.path.module.parent,STeX.Exists.path.module.name,None)
  lazy val c_exists = Constant(th_exists.toTerm,STeX.Exists.path.name,Nil,None,None,None,XHTMLTerm.notation("\\sexists{ V1 }{ 2 }","∃ V1,… . 2 prec -20"))

  def add(controller:Controller): Unit = {
    controller.add(th_set)
    controller.add(c_set)
    controller.add(th_nat)
    controller.add(PlainInclude(th_set.path,th_nat.path))
    controller.add(c_nat)
    controller.add(th_prop)
    controller.add(PlainInclude(th_set.path,th_prop.path))
    controller.add(c_prop)
    controller.add(th_fun)
    controller.add(PlainInclude(th_set.path,th_fun.path))
    controller.add(c_fun)
    controller.add(th_impl)
    controller.add(PlainInclude(th_fun.path,th_impl.path))
    controller.add(PlainInclude(th_prop.path,th_impl.path))
    controller.add(c_impl)
    controller.add(th_div)
    controller.add(PlainInclude(th_fun.path,th_div.path))
    controller.add(PlainInclude(th_prop.path,th_div.path))
    controller.add(PlainInclude(th_nat.path,th_div.path))
    controller.add(c_even)
    controller.add(th_exp)
    controller.add(PlainInclude(th_fun.path,th_exp.path))
    controller.add(PlainInclude(th_nat.path,th_exp.path))
    controller.add(c_natexp)
    controller.add(th_forall)
    controller.add(PlainInclude(th_prop.path,th_forall.path))
    controller.add(c_forall)
    controller.add(th_exists)
    controller.add(PlainInclude(th_prop.path,th_exists.path))
    controller.add(c_exists)
  }
}