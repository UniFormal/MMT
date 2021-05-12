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
import info.kwarc.mmt.stex.xhtml.{OMDocHTML}

object MitM { /*
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
  */
}

object DemoContent { /*
  import scala.xml._
  lazy val th_set = Theory(STeX.set.doc,STeX.set.module.name,Some(STeX.meta))
  lazy val c_set = {
    val c = Constant(th_set.toTerm,STeX.set.name,Nil,None,None,None)
    PreElement.addMacroName(c,"set")
    PreElement.addNotations(c,("",{<mi data-mmt-symref={c.path.toString}>Set</mi>}))
    c
  }
  lazy val th_nat = Theory(STeX.core / "arithmetics" / "natural_numbers",LocalName("NaturalNumbers"),Some(STeX.meta))
  lazy val c_nat = {
    val c = Constant(th_nat.toTerm,LocalName("NaturalNumbers"),Nil,Some(c_set.toTerm),Some(OMS(STeX.nat)),None)
    PreElement.addMacroName(c,"NaturalNumbers")
    PreElement.addNotations(c,("",{<mi data-mmt-symref={c.path.toString}>ℕ</mi>}))
    c
  }
  lazy val th_fun = Theory(STeX.funtype.doc,STeX.funtype.module.name,Some(STeX.meta))
  lazy val c_fun = {
    val c = Constant(th_fun.toTerm,STeX.funtype.name,Nil,None,None,None) // TODO prec -9990
    PreElement.addMacroName(c,"funtype")
    c.metadata.update(STeX.meta_arity,STeX.StringLiterals("ai"))
    PreElement.addNotations(c,("",{<mrow><mrow><mtext>##1a</mtext><mo data-mmt-symref={c.path.toString}>×</mo><mtext>##1b</mtext></mrow><mo data-mmt-symref={c.path.toString}>⟶</mo><mrow><mtext>#2</mtext></mrow></mrow>}))
    c
  }
  lazy val th_impl = Theory(DPath(URI.http colon "mathhub.info") / "smglom" / "logic",LocalName("Implication"),Some(STeX.meta))
  lazy val c_impl = {
    val c = Constant(th_impl.toTerm,LocalName("Implication"),Nil,Some(OMA(c_fun.toTerm,List(c_set.toTerm,c_set.toTerm,c_set.toTerm))),None,None) // TODO prec 10
    PreElement.addMacroName(c,"implication")
    c.metadata.update(STeX.meta_arity,STeX.StringLiterals("ii"))
    PreElement.addNotations(c,("",{<mrow><mtext>#1</mtext><mo data-mmt-symref={c.path.toString}>⟹</mo><mtext>#2</mtext></mrow>}))
    c
  }
  lazy val th_div = Theory(DPath(URI.http colon "mathhub.info") / "smglom" / "arithmetics" / "natural_numbers",LocalName("Divisibility"),Some(STeX.meta))
  lazy val c_even = {
    val c = Constant(th_div.toTerm,LocalName("even"),Nil,Some(OMA(c_fun.toTerm,List(c_nat.toTerm,OMS(STeX.prop)))),None,None) // TODO prec 50
    PreElement.addMacroName(c,"even")
    c.metadata.update(STeX.meta_arity,STeX.StringLiterals("i"))
    PreElement.addNotations(c,("",{<mrow><mo data-mmt-symref={c.path.toString}>even</mo><mo stretchy="true" data-mmt-symref={c.path.toString}>(</mo><mtext>#1</mtext><mo stretchy="true" data-mmt-symref={c.path.toString}>)</mo></mrow>}))
    c
  }
  lazy val th_exp = Theory(DPath(URI.http colon "mathhub.info") / "smglom" / "arithmetics" / "natural_numbers",LocalName("Exponentiation"),Some(STeX.meta))
  lazy val c_natexp = {
    val c = Constant(th_exp.toTerm,LocalName("natexp"),Nil,Some(OMA(c_fun.toTerm,List(c_nat.toTerm,c_nat.toTerm))),None,None) // TODO prec 70
    PreElement.addMacroName(c,"natpow")
    c.metadata.update(STeX.meta_arity,STeX.StringLiterals("ii"))
    PreElement.addNotations(c,("",{<mrow><msup><mtext>#1</mtext><mtext>#2</mtext></msup></mrow>}))
    c
  }

  lazy val th_forall = Theory(STeX.Forall.path.module.parent,STeX.Forall.path.module.name,Some(STeX.meta))
  lazy val c_forall = {
    val c = Constant(th_forall.toTerm,STeX.Forall.path.name,Nil,None,None,None) // TODO prec -20
    PreElement.addMacroName(c,"sforall")
    c.metadata.update(STeX.meta_arity,STeX.StringLiterals("bi"))
    PreElement.addNotations(c,("",{<mrow><mo data-mmt-symref={c.path.toString}>∀</mo><mtext>#1</mtext><mo data-mmt-symref={c.path.toString}>.</mo><mtext>#2</mtext></mrow>}))
    c
  }
  lazy val th_exists = Theory(STeX.Exists.path.module.parent,STeX.Exists.path.module.name,Some(STeX.meta))
  lazy val c_exists = {
    val c = Constant(th_exists.toTerm,STeX.Exists.path.name,Nil,None,None,None) // TODO prec -20
    PreElement.addMacroName(c,"sexists")
    c.metadata.update(STeX.meta_arity,STeX.StringLiterals("bi"))
    PreElement.addNotations(c,("",{<mrow><mo data-mmt-symref={c.path.toString}>∃</mo><mtext>#1</mtext><mo data-mmt-symref={c.path.toString}>.</mo><mtext>#2</mtext></mrow>}))
    c
  }

  def add(controller:Controller): Unit = try {
    controller.add(th_set)
    controller.add(c_set)
    controller.add(th_nat)
    controller.add(PlainInclude(th_set.path,th_nat.path))
    controller.add(c_nat)
    controller.add(th_fun)
    controller.add(PlainInclude(th_set.path,th_fun.path))
    controller.add(c_fun)
    controller.add(th_impl)
    controller.add(PlainInclude(th_fun.path,th_impl.path))
    controller.add(c_impl)
    controller.add(th_div)
    controller.add(PlainInclude(th_fun.path,th_div.path))
    controller.add(PlainInclude(th_nat.path,th_div.path))
    controller.add(c_even)
    controller.add(th_exp)
    controller.add(PlainInclude(th_fun.path,th_exp.path))
    controller.add(PlainInclude(th_nat.path,th_exp.path))
    controller.add(c_natexp)
    controller.add(th_forall)
    controller.add(c_forall)
    controller.add(th_exists)
    controller.add(c_exists)
  } catch {
    case t : Throwable =>
      print("")
  } */
}