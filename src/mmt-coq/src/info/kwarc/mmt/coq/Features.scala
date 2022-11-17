package info.kwarc.mmt.coq

import info.kwarc.mmt.api.checking.ExtendedCheckingEnvironment
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.{AbstractTheory, ModuleOrLink, Theory, View}
import info.kwarc.mmt.api.notations._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.api._
import info.kwarc.mmt.coq.coqxml.TranslationState
import info.kwarc.mmt.lf.{ApplySpine, Lambda, Pi}

import scala.collection.mutable

class Inductive extends StructuralFeature("Inductive") {
  def getHeaderNotation = List(LabelArg(1, LabelInfo.none))
  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment): Unit = ???
  override def elaborate(parent: ModuleOrLink, dd: DerivedDeclaration)(implicit env: Option[info.kwarc.mmt.api.uom.ExtendedSimplificationEnvironment] = None): Elaboration = new Elaboration {
    override def getO(name: LocalName): Option[Declaration] = dd.getO(name) match {
      case Some(c: Constant) => Some(Constant(parent.toTerm,c.name,/*LocalName(dd.name + "_C_" + dd.getPrimitiveDeclarations.indexOf(c)) ::*/ c.alias,c.tp,None,c.rl))
      case _ => None
    }

    override def domain: List[LocalName] = dd.domain
  }
}

class CoInductive extends StructuralFeature("CoInductive") {
  def getHeaderNotation = List(LabelArg(1, LabelInfo.none))
  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment): Unit = ???
  override def elaborate(parent: ModuleOrLink, dd: DerivedDeclaration)(implicit env: Option[info.kwarc.mmt.api.uom.ExtendedSimplificationEnvironment] = None): Elaboration = new Elaboration {
    override def getO(name: LocalName): Option[Declaration] = dd.getO(name) match {
      case Some(c: Constant) => Some(Constant(parent.toTerm,c.name,Nil,c.tp,None,c.rl))
      case _ => None
    }

    override def domain: List[LocalName] = dd.domain
  }
}

class Section extends StructuralFeature("Section") {
  def getHeaderNotation = List(LabelArg(1, LabelInfo.none))

  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment): Unit = ???

  override def elaborate(parent: ModuleOrLink, dd: DerivedDeclaration)(implicit env: Option[info.kwarc.mmt.api.uom.ExtendedSimplificationEnvironment] = None): Elaboration = new Elaboration {

    lazy val constants = {
      var variables: List[GlobalName] = Nil
      var cont: Context = Nil
      val consts: mutable.HashMap[GlobalName, Term] = mutable.HashMap.empty
      val replace = new StatelessTraverser {
        override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
          case OMS(p) if variables contains p => OMV(p.name)
          case OMS(p) => consts.getOrElse(p, t)
          case _ => Traverser(this, t)
        }
      }
      import info.kwarc.mmt.api.objects.Conversions._
      dd.getConstants flatMap {
        case c: Constant if c.rl contains "Variable" =>
          variables ::= c.path
          cont = cont ++ c.name % replace(c.tp.get, ())
          None
        case c =>
          val ntp = c.tp.map(t => if (cont.nonEmpty) Pi(cont, replace(t, ())) else replace(t,()))
          val ndf = c.df.map(t => if (cont.nonEmpty) Lambda(cont, replace(t, ())) else replace(t,()))
          consts(c.path) = if (cont.nonEmpty) ApplySpine(OMS(parent.modulePath ? c.name), cont.map(v => OMV(v.name)): _*)
          else OMS(parent.modulePath ? c.name)
          Some(Constant(parent.toTerm, c.name, c.alias, ntp, ndf, None))
      }
    }

    lazy val domain = constants.map(_.name)

    def getO(name: LocalName) = constants.find(_.name == name)

    /*
    val variables = names.map { p =>
      (p.name,dd.get(p.name).asInstanceOf[Constant].tp.map(Replace.apply(_,())))
    }

    private object Replace extends StatelessTraverser {
      override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
        case OMS(p) if names contains p => OMV(p.name)
        case _ => Traverser(this,t)
      }
    }

    override def getO(name: LocalName): Option[Declaration] = {
      dd.getO(name).map {
        case c : Constant =>
          val con = variables.map(p => VarDecl(p._1,None,p._2,None,None))
          val ntp = c.tp.map(Pi(con,_))
          val ndf = c.df.map(Lambda(con,_))
          Constant(parent.toTerm,c.name,Nil,ntp,ndf,c.rl)
      }
    }

    override def domain: List[LocalName] = dd.domain.filter {p =>
      dd.get(p) match {
        case c : Constant if c.rl contains "Variable" => false
        case c : Constant => true
        case _ => false
      }
    }
  }
  */
  }
}

class CoqModule extends StructuralFeature("Module") {
  def getHeaderNotation = List(LabelArg(1, LabelInfo.none),Delim("-"),SimpArg(2))

  // override val bodyDelim = "."

  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment): Unit = {}

  override def elaborate(parent: ModuleOrLink, dd: DerivedDeclaration)(implicit env: Option[info.kwarc.mmt.api.uom.ExtendedSimplificationEnvironment] = None): Elaboration = {
    val (iresult,mp) = Modules.moduleElab(parent,dd,controller)
    val result = iresult ::: {
      val inc = PlainInclude(mp,parent.modulePath)
      // inc.setOrigin(TranslatedFrom(dd, this))
      List(inc)
    }

    new Elaboration {
      override def domain: List[LocalName] = result.map(_.name)

      override def getO(name: LocalName): Option[Declaration] = result.find(_.name == name)
    }
  }

  override def processHeader(header: Term): (LocalName,Term) = {
    header match {
      case OMA(OMMOD(`mpath`), OML(oname, None, None,_,_)::arg::Nil) =>
        val tp = arg
        val name = tp match {
          case OMS(Coq.noConf) => oname
          case ApplySpine(OMS(Coq.confcolon),_) => LocalName(oname.toString + "_impl")
          case ApplySpine(OMS(Coq.confsubtp),_) => oname
        }
        (name, tp)
    }
  }

  /** inverse of processHeader */
  override def makeHeader(dd: DerivedDeclaration): Term = {
    val pname = if (dd.name.toString.endsWith("_impl")) LocalName(dd.name.toString.dropRight(5)) else dd.name
    dd.tpC.get match {
      case Some(OMA(OMMOD(`mpath`), args)) => OMA(OMMOD(mpath), OML(pname, None, None) :: args)
    }
  }

}

class CoqModuleType extends StructuralFeature("ModuleType") {
  def getHeaderNotation = List(LabelArg(1, LabelInfo.none),Delim("-"),SimpArg(2))

  // override val bodyDelim = "."

  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment): Unit = {}

  override def elaborate(parent: ModuleOrLink, dd: DerivedDeclaration)(implicit env: Option[info.kwarc.mmt.api.uom.ExtendedSimplificationEnvironment] = None): Elaboration = {
    val (result,_) = Modules.moduleElab(parent,dd,controller)

    new Elaboration {
      override def domain: List[LocalName] = result.map(_.name)

      override def getO(name: LocalName): Option[Declaration] = result.find(_.name == name)
    }
  }

  override def processHeader(header: Term): (LocalName,Term) = {
    header match {
      case OMA(OMMOD(`mpath`), OML(name, None, None,_,_)::arg::Nil) =>
        val tp = arg
        (name, tp)
    }
  }

}

object Modules {

  def moduleElab(parent: ModuleOrLink, dd: DerivedDeclaration,controller:Controller) : (List[NestedModule],MPath) = {
    val nth = dd.tp match {
      case Some(ApplySpine(OMS(Coq.confcolon),_)) =>
        // assert(dd.name.toString.endsWith("_impl"))
        val nname = if(dd.name.toString.endsWith("_impl")) LocalName(dd.name.toString.dropRight(5)) else dd.name
        val ret = Theory(parent.modulePath.parent,parent.modulePath.name / nname,Some(Coq.foundation))
        // ret.setOrigin(GeneratedBy(dd))
        ret
      //new NestedModule(parent.toTerm,nname,imod)
      case _ => dd
    }
    val viewdoms = dd.tp match {
      case Some(ApplySpine(OMS(Coq.confcolon),ls)) =>
        ls map {
          case OMMOD(mp) => controller.getO(mp) match {
            case Some(at:AbstractTheory) => at
            case _ =>
              ???
          }
        }
      case Some(ApplySpine(OMS(Coq.confsubtp),ls)) =>
        ls map {
          case OMMOD(mp) => controller.getO(mp) match {
            case Some(at:AbstractTheory) => at
            case _ =>
              ???
          }
        }
      case _ => Nil
    }

    def fill(at : AbstractTheory,in : AbstractTheory, replace : Modules.Replace): Unit = {
      at.getPrimitiveDeclarations foreach {
        case c:Constant if !in.declares(c.name) =>
          in add Constant(in.toTerm,c.name,c.alias,c.tp.map(replace.apply(_,Context.empty)),c.df.map(replace.apply(_,Context.empty)),c.rl)
        case p@PlainInclude(fr,_) if !in.declares(p.name) =>
          in add PlainInclude(replace(OMMOD(fr),Context.empty).toMPath,in.modulePath)
        case nat : DerivedDeclaration if (nat.feature=="Module" || nat.feature=="ModuleType") && !in.declares(nat.name) =>
          val nin = new DerivedDeclaration(in.toTerm,nat.name,nat.feature,nat.tpC,nat.notC)
          in add nin
          fill(nat,nin,replace)
        case nat : AbstractTheory if !in.declares(nat.name) =>
          ??? // TODO
        // fill(nat,newin,replace)
        case _ =>
      }
    }

    if (nth != dd) viewdoms foreach { at =>
      val repfrom = at.modulePath.toString
      val repto = nth.modulePath.toString
      val replace = Modules.Replace(repfrom,repto)
      fill(at,nth,replace)
    }

    val views = viewdoms.map{v =>
      val view = Modules.makeView(v,nth,parent,dd)
      val nm = new NestedModule(parent.toTerm,LocalName(view.name.last),view)
      // nm.setOrigin(GeneratedBy(dd))
      nm
    }

    val result = (if (nth != dd) {
      val nm = new NestedModule(parent.toTerm,LocalName(nth.name.last),nth.asInstanceOf[Theory])
      // nm.setOrigin(GeneratedBy(dd))
      nm :: views
    } else views)
    (result,nth.modulePath)
  }

  def makeView(dom:AbstractTheory,to:AbstractTheory,parent:ModuleOrLink,source:DerivedDeclaration):View = {
    val v = View(parent.modulePath.parent,parent.modulePath.name / LocalName(dom.name.toString + "_" + to.name + "_view"),OMMOD(dom.modulePath),OMMOD(to.modulePath),false)
    dom.getPrimitiveDeclarations foreach { d =>
     // TODO
      v add Constant(v.toTerm,d.name,Nil,None,Some(OMS(to.modulePath ? d.name)),None)
    }
    // v.setOrigin(GeneratedBy(source))
    v
  }

  def covariant(sourceURI : URI, name : LocalName, target :AbstractTheory,state : TranslationState): Unit = {
    val domain = state.controller.getO(state.toMPath(sourceURI)) match {
      case Some(at:AbstractTheory) => at
      case _ => throw CoqDependency(sourceURI)
    }
    val tp = ApplySpine(OMS(Coq.confcolon),OMMOD(domain.modulePath))
    val param = new DerivedDeclaration(target.toTerm,name,"Module",TermContainer(Some(tp)),NotationContainer.empty())
    state.controller add param

    val repfrom = domain.modulePath.toString
    val repto = param.modulePath.toString
    val replace = Replace(repfrom,repto)

    domain.getPrimitiveDeclarations foreach {
      case c:Constant =>
        state.controller add Constant(param.toTerm,c.name,c.alias,c.tp.map(replace.apply(_,Context.empty)),c.df.map(replace.apply(_,Context.empty)),c.rl)
      case PlainInclude(fr,_) =>
        state.controller add PlainInclude(fr,param.modulePath)
      case _ =>
        ???
    }
  }

  case class Replace(from : String,to : String) extends StatelessTraverser {
    override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
      case OMS(gn) =>
        OMS(Path.parseS(gn.toString.replace(from,to),NamespaceMap.empty))
      case OMMOD(mp) =>
        OMMOD(Path.parseM(mp.toString.replace(from,to),NamespaceMap.empty))
      case _ => Traverser(this,t)
    }
  }
}