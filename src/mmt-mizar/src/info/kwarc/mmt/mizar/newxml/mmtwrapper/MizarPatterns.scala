package info.kwarc.mmt.mizar.newxml.mmtwrapper

import info.kwarc.mmt.api._
import symbols.{DerivedDeclaration, ParametricTheoryLike}
import objects._
import patterns.PatternFeature
import uom.FlexaryConstantScala
import info.kwarc.mmt.lf._
import structuralfeatures.StructuralFeatureUtils
import MizSeq.{Ellipsis, OMI, Rep, nTerms}
import info.kwarc.mmt.mizar.newxml._
import MMTUtils._
import translator.TranslationController
import info.kwarc.mmt.sequences.NatRules

object PatternUtils {
  def structureDefinitionPatternName = LocalName("structureDef")
  def structureDefPropName(decl:String) = LocalName(decl) / "prop"
  def structureDefRestrName(substrName:String) = LocalName("restr") / substrName
  def structureDefSubstrSelPropName(restrName:LocalName, sel: LocalName) = LocalName(restrName) / "selProp" / sel
  def referenceExtDecl(substrPath:GlobalName, nm: String) = OMS(StructuralFeatureUtils.externalName(substrPath,LocalName(nm)))
}

import PatternUtils._
object StructureDefinition {
  def apply(l:Int, argTps:List[Term], n:Int, substr:List[Term], m:Int, fieldDecls:List[VarDecl]): Unit = {
    val params = MMTUtils.freeVars(argTps)
    val fieldTps = fieldDecls map (_.tp.get)
    val fieldNames = fieldDecls map (_.name.toString)
    val depType = info.kwarc.mmt.lf.Arrow(Rep(Mizar.any, OMI(l)), Mizar.constant("mode"))
    def mkInd(tm:Term,str:String):Term = MizSeq.Index(tm,OMV(LocalName(str)))
    def proj(tm:Term,ind:Int):Term = MizSeq.Index(tm, OMI(ind))
    def subInd(tm:List[Term], str:String) = MizSeq.Index(MMTUtils.flatten(tm),OMV(LocalName(str)))
    def ellipses(body:Term, str:String, max:Int) = Ellipsis(OMI(max),LocalName(str),body)

    val argsTyped =ellipses(Mizar.is(mkInd(OMV(LocalName("x")),"i"), subInd(argTps,"i")),"i",l)
    def typedArgsCont(nm:Option[String]= None) : (Term => Term) = { tm: Term => Pi(LocalName("x"), nTerms(l), nm match {
      case Some(name) => Pi(LocalName(name),argsTyped, tm)
      case None => Arrow(argsTyped, tm) })
    }
    val xs = MMTUtils.flatten(params.variables.map(_.toTerm).toList)
    val ps = MMTUtils.flatten(MMTUtils.freeAlternatingVars(argTps, List("p")))
    val frs = MMTUtils.freeAlternatingVars(argTps, List("f", "r"))
    val structx = Apply(OMV("struct"), xs)
    def viAppl(i:Int):Term = ApplyGeneral(OMV(fieldDecls(i).name), List(xs, ps, OMV("s")))
    def viIsTi(i:Int):Term = Mizar.is(viAppl(i), fieldTps(i))

    val aggrTp = typedArgsCont()(fieldTps.zipWithIndex.foldRight(Mizar.any)({
      case ((tm, j), body) => MMTUtils.Lam("f"+j,fieldTps(j),
        Arrow(ellipses(Mizar.is(OMV("f"+j), OMV("t"+j)), "j", m),body))
    }))
    val aggrAppl = ApplyGeneral(OMV("aggr"), xs::ps::frs)
    val aggrPropTpClaim = Mizar.is(aggrAppl, OMV("struct"))
    def aggrApplContext(body:Term) = typedArgsCont(Some("p"))(fieldTps.zipWithIndex.foldRight(body)({
      case ((tm, j), body) => MMTUtils.Lam("f"+j,fieldTps(j),
        MMTUtils.Lam("r"+j,ellipses(Mizar.is(OMV("f"+j),OMV("t"+j)), "j", m), body))
    }))
    val aggrPropTp = aggrApplContext(aggrPropTpClaim)
    def viTp = typedArgsCont(Some("p"))(Arrow(structx,Mizar.any))
    def viPropTp(i:Int) = typedArgsCont(Some("p"))(Pi(LocalName("s"),structx,viIsTi(i)))
    def viProjProp(i:Int) = aggrApplContext(Pi(LocalName("s"),aggrAppl,Mizar.eq(viAppl(i),proj(flatten(frs),i))))
    def viPropProj = typedArgsCont(Some("p"))(Pi(LocalName("s"),structx,Mizar.eq(OMV("s"), ApplyGeneral(OMV("aggr"),OMV("x")::OMV("p")::fieldNames.zipWithIndex.flatMap { case (nm: String, ind: Int) =>
      List(viAppl(ind), ApplyGeneral(OMV(structureDefPropName(nm)), List(OMV("x"), OMV("p"), OMV("s"))))
    }))))

    def mkVD(str:String, tp:Term) = VarDecl(LocalName(str),tp)
    val struct = mkVD("struct",depType)
    val aggr = mkVD("aggr", aggrTp)
    val aggrProp = VarDecl(structureDefPropName("aggr"), aggrPropTp)
    val vs : List[VarDecl] = fieldDecls.zipWithIndex.flatMap{case (vd: VarDecl,j:Int) =>
      List(VarDecl(vd.name,viTp),VarDecl(structureDefPropName("aggr"),viPropTp(j)))
    }

    val substrRestr : List[VarDecl] = substr.zipWithIndex.flatMap{case (OMS(substrPath),i) =>
      val (substruct, substrName, sl, sargTps, sm, sfieldDefs) = TranslationController.controller.get(substrPath) match {
        case subStruct @ StructureInstance(substrName, sl, sargTps, _, _, sm, sfieldDefs) => (subStruct, substrName, sl, sargTps, sm, sfieldDefs)
      }
      val restrName = structureDefRestrName(substrName)
      val restr = VarDecl(restrName,typedArgsCont(Some("p"))(Pi(LocalName("s"),structx,referenceExtDecl(substrPath,"struct"))))
      val restrSelProps = sfieldDefs map {vd =>
        VarDecl(structureDefSubstrSelPropName(restrName,vd.name),Mizar.eq(OMV(restrName),referenceExtDecl(substrPath,restrName.toString)))
      }
      restr::restrSelProps
    }

    val vds = struct::aggr::aggrProp::vs++substrRestr
    MizPattern.apply(structureDefinitionPatternName,params,vds)
  }
}

object StructureInstance {
  /**
   * Constructs a Mizar structure instance
   * @param name
   * @param l The number of arguments to the structure instance
   * @param argTps The types of the arguments to the structure instance
   * @param n The number of structure instances it extends
   * @param substr A list of OMMOD(p), where p is the mpath of a derived declaration dd
   *               of another Mizar structure instance
   * @param m the number of field declarations (selectors in Mizar)
   * @param fieldDecls The field declarations (selectors) of the structure,
   *                   inherited selectors must be repeated here
   */
  def apply(name:String, l:Int, argTps:List[Term], n:Int, substr:List[Term], m:Int, fieldDecls:List[VarDecl]): Unit = {
    val args : List[(Option[LocalName], Term)] = argTps map (tp => (None, tp))
    val declarationPath = Mizar.MizarPatternsTh ? name
    MizarStructure.elaborateAsMizarStructure(declarationPath,args,fieldDecls,substr,TranslationController.controller)(declarationPath)
  }
  def unapply(mizPattern: DerivedDeclaration) : Option[(String, Int, List[Term],Int,List[Term],Int,List[VarDecl])] = mizPattern match {
    case pat @ MizarPatternInstance(name, patternFeaturn, args) if patternFeaturn == "StructureDefinition" =>
      def match_args(args:Seq[Term]) : (Int, List[Term], Int, List[Term], Int, List[VarDecl]) = {
        val NatRules.NatLit(l)::argss = args
        val (argTps, argsss) = argss.splitAt(l.asInstanceOf[Int])
        val NatRules.NatLit(n)::argssss = argsss
        val (substr, argsssss) = argssss.splitAt(n.asInstanceOf[Int])
        val NatRules.NatLit(m)::fieldTpss = argsssss
        val fieldDecls:List[VarDecl] = fieldTpss.sliding(2,2).toList.map {case List(v:OMV, tp:Term) => v % tp}
        (l.asInstanceOf[Int], argTps, n.asInstanceOf[Int], substr, m.asInstanceOf[Int], fieldDecls)
      }
      val (l, argTps, n, substr, m, fieldDecls) = match_args(args)
      Some((name, l,argTps,n,substr,m,fieldDecls))
    case _ => None
  }
}

object MizarPatternInstance {
  def apply(name: String, pat: String, args: List[Term]) {
    val home : Term = OMMOD(TranslationController.currentTheoryPath)
    val ln = LocalName(name)
    val pattern = Mizar.MizarPatternsTh ? LocalName(pat)
    MizInstance(home, ln, pattern, args)
  }
  def unapply(mizInstance: DerivedDeclaration): Option[(String, String, List[Term])] = mizInstance match {
    case dd : DerivedDeclaration if dd.feature == "instance"  =>
      val (patPath:MPath, args:List[Term]) = dd.tpC.get match {
        case Some(patterns.Instance.Type(p,args)) => (p, args)
      }
      val patternFeature = TranslationController.controller.get(patPath).feature
      Some(mizInstance.name.toString, patternFeature, args)
    case _ => None
  }
}

trait functorDefInstance
object directPartialFunctorDefinition extends functorDefInstance {
  def apply(name: String, argNum: Int, argTypes: List[Term], ret: Term, caseNum:Int, cases:List[Term], caseRes: List[Term], defRes:Term) = {
    assert(argTypes.length == argNum)
    assert(cases.length == caseNum && caseRes.length == caseNum)
    MizarPatternInstance(name, "directPartFuncDef", List(OMI(argNum), flatten(argTypes), ret, OMI(caseNum), flatten(cases),flatten(caseRes), defRes))
  }
}
object indirectPartialFunctorDefinition extends functorDefInstance {
  def apply(name: String, argNum: Int, argTypes: List[Term], ret: Term, caseNum:Int, cases:List[Term], caseRes: List[Term], defRes:Term) = {
    assert(argTypes.length == argNum)
    assert(cases.length == caseNum && caseRes.length == caseNum)
    MizarPatternInstance(name, "indirectPartFuncDef", List(OMI(argNum), flatten(argTypes), ret, OMI(caseNum), flatten(cases),flatten(caseRes), defRes))
  }
}
object directCompleteFunctorDefinition extends functorDefInstance {
  def apply(name: String, argNum: Int, argTypes: List[Term], ret: Term, caseNum:Int, cases:List[Term], caseRes: List[Term]) = {
    assert(argTypes.length == argNum)
    assert(cases.length == caseNum && caseRes.length == caseNum)
    MizarPatternInstance(name, "directPartFuncDef", List(OMI(argNum), flatten(argTypes), ret, OMI(caseNum), flatten(cases),flatten(caseRes)))
  }
}
object indirectCompleteFunctorDefinition extends functorDefInstance {
  def apply(name: String, argNum: Int, argTypes: List[Term], ret: Term, caseNum:Int, cases:List[Term], caseRes: List[Term]) = {
    assert(argTypes.length == argNum)
    assert(cases.length == caseNum && caseRes.length == caseNum)
    MizarPatternInstance(name, "indirectPartFuncDef", List(OMI(argNum), flatten(argTypes), ret, OMI(caseNum), flatten(cases),flatten(caseRes)))
  }
}

trait PredicateDefinitionInstance
object directPartialPredicateDef extends PredicateDefinitionInstance {
  def apply(name: String, argNum: Int, argTypes: List[Term], caseNum:Int, cases:List[Term], caseRes: List[Term], defRes:Term) = {
    assert(argTypes.length == argNum)
    assert(cases.length == caseNum && caseRes.length == caseNum)
    MizarPatternInstance(name, "directParPredDef", List(OMI(argNum), flatten(argTypes), OMI(caseNum), flatten(cases),flatten(caseRes), defRes))
  }
}
object directCompletePredicateDef extends PredicateDefinitionInstance {
  def apply(name: String, argNum: Int, argTypes: List[Term], caseNum:Int, cases:List[Term], caseRes: List[Term]) = {
    assert(argTypes.length == argNum)
    assert(cases.length == caseNum && caseRes.length == caseNum)
    MizarPatternInstance(name, "directComplPredDef", List(OMI(argNum), flatten(argTypes), OMI(caseNum), flatten(cases),flatten(caseRes)))
  }
}

trait AttributeDefinitionInstance
object indirectCompleteAttributeDefinitionInstance extends AttributeDefinitionInstance {
  def apply(name: String, argNum: Int, argTypes: List[Term], motherTp:Term, caseNum:Int, cases:List[Term], caseRes: List[Term]) = {
    assert(argTypes.length == argNum)
    assert(cases.length == caseNum && caseRes.length == caseNum)
    MizarPatternInstance(name, "indirectComplAttrDef", List(OMI(argNum), flatten(argTypes), motherTp, OMI(caseNum), flatten(cases),flatten(caseRes)))
  }
}
object indirectPartialAttributeDefinitionInstance extends AttributeDefinitionInstance {
  def apply(name: String, argNum: Int, argTypes: List[Term], motherTp:Term, caseNum:Int, cases:List[Term], caseRes: List[Term], defRes:Term) = {
    assert(argTypes.length == argNum)
    assert(cases.length == caseNum && caseRes.length == caseNum)
    MizarPatternInstance(name, "indirectPartAttrDef", List(OMI(argNum), flatten(argTypes), motherTp, OMI(caseNum), flatten(cases),flatten(caseRes), defRes))
  }
}

trait ModeDefinitionInstance
object directPartialModeDefinitionInstance extends ModeDefinitionInstance {
  def apply(name: String, argNum: Int, argTypes: List[Term], caseNum:Int, cases:List[Term], caseRes: List[Term], defRes:Term) = {
    assert(argTypes.length == argNum)
    assert(cases.length == caseNum && caseRes.length == caseNum)
    MizarPatternInstance(name, "directParModeDef", List(OMI(argNum), flatten(argTypes), OMI(caseNum), flatten(cases),flatten(caseRes), defRes))
  }
}
object directCompleteModeDefinitionInstance extends ModeDefinitionInstance {
  def apply(name: String, argNum: Int, argTypes: List[Term], caseNum:Int, cases:List[Term], caseRes: List[Term]) = {
    assert(argTypes.length == argNum)
    assert(cases.length == caseNum && caseRes.length == caseNum)
    MizarPatternInstance(name, "directComplModeDef", List(OMI(argNum), flatten(argTypes), OMI(caseNum), flatten(cases),flatten(caseRes)))
  }
}
object indirectPartialModeDefinitionInstance extends ModeDefinitionInstance {
  def apply(name: String, argNum: Int, argTypes: List[Term], caseNum:Int, cases:List[Term], caseRes: List[Term], defRes:Term) = {
    assert(argTypes.length == argNum)
    assert(cases.length == caseNum && caseRes.length == caseNum)
    MizarPatternInstance(name, "indirectParModeDef", List(OMI(argNum), flatten(argTypes), OMI(caseNum), flatten(cases),flatten(caseRes), defRes))
  }
}
object indirectCompleteModeDefinitionInstance extends ModeDefinitionInstance {
  def apply(name: String, argNum: Int, argTypes: List[Term], caseNum:Int, cases:List[Term], caseRes: List[Term]) = {
    assert(argTypes.length == argNum)
    assert(cases.length == caseNum && caseRes.length == caseNum)
    MizarPatternInstance(name, "indirectComplModeDef", List(OMI(argNum), flatten(argTypes), OMI(caseNum), flatten(cases),flatten(caseRes)))
  }
}

object schemeDefinitionInstance {
  def apply(name: String, argNum: Int, argTypes: List[Term], assNum:Int, assumptions:List[Term], p:Term) = {
    assert(argTypes.length == argNum)
    assert(assumptions.length == assNum)
    MizarPatternInstance(name, "schemeDef", List(OMI(argNum), flatten(argTypes), OMI(assNum), flatten(assumptions), p))
  }
}

trait RegistrationInstance
object attributeRegistrationInstance extends RegistrationInstance {
  def apply(name: String, argNum: Int, argTypes: List[Term], atrNum:Int, tp:Term, atrs:List[Term]) = {
    assert(argTypes.length == argNum)
    assert(atrs.length == atrNum)
    MizarPatternInstance(name, "attrRegistration", List(OMI(argNum),flatten(argTypes),OMI(atrNum),tp,flatten(atrs)))
  }
}

trait NotationInstance
class NymicNotation(key:String) extends NotationInstance {
  def apply(name: String, argNum: Int, argTypes: List[Term], v: Term): Unit = {
    assert(argTypes.length == argNum)
    MizarPatternInstance(name, key, List(OMI(argNum),flatten(argTypes),v))
  }
}
object synonymicNotation extends NymicNotation("synonymicNotation")
object antonymicNotation extends NymicNotation("antonymicNotation")