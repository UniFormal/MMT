package info.kwarc.mmt.mizar.mizar.translator

import info.kwarc.mmt.mizar.mizar.objects._
import info.kwarc.mmt.mizar.mizar.reader._
import info.kwarc.mmt.mizar.mmt.objects._
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.patterns._
import info.kwarc.mmt.lfs._
import info.kwarc.mmt.lf._


import objects.Conversions._

object DefinitionTranslator {

   import TranslationController.makeConstant

   def makeMatches(args: List[Term], ret: Option[(String, Term)],
    cases: List[Term], results: List[Term], default: Option[Term]): Substitution = {
    val argSubs = Sub("n", OMI(args.length)) :: Sub("args", Sequence(args: _*)) :: Nil
    val retSub = ret.map(t => Sub(t._1, t._2) :: Nil).getOrElse(Nil)
    val casesSubs = Sub("m", OMI(cases.length)) :: Sub("cases", Sequence(cases: _*)) ::
      Sub("results", Sequence(results: _*)) :: Nil
    val defSub = default.map(d => Sub("default", MMTUtils.args("x", args.length, MMTUtils.argTypes("x", args, args.length, d))) :: Nil).getOrElse(Nil)

    Substitution(argSubs ++ retSub ++ casesSubs ++ defSub: _*)
  }

  def getName(prefix : String, kind : String, nr : Int) : String = {
    prefix + kind + nr
  }
  
  def makeDefTheorems(dts: List[MizDefTheorem]) = {
    dts foreach { dt =>
      val name = "D" + dt.nr
      TranslationController.addGlobalProp(dt.prop.nr, name)
      val tp = PropositionTranslator.translateProposition(dt.prop)
      val const = makeConstant(LocalName(name), tp)
      TranslationController.controller.add(const)
    }

  }

  def translatePredMeansDef(p: MizPredMeansDef) = {
    val name = p.name match {
      case None => getName(p.prefix ,"R", p.absnr)
      case Some(s) => s
    }
    println(name, p.dts.length)
    p.args.zipWithIndex.map(p => TranslationController.addLocusVarBinder(Index(OMV("x"), OMI(p._2 + 1))))
    TranslationController.addRetTerm(MMTUtils.getPath(TranslationController.currentAid, name :: "pred" :: Nil))

    val args = p.args.map(x => TypeTranslator.translateTyp(x._2))
    //val retType = TypeTranslator.translateTyp(p.retType)
    val cases = p.cases.map(x => PropositionTranslator.translateFormula(x._2)).map(x => MMTUtils.args("x", args.length, MMTUtils.argTypes("x", args, args.length, x)))
    val results = p.cases.map(x => PropositionTranslator.translateFormula(x._1)).map(x => MMTUtils.args("x", args.length, MMTUtils.argTypes("x", args, args.length, x)))
    val default = p.form.map(PropositionTranslator.translateFormula)
    val matches = makeMatches(args, None, cases, results, default)
    val pattern = default match {
      case None => DefPatterns.MizPredMeansCompleteDef
      case _ => DefPatterns.MizPredMeansPartialDef
    }

    val inst = new Instance(OMMOD(TranslationController.currentTheory),
      LocalName(name), pattern.home.toMPath ? pattern.name, matches)

    //to be removed
    //var const = new Constant(OMMOD(TranslationController.currentTheory), LocalName(name), None, None, null)
    //TranslationController.add(const)

    //println(inst.toNode)
    TranslationController.addSourceRef(inst, p)
    TranslationController.controller.add(inst)
    TranslationController.clearLocusVarContext()
    makeDefTheorems(p.dts)
  }

  def translatePredIsDef(p: MizPredIsDef) = {
    val name = p.name match {
      case None => getName(p.prefix ,"R", p.absnr)
      case Some(s) => s
    }
    println(name, p.dts.length)
    p.args.zipWithIndex.map(p => TranslationController.addLocusVarBinder(Index(OMV("x"), OMI(p._2 + 1))))

    val args = p.args.map(x => TypeTranslator.translateTyp(x._2))
    //val retType = TypeTranslator.translateTyp(p.retType)
    val cases = p.cases.map(x => PropositionTranslator.translateFormula(x._2)).map(x => MMTUtils.args("x", args.length, MMTUtils.argTypes("x", args, args.length, x)))
    val results = p.cases.map(x => TypeTranslator.translateTerm(x._1)).map(x => MMTUtils.args("x", args.length, MMTUtils.argTypes("x", args, args.length, x)))
    val default = p.term.map(TypeTranslator.translateTerm)
    val matches = makeMatches(args, None, cases, results, default)
    val pattern = default match {
      case None => DefPatterns.MizPredIsCompleteDef
      case _ => DefPatterns.MizPredIsPartialDef
    }

    val inst = new Instance(OMMOD(TranslationController.currentTheory),
      LocalName(name), pattern.home.toMPath ? pattern.name, matches)

    TranslationController.addSourceRef(inst, p)
    TranslationController.controller.add(inst)
    TranslationController.clearLocusVarContext()
    makeDefTheorems(p.dts)
  }

  def translateFuncMeansDef(f: MizFuncMeansDef) = {
    val name = f.name match {
      case None => getName(f.prefix, f.kind, f.absnr)
      case Some(s) => s
    }
    println(name, f.dts.length)
    
    f.args.zipWithIndex.map(p => TranslationController.addLocusVarBinder(Index(OMV("x"), OMI(p._2 + 1))))
    TranslationController.addRetTerm(MMTUtils.getPath(TranslationController.currentAid, name :: "func" :: Nil))
    val args = f.args.map(x => TypeTranslator.translateTyp(x._2))
    val retType = TypeTranslator.translateTyp(f.retType)
    val cases = f.cases.map(x => PropositionTranslator.translateFormula(x._2)).map(x => MMTUtils.args("x", args.length, MMTUtils.argTypes("x", args, args.length, x)))
    val results = f.cases.map(x => PropositionTranslator.translateFormula(x._1)).map(x => MMTUtils.args("x", args.length, MMTUtils.argTypes("x", args, args.length, x)))
    val default = f.form.map(PropositionTranslator.translateFormula)
    val matches = makeMatches(args, Some("retType" -> retType), cases, results, default)
    val pattern = default match {
      case None => DefPatterns.MizFuncMeansCompleteDef
      case _ => DefPatterns.MizFuncMeansPartialDef
    }

    val inst = new Instance(OMMOD(TranslationController.currentTheory),
      LocalName(name), pattern.home.toMPath ? pattern.name, matches)

    //println(inst.toNode)
    TranslationController.addSourceRef(inst, f)
    TranslationController.controller.add(inst)
    TranslationController.clearLocusVarContext()
    makeDefTheorems(f.dts)
  }

  def translateFuncIsDef(f: MizFuncIsDef) = {
    val name = f.name match {
      case None => getName(f.prefix, f.kind, f.absnr)
      case Some(s) => s
    }
    println(name, f.dts.length)
    f.args.zipWithIndex.map(p => TranslationController.addLocusVarBinder(Index(OMV("x"), OMI(p._2 + 1))))

    val args = f.args.map(x => TypeTranslator.translateTyp(x._2))
    val retType = TypeTranslator.translateTyp(f.retType)
    val cases = f.cases.map(x => PropositionTranslator.translateFormula(x._2)).map(x => MMTUtils.args("x", args.length, MMTUtils.argTypes("x", args, args.length, x)))
    val results = f.cases.map(x => TypeTranslator.translateTerm(x._1)).map(x => MMTUtils.args("x", args.length, MMTUtils.argTypes("x", args, args.length, x)))
    val default = f.term.map(TypeTranslator.translateTerm)
    val matches = makeMatches(args, Some("retType" -> retType), cases, results, default)
    val pattern = default match {
      case None => DefPatterns.MizFuncIsCompleteDef
      case _ => DefPatterns.MizFuncIsPartialDef
    }

    val inst = new Instance(OMMOD(TranslationController.currentTheory),
      LocalName(name), pattern.home.toMPath ? pattern.name, matches)

    //println(inst.toNode)
    TranslationController.addSourceRef(inst, f)
    TranslationController.controller.add(inst)
    TranslationController.clearLocusVarContext()
    makeDefTheorems(f.dts)
  }

  def translateModeMeansDef(m: MizModeMeansDef) = {
    val name = m.name match {
      case None => getName(m.prefix, "M", m.absnr)
      case Some(s) => s
    }
    println(name, m.dts.length)
    m.args.zipWithIndex.map(p => TranslationController.addLocusVarBinder(Index(OMV("x"), OMI(p._2 + 1))))
    TranslationController.addRetTerm(MMTUtils.getPath(TranslationController.currentAid, name :: "mode" :: Nil))

    val args = m.args.map(x => TypeTranslator.translateTyp(x._2))
    val cases = m.cases.map(x => PropositionTranslator.translateFormula(x._2)).map(x => MMTUtils.args("x", args.length, MMTUtils.argTypes("x", args, args.length, x)))
    val results = m.cases.map(x => PropositionTranslator.translateFormula(x._1)).map(x => MMTUtils.args("x", args.length, MMTUtils.argTypes("x", args, args.length, x)))
    val default = m.form.map(PropositionTranslator.translateFormula)
    val matches = makeMatches(args, None, cases, results, default)
    val pattern = default match {
      case None => DefPatterns.MizModeMeansCompleteDef
      case _ => DefPatterns.MizModeMeansPartialDef
    }

    val inst = new Instance(OMMOD(TranslationController.currentTheory),
      LocalName(name), pattern.home.toMPath ? pattern.name, matches)

    //println(inst.toNode)
    TranslationController.addSourceRef(inst, m)
    TranslationController.controller.add(inst)
    TranslationController.clearLocusVarContext()
    makeDefTheorems(m.dts)
  }

  def translateModeIsDef(m: MizModeIsDef) = {
    val name = m.name match {
      case None => getName(m.prefix, "M", m.absnr)
      case Some(s) => s
    }
    println(name, m.dts.length)

    m.args.zipWithIndex.foreach(p => TranslationController.addLocusVarBinder(Index(OMV("x"), OMI(p._2 + 1))))

    val args = m.args.map(x => TypeTranslator.translateTyp(x._2))
    val cases = m.cases.map(x => PropositionTranslator.translateFormula(x._2)).map(x => MMTUtils.args("x", args.length, MMTUtils.argTypes("x", args, args.length, x)))
    val results = m.cases.map(x => TypeTranslator.translateTerm(x._1)).map(x => MMTUtils.args("x", args.length, MMTUtils.argTypes("x", args, args.length, x)))
    val default = m.term.map(TypeTranslator.translateTerm)
    val matches = makeMatches(args, None, cases, results, default)
    val pattern = default match {
      case None => DefPatterns.MizModeIsCompleteDef
      case _ => DefPatterns.MizModeIsPartialDef
    }

    val inst = new Instance(OMMOD(TranslationController.currentTheory),
      LocalName(name), pattern.home.toMPath ? pattern.name, matches)

    //println(inst.toNode)
    TranslationController.addSourceRef(inst, m)
    TranslationController.controller.add(inst)
    TranslationController.clearLocusVarContext()
    makeDefTheorems(m.dts)

  }

  def translateAttrMeansDef(a: MizAttrMeansDef) = {
    val name = a.name match {
      case None => getName(a.prefix, "V", a.absnr)
      case Some(s) => s
    }
    println(name, a.dts.length)
    a.args.zipWithIndex.map(p => TranslationController.addLocusVarBinder(Index(OMV("x"), OMI(p._2 + 1))))
    TranslationController.addRetTerm(MMTUtils.getPath(TranslationController.currentAid, name :: "attr" :: Nil))

    val args = a.args.map(x => TypeTranslator.translateTyp(x._2))
    val mType = TypeTranslator.translateTyp(a.retType)
    val cases = a.cases.map(x => PropositionTranslator.translateFormula(x._2)).map(x => MMTUtils.args("x", args.length, MMTUtils.argTypes("x", args, args.length, x)))
    val results = a.cases.map(x => PropositionTranslator.translateFormula(x._1)).map(x => MMTUtils.args("x", args.length, MMTUtils.argTypes("x", args, args.length, x)))
    val default = a.form.map(PropositionTranslator.translateFormula)
    val matches = makeMatches(args, Some("mType" -> mType), cases, results, default)
    val pattern = default match {
      case None => DefPatterns.MizAttrMeansCompleteDef
      case _ => DefPatterns.MizAttrMeansPartialDef
    }

    val inst = new Instance(OMMOD(TranslationController.currentTheory),
      LocalName(name), pattern.home.toMPath ? pattern.name, matches)

    //println(inst.toNode)
    TranslationController.addSourceRef(inst, a)
    TranslationController.controller.add(inst)
    TranslationController.clearLocusVarContext()
    makeDefTheorems(a.dts)
  }

  def translateAttrIsDef(a: MizAttrIsDef) = {
    val name = a.name match {
      case None => getName(a.prefix, "V", a.absnr)
      case Some(s) => s
    }
    println(name, a.dts.length)
    
    a.args.zipWithIndex.map(p => TranslationController.addLocusVarBinder(Index(OMV("x"), OMI(p._2 + 1))))
    TranslationController.addRetTerm(MMTUtils.getPath(TranslationController.currentAid, name :: "attr" :: Nil))

    val args = a.args.map(x => TypeTranslator.translateTyp(x._2))
    val mType = TypeTranslator.translateTyp(a.retType)
    val cases = a.cases.map(x => PropositionTranslator.translateFormula(x._2)).map(x => MMTUtils.args("x", args.length, MMTUtils.argTypes("x", args, args.length, x)))
    val results = a.cases.map(x => TypeTranslator.translateTerm(x._1)).map(x => MMTUtils.args("x", args.length, MMTUtils.argTypes("x", args, args.length, x)))
    val default = a.term.map(TypeTranslator.translateTerm)
    val matches = makeMatches(args, Some("mType" -> mType), cases, results, default)
    val pattern = default match {
      case None => DefPatterns.MizAttrIsCompleteDef
      case _ => DefPatterns.MizAttrIsPartialDef
    }

    val inst = new Instance(OMMOD(TranslationController.currentTheory),
      LocalName(name), pattern.home.toMPath ? pattern.name, matches)

    //println(inst.toNode)
    TranslationController.addSourceRef(inst, a)
    TranslationController.controller.add(inst)
    TranslationController.clearLocusVarContext()
    makeDefTheorems(a.dts)
  }
  
  def translateExpMode(m : MizExpMode) = {
    val name = m.name match {
      case None => getName("", "NM", m.absnr)
      case Some(s) => s
    }
    println(name)
    m.args.zipWithIndex.map(p => TranslationController.addLocusVarBinder(Index(OMV("x"), OMI(p._2 + 1))))
    val args = m.args.map(x => TypeTranslator.translateTyp(x._2))
    val exp = TypeTranslator.translateTyp(m.exp)
    val tp = Arrow(args.map(x => Mizar.tp), Mizar.tp)
    val con = Context(VarDecl("x", Some(Sequence(args : _*)), None))
    val df = Lambda(con, exp)
    
    val const = makeConstant(name, tp, df)
    
    TranslationController.clearLocusVarContext()
  }

  private def genFieldsSub(s: List[Term]): Substitution = {
    s.length match {
      case 0 => Substitution()
      case 1 => (OMV("MS1") / s(0))
      case _ => genFieldsSub(s.tail) ++ (("MS" + s.length) / s(0))
    }
  }

  def translateStructDef(s: MizStructDef) = {
    val name = s.name match {
      case None => "L" + s.absnr
      case Some(str) => str
    }

    s.selDecls.map(x => {
      val sname = "U" + x.absnr
      TranslationController.addLocusVarBinder(OMV("mType"))
      val rt = x.retType match {
        case None => Mizar.constant("set")
        case Some(t) => TypeTranslator.translateTyp(t)
      }
      val mt = TypeTranslator.translateTyp(x.mType)
      val matches = (OMV("mType") / mt) ++ (OMV("retType") / rt)
      val pattern = DefPatterns.MizSelDef
      val i = new Instance(OMMOD(TranslationController.currentTheory), LocalName(sname), pattern.home.toMPath ? pattern.name, matches)
      TranslationController.addSourceRef(i, s)
      TranslationController.controller.add(i)
      TranslationController.clearLocusVarContext()

    })
    s.args.zipWithIndex.map(p => TranslationController.addLocusVarBinder(Index(OMV("x"), OMI(p._2 + 1))))

    val args = s.args.map(x => TypeTranslator.translateTyp(x))

    val fields = s.fields.map(x => MMTResolve(x.aid, x.kind, x.absnr)).toList

    val mstructs = s.mstructs.map(x => TypeTranslator.translateTyp(x))
    val nrFields = fields.length

    val inst = {
      val matches = (OMV("n") / OMI(args.length)) ++ (OMV("args") / Sequence(args: _*)) ++ genFieldsSub(fields.reverse)

      val pattern = DefPatterns.MizStructDef(nrFields)
      new Instance(OMMOD(TranslationController.currentTheory), LocalName(name), pattern.home.toMPath ? pattern.name, matches)
    }

    TranslationController.addSourceRef(inst, s)
    TranslationController.controller.add(inst)
    TranslationController.clearLocusVarContext()

  }

}