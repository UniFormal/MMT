package info.kwarc.mmt.frameit


import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.objects.{OMID, OMS, Term}
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.{ContentPath, LocalName, Path}
import info.kwarc.mmt.frameit.mitm.Foundation.{RealLiterals, StringLiterals}
import info.kwarc.mmt.lf.ApplySpine

class FactToMMT(ctrl: Controller)   {

  var factcounter= 1;



  def addVector(v: Vector, situationTheory: Theory ) : Option[String] = {
    val validator = new DeclarationValidator(ctrl, situationTheory)
    val a = v.a.toString
    val b = v.b.toString
    val c = v.c.toString
    val tp = OMS(v.TypeUri)
    val vectorOf = v.TypeGenerator
    val df = ApplySpine( OMS (vectorOf), RealLiterals.parse(a),RealLiterals.parse(b),RealLiterals.parse(c))
    val tmp_const = validator.addFinalConstant(Some(tp),Some(df))
    val ok = validator.checkScrap()
    if (ok ){
      val finalConstant = Constant(situationTheory.toTerm,LocalName.parse("fact_"+factcounter),List(),Some[Term](tp),Some(df),None)
      factcounter = factcounter + 1
      ctrl.add(finalConstant)
      Some ("{ \"factUri\":\""+ finalConstant.path.toString() +"\""+
        "}" )
    }else{
      None
    }

  }

  def addLine(l : Line ,situationTheory: Theory ): Option[String] = {
    val validator = new DeclarationValidator(ctrl,situationTheory)
    val baseOMID = OMID(Path.parse(l.base).asInstanceOf[ContentPath])
    val secondOMID  = OMID(Path.parse(l.second).asInstanceOf[ContentPath])
    val tp = OMS(l.TypeUri)
    val lineOf = OMS( l.TypeGenerator)
    val df = ApplySpine( lineOf, baseOMID,secondOMID)
    val tmp_const = validator.addFinalConstant(Some(tp),Some(df))
    val ok = validator.checkScrap()
    if (ok ){
      val finalConstant = Constant(situationTheory.toTerm,LocalName.parse("fact_"+factcounter),List(),Some[Term](tp),Some(df),None)
      factcounter = factcounter + 1
      ctrl.add(finalConstant)
      Some ("{ \"factUri\":\""+ finalConstant.path.toString() +"\""+
        "}" )
    }else{
      None
    }
  }

  def addDistance(d: DistanceFact,situationTheory: Theory): Option[String] = {
    val validator =  new DeclarationValidator(ctrl, situationTheory)
    val aOMID = OMID(Path.parse(d.pointA).asInstanceOf[ContentPath])
    val bOMID = OMID(Path.parse(d.pointB).asInstanceOf[ContentPath])
    val vOMLIT = RealLiterals.parse( d.value.toString())

    // for factval: |R | = 1.0
    val tp = RealLiterals.synType
    val df = vOMLIT
    val tmpConst1 = validator.addFinalConstant(Some(tp),Some(df))

    // for jder d a b = c
    val aTerm = ctrl.get(aOMID.path).asInstanceOf[Constant].toTerm
    val bTerm = ctrl.get(bOMID.path).asInstanceOf[Constant].toTerm
    val tp2_tmp = ApplySpine(OMID(d.TypeUri.asInstanceOf[ContentPath]), aTerm,bTerm,tmpConst1.toTerm)
    val sketchtype_tmp = ApplySpine(OMID(Archives.MetaSymbols.jdoteq),RealLiterals.synType, ApplySpine( OMID(d.FactFuction.asInstanceOf[ContentPath]), aTerm, bTerm),tmpConst1.toTerm )
    val df2_tmp = ApplySpine(OMID(Archives.MetaSymbols.proofSketch), sketchtype_tmp ,StringLiterals.parse("measuredDistance " + d.pointA + ", " + d.pointB ))

    val tmpconst2 = validator.addFinalConstant(Some(tp2_tmp),Some(df2_tmp))

    if( validator.checkScrap() ){
      val finalConstant = Constant(situationTheory.toTerm, LocalName.parse("fact_"+factcounter),List(),Some(tp), Some(df),None)
      factcounter = factcounter +1
      ctrl.add(finalConstant)
      val sketchtype = ApplySpine(OMID(Archives.MetaSymbols.jdoteq), RealLiterals.synType ,ApplySpine( OMID(d.FactFuction.asInstanceOf[ContentPath]), aTerm, bTerm),finalConstant.toTerm )
      val tp2 = ApplySpine(OMID(d.TypeUri.asInstanceOf[ContentPath]), aTerm,bTerm,finalConstant.toTerm)
      val df2 = ApplySpine(OMID(Archives.MetaSymbols.proofSketch), sketchtype ,StringLiterals.parse("measuredDistance " + d.pointA + ", " + d.pointB ))
      val finalConstant2 = Constant(situationTheory.toTerm,LocalName.parse("fact_"+factcounter),List(),Some(tp2),Some(df2),None)
      factcounter = factcounter +1
      ctrl.add(finalConstant2)
      Some ("{ \"factUri\":\""+ finalConstant2.path.toString() +"\","+
        "\"factValUri\":\""+ finalConstant.path.toString() +"\"" +
        "}" )
    }else{
      None
    }
  }

  def addAngle(a :AngleFact, situationTheory: Theory): Option[String] = {
    val validator = new DeclarationValidator(ctrl, situationTheory)
    val aOMID = OMID(Path.parse(a.left).asInstanceOf[ContentPath])
    val bOMID = OMID(Path.parse(a.middle).asInstanceOf[ContentPath])
    val cOMID = OMID(Path.parse(a.right).asInstanceOf[ContentPath])
    val vOMLIT = RealLiterals.parse( a.value.toString())
    val angleType = a.TypeUri

    //for val: |R | = 1.0
    val tp = RealLiterals.synType
    val df = vOMLIT
    val tmpConst1 = validator.addFinalConstant(Some(tp),Some(df))

    val aTerm = ctrl.get(aOMID.path).asInstanceOf[Constant].toTerm
    val bTerm = ctrl.get(bOMID.path).asInstanceOf[Constant].toTerm
    val cTerm = ctrl.get(cOMID.path).asInstanceOf[Constant].toTerm
    val tp2_tmp = ApplySpine(OMID(a.TypeUri.asInstanceOf[ContentPath]), aTerm,bTerm,cTerm, tmpConst1.toTerm)
    val sketchtype_tmp = ApplySpine(OMID(Archives.MetaSymbols.jdoteq),RealLiterals.synType,ApplySpine(OMID( a.FactFuction.asInstanceOf[ContentPath]),aTerm, bTerm,cTerm ),tmpConst1.toTerm)
    val df2_tmp = ApplySpine(OMID(Archives.MetaSymbols.proofSketch),sketchtype_tmp,StringLiterals.parse("measuredAngle " + a.left + ", " + a.middle + ", "+ a.right + ""))

    val tmpConst2 = validator.addFinalConstant(Some(tp2_tmp),Some(df2_tmp))

    val ok = validator.checkScrap()
    if(ok ){
      val finalConstant = Constant(situationTheory.toTerm, LocalName.parse("fact_"+factcounter),List(),Some(tp), Some(df),None)
      factcounter = factcounter +1
      ctrl.add(finalConstant)
      val tp2 = ApplySpine(OMID(a.TypeUri.asInstanceOf[ContentPath]), aTerm,bTerm,cTerm, finalConstant.toTerm)
      val sketchtype= ApplySpine(OMID(Archives.MetaSymbols.jdoteq),RealLiterals.synType,ApplySpine(OMID( a.FactFuction.asInstanceOf[ContentPath]),aTerm, bTerm,cTerm ),finalConstant.toTerm)
      val df2 =ApplySpine(OMID(Archives.MetaSymbols.proofSketch),sketchtype,StringLiterals.parse("measuredAngle " + a.left + ", " + a.middle + ", "+ a.right + ""))
      val finalConstant2 = Constant(situationTheory.toTerm,LocalName.parse("fact_"+factcounter),List(),Some(tp2),Some(df2),None)
      factcounter = factcounter +1
      ctrl.add(finalConstant2)
      Some ("{ \"factUri\":\""+ finalConstant2.path.toString() +"\","+
        "\"factValUri\":\""+ finalConstant.path.toString() +"\"" +
        "}" )
    }else{
      None
    }
  }

  def addOnLineFact(lf: OnLineFact, situationTheory: Theory): Option[String]={
    val validator = new DeclarationValidator(ctrl,situationTheory)
    val vecOMID =  OMID(Path.parse(lf.vector).asInstanceOf[ContentPath])
    val lineOMID =  OMID(Path.parse(lf.line).asInstanceOf[ContentPath])
    val vecTerm = ctrl.get(vecOMID.path).asInstanceOf[Constant].toTerm
    val lineTerm = ctrl.get(lineOMID.path).asInstanceOf[Constant].toTerm

    val tp = ApplySpine(OMID(lf.TypeUri.asInstanceOf[ContentPath]), vecTerm, lineTerm)
    val df = ApplySpine(OMID(Archives.MetaSymbols.proofSketch),ApplySpine(OMID( lf.FactFuction.asInstanceOf[ContentPath]),lineTerm,vecTerm),StringLiterals.parse("measured that "+ lf.vector +" is on "+ lf.line))
    val constant_tmp = validator.addFinalConstant(Some(tp),Some(df))
    val ok = validator.checkScrap()
    if(ok ){
      val finalConstant = Constant(situationTheory.toTerm, LocalName.parse("fact_"+factcounter),List(),Some(tp), Some(df),None)
      factcounter = factcounter +1
      ctrl.add(finalConstant)
      Some("{\"factUri\":\""+ finalConstant.path.toString() +"\"}")
    }else{
      None
    }

  }
}
