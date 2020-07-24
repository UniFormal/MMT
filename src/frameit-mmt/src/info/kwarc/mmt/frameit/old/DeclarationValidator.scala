package info.kwarc.mmt.frameit.old

import info.kwarc.mmt.api.{DPath, ErrorLogger, ErrorThrower, LocalName, MMTTask}
import info.kwarc.mmt.api.checking.{CheckingEnvironment, MMTStructureChecker, RelationHandler, RuleBasedChecker, StructureChecker}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.{Theory, View}
import info.kwarc.mmt.api.objects.Term
import info.kwarc.mmt.api.symbols.{Constant, FinalConstant}
import info.kwarc.mmt.api.utils.URI

class DeclarationValidator(ctrl: Controller, situationTheory: Theory) extends MMTTask {
  lazy val checker = {
    val ret = new MMTStructureChecker(new RuleBasedChecker)
    ctrl.extman.addExtension(ret)
    ret
  }
  implicit lazy val ce : CheckingEnvironment = new CheckingEnvironment(ctrl.simplifier,ErrorThrower,RelationHandler.ignore,this)
  val scrapTheory = Theory.empty(DPath.apply(URI("http://BenniDoes.Stuff")),LocalName.parse("ScrapTheory"),Some(situationTheory.path))
  val nothing = ctrl.add(scrapTheory);
  var count = 0

  def checkTheory(t : Theory)={
    checker.apply(t)
  }
  def checkScrap( ): Boolean = {
    try{
      checkTheory(scrapTheory)
      true
    }catch{
      case e: Exception => {
        println(scrapTheory)
        e.printStackTrace()
        false
      }
    }finally {
      ctrl.delete(scrapTheory.path)
    }
  }

  def addFinalConstant(tp:Option[Term], df:Option[Term]) : FinalConstant = {
    val finalConstant = Constant.apply(scrapTheory.toTerm,LocalName.parse("test"+ count ),List(),tp,df,None)
    count = count +1
    ctrl.add(finalConstant)
    finalConstant
  }
}

class ViewValidator(ctrl: Controller) extends MMTTask {
  lazy val checker = {
    val ret = new MMTStructureChecker(new RuleBasedChecker)
    ctrl.extman.addExtension(ret)
    ret
  }
  implicit lazy val ce : CheckingEnvironment = new CheckingEnvironment(ctrl.simplifier,ErrorThrower,RelationHandler.ignore,this)

  def checkView(v :View) = {
    checker.apply(v)
  }
}
