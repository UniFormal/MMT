package info.kwarc.mmt.frameit

import info.kwarc.mmt.api.{DPath, LocalName, MPath, Path}
import info.kwarc.mmt.api.frontend.{ConsoleHandler, Controller}
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.ontology.IsTheory
import info.kwarc.mmt.api.symbols.FinalConstant
import info.kwarc.mmt.api.utils.{File, FilePath, URI}
object FrameItLogic{
  def apply(files: List[File]): FrameItLogic={
    val ctrl = new Controller()
    ctrl.report.addHandler(ConsoleHandler)
    files.foreach(ctrl.addArchive)
    val frameitArchive = {
      ctrl.handleLine(s"build ${Archives.frameworldIdentifier} scala-bin")
      ctrl.handleLine(s"build ${Archives.frameworldIdentifier} mmt-omdoc")
      
      ctrl.backend.getArchive(Archives.frameworldIdentifier).getOrElse {
        throw info.kwarc.mmt.api.GetError(s"Archive ${Archives.frameworldIdentifier} could not be found!")
      }
    }
    frameitArchive.allContent

    // force-read relational data as e.g. [[ScrolltoJSON]] uses the depstore
    // to get meta tags on things
    frameitArchive.readRelational(FilePath("/"), ctrl, "rel")

    val situationTheory = Theory.empty(
      DPath(frameitArchive.narrationBase),
      LocalName("SituationTheory"),
      Some(Archives.Frameworld.FactCollection)
    )
    ctrl.add(situationTheory)

    new FrameItLogic(ctrl,situationTheory)
  }
}

class FrameItLogic (val ctrl : Controller, var situationTheory: Theory){
  val scrollTranslator = new ScrolltoJSON(ctrl)
  val factToMMT = new FactToMMT(ctrl)
  val viewToMMT = new ViewToMMT(ctrl)
  val pushoutHandler = new PushoutHandler(ctrl)

  def getAllScrolls():Option[String] ={
    val theoriespaths = ctrl.depstore.getInds(IsTheory)
    val theos = theoriespaths.map( t => ctrl.getTheory(t.asInstanceOf[MPath]))
    val scrolls = theos.filter( t => scrollTranslator.isScroll(t))
    Some (scrollTranslator.convertToJson(scrolls))
  }

  def addGameElement(element : GameElement):Option[String] = {
    val ret = element match {
      case v: Vector => factToMMT.addVector(v, situationTheory)
      case l : Line => factToMMT.addLine(l,situationTheory)
      case d: DistanceFact => factToMMT.addDistance(d, situationTheory)
      case a: AngleFact => factToMMT.addAngle(a, situationTheory)
      case lf : OnLineFact => factToMMT.addOnLineFact(lf, situationTheory)
      case default => None
    }
    println(situationTheory)
    ret
  }

  def addView( viewCode: ViewCode): Option[String] = {
    viewToMMT.addView(viewCode)
  }

  def applyScroll(prob : String,sol:String,view:String ) : Option[String] = {
    try {
      val outpush = pushoutHandler.generatePushout(prob,sol,situationTheory.path.toString(),view)
      val ret = pushoutHandler.readPushout(outpush)
      situationTheory = outpush
      ctrl.add(situationTheory)
      val out = "{\"newSituation\":\""+ situationTheory.path.toString() +"\","+ ret + "}"
      Some(out)
    } catch {
      case e :Exception => None
      case default => None
    }


  }

  def getDeclaration(uri:String): Option[String] = {
    try {
       val  decl = ctrl.get(Path.parse((uri))).asInstanceOf[FinalConstant]
       val ret = "{"+
        "\"name\":\""+ decl.name + "\"," +
        "\"type\":\""+ decl.tp.get.toStr(true) + "\"," +
        "\"definition\":\""+ decl.df.get.toStr(true) + "\"" +
      "}"
      Some(ret)
    }catch{
      case e: Exception => None
    }
  }


}
