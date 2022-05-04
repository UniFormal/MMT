package info.kwarc.mmt.intellij.checking

import scala.language.reflectiveCalls
import info.kwarc.mmt.api
import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.parser.{ParsingStream, SourceRegion}
import info.kwarc.mmt.api.symbols.Declaration
import info.kwarc.mmt.api.utils._

class Checker(controller: Controller,ev : ErrorViewer) {
  def check(uri : String, text: String,
            clearFileP : Any,
            noteP : Any,
            errorContP : Any): Unit = {
    val clearFile = clearFileP.asInstanceOf[{
      def apply(v1 : String) : Unit
    }]
    val note = noteP.asInstanceOf[{
      def apply(v1 : String, v2 : String) : Unit
    }]
    val errorCont = errorContP.asInstanceOf[{
    def apply(v1 : Int,v2 : Int,v3 : String,v4 : String,v5 : List[String]) : Unit
    }]
    val nsMap = controller.getNamespaceMap
    val file = FileURI.unapply(URI(uri)).getOrElse(return)
    val ps = ParsingStream.fromString(text,DPath(URI(uri)),file.getExtension.getOrElse(""),Some(nsMap))
    val progress = new Progresser(file,(s1,s2) => note(s1,s2))
    ps.addListener(progress)
    clearFile(file.toString)
    val error = new ErrorForwarder(file,(p,s1,s2,ls) => errorCont(p._1,p._2,s1,s2,ls))
    // background {
    val doc = controller.read(ps, true, true)(error) match {
      case d: Document =>
        progress.done(d)
      case _ => //throw ImplementationError("document expected")
    }
  }

  class Progresser(file : File, note : (String,String) => Unit) extends MMTTaskProgressListener {
    override def apply(p: MMTTaskProgress): Unit = p match {
      case Parsed(s) =>
        s match {
          case c : Declaration =>
            note("Checking " + c.path.module.name + "?" + c.name,file.toString)
          case _ =>
        }
      case _ =>
    }
    def done(d : Document) = {
      note("Done: " + d.path.toString,file.toString)
      ev.finish(file,d)
    }
  }

  class ErrorForwarder(file: File,error : ((Int,Int),String,String,List[String]) => Unit) extends ErrorHandler {

    override protected def addError(e: api.Error): Unit = {
      val (reg,main,extra,isWarning) = processError(e)

      error((reg.start.offset,reg.length),file.toString,if (isWarning) "Warning: " + main else main,extra)
    }

    def processError(e: api.Error): (SourceRegion, String, List[String], Boolean) = {
      import info.kwarc.mmt.api._
      import archives.source
      import objects._
      import parser._

      e match {
        case s: SourceError =>
          (s.ref.region, s.mainMessage, s.extraMessages, s.level == Level.Warning || e.excuse.isDefined)
        case e: Invalid =>
          var mainMessage = e.shortMsg
          var extraMessages: List[String] = e.extraMessage.split("\n").toList
          val causeOpt: Option[metadata.HasMetaData] = e match {
            case e: InvalidObject => Some(e.obj)
            case e: InvalidElement => Some(e.elem)
            case e: InvalidUnit =>
              val steps = e.history.getSteps
              extraMessages :::= steps.map(_.present(o => controller.presenter.asString(o)))
              val declOpt = e.unit.component.map(p => controller.localLookup.get(p.parent))
              // WFJudgement must exist because we always start with it
              // find first WFJudgement whose region is within the failed checking unit
              declOpt.flatMap { decl =>
                SourceRef.get(decl).flatMap { bigRef =>
                  steps.mapFind { s =>
                    s.removeWrappers match {
                      case j: WFJudgement =>
                        SourceRef.get(j.wfo) flatMap { smallRef =>
                          if (bigRef contains smallRef) {
                            mainMessage += ": " + controller.presenter.asString(j.wfo)
                            Some(j.wfo)
                          } else
                            None
                        }
                      case _ =>
                        None
                    }
                  }
                }.orElse(declOpt)
              }
          }
          val ref = causeOpt.flatMap { cause => SourceRef.get(cause) }.getOrElse {
            mainMessage = "error with unknown location: " + mainMessage
            SourceRef(utils.FileURI(file), SourceRegion(SourcePosition(0, 0, 0), SourcePosition(0, 0, 0)))
          }
          (ref.region, mainMessage, extraMessages.filter(_.trim != ""),e.level == Level.Warning || e.excuse.isDefined)
        case e: Error =>
          (SourceRegion.none, "error with unknown location: " + e.getMessage, e.extraMessage.split("\n").toList,e.level==Level.Warning || e.excuse.isDefined)
      }
    }
  }
}