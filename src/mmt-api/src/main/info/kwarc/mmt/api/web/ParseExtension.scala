package info.kwarc.mmt.api.web

import info.kwarc.mmt.api._
import utils._
import parser._
import ServerResponse._

class ParseServer extends ServerExtension(":parse") {
    private lazy val presenter = controller.extman.get(classOf[presentation.HTMLPresenter]).headOption.getOrElse{throw LocalError("no html presenter found")}
    def apply(request: ServerRequest): ServerResponse = {
      val wq = request.parsedQuery
      val text = wq.string("text", throw LocalError("found no text to parse"))
      val save = wq.boolean("save", false) //if save parameter is "true" then save otherwise don't
      val format = wq.string("format", "elf")
      val theoryS = wq.string("theory", throw LocalError("found no theory"))
      stringToList(theoryS,"\\?") match {
        case strDPath :: strThy :: Nil =>
          val dpath = DPath(URI(strDPath))
          val mpath = dpath ? strThy
          val ctrl = new frontend.Controller(controller.report)
          val parser = controller.extman.get(classOf[Parser], format).getOrElse {
             throw LocalError("no parser found")
          }
          val errorCont = new ErrorContainer
          val ps = ParsingStream.fromString(text, dpath, format)
          val doc = parser(ps)(new StructureParserContinuations(errorCont))
          errorCont.getErrors match {
            case Nil => //no error -> parsing successful
                val rb = new presentation.StringBuilder()
                val mod = ctrl.memory.content.getModule(mpath)
                val module = if(save) {
                  controller.get(mpath)
                } else {
                  mod
                }
                presenter(module)(rb)
                val thyString = rb.get
                var response: List[(String,JSON)] = Nil
                response ::= "success" -> JSONBoolean(true)
                val sdiff = controller.detectChanges(List(mod))
                save match {
                  case false => //just detecting refinements
                    val refs = controller.detectRefinements(sdiff)
                    response ::= "info" -> JSONArray(refs.map(JSONString(_)):_*)
                    response ::= "pres" -> JSONString(thyString)
                  case true => //updating and returning list of done updates
                    val pchanges = stringToList(wq.string("pchanges"), "\n")
                    val boxedPaths = controller.update(sdiff, pchanges)

                    def invPaths(p : Path, parents : Set[Path] = Nil.toSet) : Set[Path] = {
                       println("calling for path " + p + " with parents " + parents.mkString(", "))
                       p match {
                         case d : DPath =>
                           controller.getDocument(d).getDeclarations.flatMap {
                              case r: documents.NRef => invPaths(r.target, parents + d)
                              case doc: documents.Document => invPaths(doc.path, parents + d)
                              case _ => Nil
                           }.toSet
                         case m : MPath =>
                           val affected = boxedPaths exists {cp => cp.parent match {
                             case gn : GlobalName => gn.module == m
                             case mp : MPath => mp == p
                             case _ => false
                           }}
                           if (affected)
                             parents + p
                           else
                             Nil.toSet
                         case _ => Nil.toSet
                       }
                    }
                    response ::= "pres" -> JSONArray(invPaths(controller.getBase).toList.map(x => JSONString(x.toString)) :_*)
                }
                JsonResponse(JSONObject(response :_*))
            case l => //parsing failed -> returning errors
              var response: List[(String, JSON)] = Nil
              response ::= "success" -> JSONBoolean(false)
              response ::= "info" -> JSONArray()
              response ::= "pres" -> JSONString(l.map(e => (<p>{e.getStackTrace().toString}</p>).toString).mkString(""))
              JsonResponse(JSONObject(response :_*))
          }
        case _ => throw LocalError(s"invalid theory name in query : {$request.query}")
      }
    }
}
