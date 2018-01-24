package info.kwarc.mmt.guidedtours
import java.lang.String
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.notations._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.web._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.parser._
import info.kwarc.mmt.api.checking._
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.modules.DeclaredTheory
import objects._
import libraries._
import scala.concurrent._
import scala.collection.mutable.HashMap._
import info.kwarc.mmt.api.web._
import scala.util.parsing.json._
import scala.util._
import scala.annotation.tailrec
import scala.io._
import scala.io.Source
import java.util.Calendar


class GuidedToursServerPlugin extends ServerExtension("guided-tours") with Logger with utils.sqlite {

  def error(msg : String, request: ServerRequest) : HLet = {
    log("ERROR: " + msg)
    Server.errorResponse(msg, req)
  }

  def apply(request: ServerRequest): ServerResponse = {
    try {
      uriComps match {
        case "getTutorial" :: _ => getTutorial(request)
        case "gethtml" :: _ => getHtmlResponse(request)
        case _ => error("Invalid request: " + request.path.mkString("/"), request)
      }
    } catch {
      case e : Error =>
        log(e.shortMsg)
        ServerResponse.errorResponse(e.shortMsg, request)
      case e : Exception =>
        error("Exception occured : " + e.getStackTrace(), request)
    }
  }



  /*private def repeatEx(topic: Path, user: UserKarmaTemp, example : Example) : List[Example] = {
    val globalRating = example.getRating(topic) / 20
    val userRating = user.personalRating(example.getPath, topic)
    //val repeatPenalty = Calendar.getInstance().getTime() - example.lastUsed
    val overall = globalRating + userRating

    List.fill(overall.toInt)(example)
  }*/

  /*private def uniqueEx(allEx: List[ExampleRating], size: Int) : List[ExampleRating] = {
    @tailrec
    def helper(aE: List[ExampleRating], sz: Int, res: List[ExampleRating]) : List[ExampleRating] = {
      aE match{
        case Nil => res
        case head::tail => if(res.contains(head)) {
          helper(tail, sz, res)
        }
        else {
          if(sz == 0)
            res
          else
            helper(tail, sz - 1, head::res)
        }
      }
    }
    helper(allEx, size, Nil)
  }*/

  /*private def chooseExamples(topic: Path, user: UserKarmaTemp, examples : List[ExampleRating]) : List[ExampleRating] = {
    val allEx = Random.shuffle(examples.flatMap(x => repeatEx(topic, user, x)))

    uniqueEx(allEx, 4)
  }*/

  private def check(list1: List[Path], list2: List[Path]) : Path = {
    list1 match{
      case Nil => null
      case head :: tail =>
        if(list2.contains(head))
        {
          head
        }
        else
        {
          check(tail, list2)
        }
    }
  }

    private def getTutorial(request: ServerRequest) : ServerResponse = try {
        val topicName = request.parsedQuery("topic").getOrElse(throw ServerError("No topic name found")).toString
        val uid = request.parsedQuery("uid")getOrElse(throw ServerError("No user id found")).toString
        val length = request.parsedQuery("length").getOrElse("20").toString
        val path = Path.parseM(topicName, NamespaceMap.empty)

        val tutorial = new Tutorial(controller, path, uid.toInt)
        ServerResponse.TextResponse(tutorial.getContent(length.toInt)).aact(tk)
      }
      catch {
        case e : Error => error(e.getMessage + "\n" + e.extraMessage, request)
        case e : Exception => error(e.getMessage, request)
      }

   private def getHtmlResponse(request: ServerRequest) : ServerResponse = try {
    val topicName = request.parsedQuery("topic").getOrElse(throw ServerError("No topic name found")).toString
    //val token = tk.req.param("token").getOrElse(throw ServerError("No token was provided. Unauthorized")).toString

    //val userKarma = new UserKarmaTemp(token)
    //if(userKarma == null) {
    //  throw ServerError("No user was found")
    //}

    val path = Path.parseM(topicName, NamespaceMap.empty)
    //val sorted = controller.depstore.getInds(ontology.IsTheory).toList;
    //val sorted = tour(userKarma, getAllChildren(path, 3))

    //val response = sorted.length.toString + "\n" + sorted.mkString("(",",",")")// + "\n\n\n\n\n\n\n\n\n" + examples.mkString("(",",",")")

    println("Before everything")

    val tut = new Tutorial(controller, path, 0)

    //
    /*val sb = new presentation.StringBuilder()
    val presenter = controller.extman.getPresenter("planetary").getOrElse{
      println("defaulting to default presenter")
      controller.presenter
    }
    presenter.apply(controller.get(sorted(0)), false)(sb)
    val out = sb.get*/
    //

    //var response = ""
    println("Somehow here")

    /*val tmp = utils.Utilities.parseGraph("/home/filipbitola/Downloads/parsed_graph.txt")

    val matrix  = utils.Utilities.getMatrixFromGraph(tmp._1, tmp._2)
    println("Before paths")
    val paths = pathify(tmp._1)
    println("Paths")
    println(paths.mkString)
    val clusters = new utils.MarkovClusterer().cluster(matrix, paths)
    println(clusters.deep)

    val response = clusters.deep.mkString("(", ",", ")")*/

    ServerResponse.TextResponse(tut.getContent(20))
  } catch {
    case e : Error => error(e.getMessage + "\n" + e.extraMessage, request)
    case e : Exception => error(e.getMessage, request)
  }

   private def pathify(topics: List[String]) : List[Path] = {
     topics.map{x => path(x)}
   }

   private def path(name: String) : Path = {
     try{
      Path.parseM(name, NamespaceMap.empty)
     }
     catch {
       case e : Exception => {
         println(name)
         null
       }
     }
    }
}

