package info.kwarc.mmt.frameit

import cats.effect.IO
import com.twitter.finagle.Http
import com.twitter.util.Await
import info.kwarc.mmt.api.utils.File
import io.finch._

import scala.util.parsing.json.JSON

object Server extends App with EndpointModule[IO] {

  def createEndpoints(gameLogic: FrameItLogic) =
    getAllScrolls( gameLogic) :+:
    addVector(gameLogic) :+:
    addLine(gameLogic) :+:
    addAngle(gameLogic) :+:
    addDistance(gameLogic) :+:
    addOnLine(gameLogic) :+:
    addView(gameLogic) :+:
    getPushOut(gameLogic):+:
    getDecl(gameLogic)

  override def main(args: Array[String]): Unit = {
    if(args.length < 2 ) {
      println(s"Usage: ${args.head} <PathToArchiveRoot> <Port>")
      println("PathToArchiveRoot should point to your working copy of https://github.com/UFrameIT/archives")
      return
    }

    val archiveRoot = File(args(0))
    val port = args(1)

    val gameLogic = FrameItLogic(Archives.getPaths(archiveRoot))

    Await.result( Http.server.serve(s":${port}", createEndpoints(gameLogic).toServiceAs[Text.Plain]) )
  }

  def getAllScrolls( gameLogic :FrameItLogic): Endpoint[IO, String] = get(path("scroll") :: path("list")) {
    val ret = gameLogic.getAllScrolls()
    if (ret.isDefined) Ok(ret.get) else BadRequest( new IllegalStateException () )
  }

  def addVector( gameLogic: FrameItLogic): Endpoint[IO,String] = post(path("fact") :: path ("add") :: path("vector") ::stringBody )  {
     v: String=>{
       println(v)
       JSON.parseFull(v) match {
         case Some(map: Map[String, Any]) if (map.isDefinedAt("a") && map.isDefinedAt("b") && map.isDefinedAt("c")) => {
           val vec = new Vector(map("a").asInstanceOf[Double], map("b").asInstanceOf[Double], map("c").asInstanceOf[Double])
           try{
             val ret = gameLogic.addGameElement(vec)
             if(ret.isDefined) Ok(ret.get) else BadRequest(new IllegalArgumentException())
           }catch{
             case e:Exception => BadRequest(new IllegalArgumentException())
           }
         }
         case default => {
           println("Failed to parse" )
           BadRequest(new IllegalArgumentException() )
         }
       }
     }
  }

  def addLine( gameLogic: FrameItLogic): Endpoint[IO,String] = post(path("fact") :: path ("add") :: path("line") ::stringBody )  {
    v: String=>{
      println(v)
      JSON.parseFull(v) match {
        case Some(map: Map[String, Any]) if (map.isDefinedAt("base") && map.isDefinedAt("second") ) => {
          val lin = new Line(map("base").toString, map("second").toString)
          try{
            val ret = gameLogic.addGameElement(lin)
            if(ret.isDefined) Ok(ret.get) else BadRequest(new IllegalArgumentException())
          }catch{
            case e:Exception => BadRequest(new IllegalArgumentException())
          }
        }
        case default =>  {
          println("Failed to parse" )
          BadRequest(new IllegalArgumentException() )
        }
      }
    }
  }

  def addDistance( gameLogic: FrameItLogic): Endpoint[IO,String] = post(path("fact") :: path ("add") :: path("distance") :: stringBody )  {
    d:String => {
      println(d)
      JSON.parseFull(d) match {
        case Some(map: Map[String, Any]) if (map.isDefinedAt("pointA") && map.isDefinedAt("pointB") && map.isDefinedAt("value")) => {
          val dis = new DistanceFact(map("pointA").asInstanceOf[String], map("pointB").asInstanceOf[String], map("value").toString)
          try{
            val ret = gameLogic.addGameElement(dis)
            if(ret.isDefined) Ok(ret.get) else BadRequest(new IllegalArgumentException())
          }catch {
            case e: Exception => BadRequest(new IllegalArgumentException())
          }
        }
        case default =>  {
          println("Failed to parse" )
          BadRequest(new IllegalArgumentException() )
        }
      }
    }
  }

  def addAngle( gameLogic: FrameItLogic): Endpoint[IO,String] = post(path("fact") :: path ("add") :: path("angle") :: stringBody )  {
    a:String => {
      println(a)
      JSON.parseFull(a) match {
        case Some(map: Map[String, Any]) if (map.isDefinedAt("left") && map.isDefinedAt("middle") && map.isDefinedAt("right") && map.isDefinedAt("value")) => {
          val ang = new AngleFact(map("left").asInstanceOf[String], map("middle").asInstanceOf[String], map("right").asInstanceOf[String], map("value").toString)
          try{
            val ret = gameLogic.addGameElement(ang)
            if(ret.isDefined) Ok(ret.get) else BadRequest(new IllegalArgumentException())
          }catch{
            case e:Exception => BadRequest(new IllegalArgumentException())
          }

        }
        case default =>  {
          println("Failed to parse" )
          BadRequest(new IllegalArgumentException() )
        }
      }
    }
  }

  def addOnLine(gameLogic: FrameItLogic): Endpoint[IO,String] = post(path("fact") :: path ("add") :: path("onLine") :: stringBody ) {
    lf:String => {
      println(lf)
      JSON.parseFull(lf) match {
        case Some(map:Map[String,Any]) if (map.isDefinedAt("vector") && map.isDefinedAt("line")) => {
          val fact = new OnLineFact(map("vector").toString,map("line").toString)
          try{
            val ret = gameLogic.addGameElement(fact)
            if(ret.isDefined) Ok(ret.get) else BadRequest(new IllegalArgumentException())
          }catch{
            case e:Exception => BadRequest(new IllegalArgumentException())
          }
        }
        case default =>  {
          println("Failed to parse" )
          BadRequest(new IllegalArgumentException() )
        }
      }
    }
  }

  def getHintsForPartialScroll(gameLogic: FrameItLogic): Endpoint[IO,String] = post(path("scroll") :: path("hints") :: stringBody) {data: String => {
    /**
      * Example:
      * {
      *   domainTheory: http://...?SituationTheory,
      *   scroll: {
      *     "problem": http://...?OppositeLen_Problem,
      *     "solution": // unused
      *   },
      *   assignments: {
      *     "http://...?pA": {x: ..., y: ..., z: ...}
      *     ...
      *   }
      * }
      */
    JSON.parseFull(data)
    Ok("abc")
  }}

  def addView( gameLogic: FrameItLogic) : Endpoint[IO,String] = post(path("view"):: path("add"):: stringBody) {
    v : String => {
      println(v)
      JSON.parseFull(v ) match {
        case Some(map: Map[String,Any]) if map.isDefinedAt("from") && map.isDefinedAt("to") && map.isDefinedAt("mappings") => {
          val viewCode  = new ViewCode(map("from").asInstanceOf[String], map("to").asInstanceOf[String],map("mappings").asInstanceOf[Map[String,String]])
          try{
           val ret = gameLogic.addView(viewCode)
            if(ret.isDefined) Ok(ret.get) else BadRequest(new IllegalArgumentException())
          } catch {
            case e: Exception => BadRequest(new IllegalArgumentException())
          }
        }
        case default =>  {
          println("Failed to parse" )
          BadRequest(new IllegalArgumentException() )
        }
      }
    }
  }

  def getPushOut(gameLogic: FrameItLogic): Endpoint[IO,String ] =
    get(
      path("pushout")
        ::param("problem")
        ::param("solution")
        ::param("view")
    ){
    (prob : String,sol:String,view:String ) =>{
      val ret = gameLogic.applyScroll(prob, sol,view)
      if(ret.isDefined) Ok(ret.get) else BadRequest(new IllegalArgumentException())
    }
  }

  def getDecl(gameLogic: FrameItLogic): Endpoint[IO,String] = get(path("fact")::path("get")::param("uri")) {
    (uri :String) =>{
      val ret = gameLogic.getDeclaration(uri)
      if(ret.isDefined) Ok(ret.get) else BadRequest(new IllegalArgumentException())
    }
  }



}
