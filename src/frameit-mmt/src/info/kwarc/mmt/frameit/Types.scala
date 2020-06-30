package info.kwarc.mmt.frameit

import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.api.{ContentPath, DPath, Path}
import io.circe.{Decoder, HCursor}
import io.circe.generic.JsonCodec
import io.circe.syntax._

object Types {
  val rVector = new Vector(0.0,0.0,0.0)
  val rDistance = new DistanceFact("","","0.0")
  val rAngle = new AngleFact("","","","0.0")
}
 abstract class GameElement {
   val TypeUri : ContentPath
   def toJSONString( uri : String): String
 }

 abstract  class GameObject  extends GameElement {
   val TypeGenerator: ContentPath
 }
 abstract class Fact extends GameElement {
  val FactFuction: ContentPath
}

case class Vector (val a: Double, val b: Double, val c : Double) extends GameObject {
   override val TypeUri = DPath(URI.http colon("BenniDoes.Stuff"))? "Vector" ? "vector"
   override val TypeGenerator= DPath(URI.http colon("BenniDoes.Stuff"))? "Vector" ? "vectorOf"

  def toJSONString( uri : String):String ={
    "{" +
      "\"uri\":\"" + uri+ "\"," +
      "\"a\":\"" + a + "\"," +
      "\"b\":\"" + b + "\"," +
      "\"c\":\"" + c + "\"" +
      "}"
  }
}

case class Line (val base: String, val second : String) extends GameObject{
  override val TypeGenerator = DPath(URI.http colon("BenniDoes.Stuff"))?"Lines"?"lineOf"
  override val TypeUri = DPath(URI.http colon("BenniDoes.Stuff"))?"Lines"?"line"

  override def toJSONString( uri : String): String = {
    "{" +
      "\"uri\":\"" + uri+ "\"," +
      "\"base\":\""+ base +"\","+
      "\"dir\":\""+ second +"\""+
    "}"
  }
}

case class DistanceFact (val pointA : String, val pointB: String, val value : String) extends Fact{

  override val TypeUri = DPath( URI.http colon("BenniDoes.Stuff"))?"DistanceFact"?"distanceFact"
  override val FactFuction = DPath( URI.http colon("BenniDoes.Stuff"))?"Vector"?"metric"

  override def toJSONString(uri : String): String = {
    "{" +
      "\"uri\":\"" + uri+ "\"," +
      "\"pointA\":\"" + pointA + "\"," +
      "\"pointB\":\"" + pointB + "\"," +
      "\"value\":\"" + value + "\"" +
      "}"
  }
}

case class AngleFact ( val left: String, val middle: String, val right: String, val value: String ) extends Fact{
  override val TypeUri = DPath( URI.http colon("BenniDoes.Stuff"))?"AngleFact"?"angleFact"
  override val FactFuction = DPath( URI.http colon("BenniDoes.Stuff"))?"Vector"?"angle_between"

  override def toJSONString(uri : String): String = {
    "{" +
      "\"uri\":\"" + uri+ "\"," +
      "\"left\":\"" + left + "\"," +
      "\"middle\":\"" + middle + "\"," +
      "\"right\":\"" + right + "\"," +
      "\"value\":\"" + value + "\"" +
      "}"
  }

}

case class OnLineFact (val vector : String, val line: String) extends Fact{
  override val TypeUri = DPath( URI.http colon("BenniDoes.Stuff"))?"OnLineFact"?"onLineFact"
  override val FactFuction = DPath(URI.http colon("BenniDoes.Stuff"))?"Lines"?"onLine"


  override def toJSONString(uri : String): String = {
    "{" +
      "\"uri\":\"" + uri+ "\"," +
      "\"vector\":\"" + vector + "\"," +
      "\"line\":\"" + line + "\"," +
      "}"
  }
}


