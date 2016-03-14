package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import web._

import scala.collection.mutable
import QueryTypeConversion._
import info.kwarc.mmt.api.utils._


abstract class Alignment {
  val from : GlobalName
  val link : String

  def toJSON : (JSONString,JSONObject)
}

abstract class FormalAlignment extends Alignment {
  val to : GlobalName
  val link = to.toString

  def applicable(t:Term) : Boolean = t match {
    case OMS(f) if f==from => true
    case _ => altapplicable(t)
  }
  protected def altapplicable(t : Term) : Boolean

  def apply(t : Term)(implicit cont : Option[StatelessTraverser] = None) : Term = {
    assert(applicable(t))
    t match {
      case OMS(f) if f==from => OMS(to)
      case _ if cont.isDefined => translate(t,cont.get)
      case _ => ???
    }
  }
  protected def translate(t: Term,cont : StatelessTraverser) : Term
}

case class SimpleAlignment(from : GlobalName, to : GlobalName) extends FormalAlignment {

  def altapplicable(t : Term) = false
  def translate(t : Term, cont : StatelessTraverser) = t // cannot ever occur

  def toJSON = (JSONString("Simple"),JSONObject(List(
    (JSONString("from"),JSONString(from.toString)),
    (JSONString("to"),JSONString(to.toString))
  )))
}

case class ArgumentAlignment(from : GlobalName, to : GlobalName, arguments: List[(Int,Int)]) extends FormalAlignment {
  def altapplicable(t : Term) = t match {
    case OMA(OMS(f),args) if f == from => true
    case _ => false
  }
  private def reorder(args:List[Term]) = {
    // TODO what if the last arguments are implicit?
    val max = arguments.sortBy(_._2).last._2
    (1 to max).map(i => args(arguments.find(p => p._2==i).map(_._1).getOrElse(???))).toList // TODO insert variables
  }
  def translate(t:Term, cont : StatelessTraverser) = t match {
    case OMA(OMS(f),args) if f == from =>
      OMA(OMS(to),reorder(args).map(cont.apply(_,Context.empty))) // TODO insert variables
  }

  def toJSON = (JSONString("Argument"),JSONObject(List(
    (JSONString("from"),JSONString(from.toString)),
    (JSONString("to"),JSONString(to.toString)),
    (JSONString("args"),JSONArray(arguments.map(p => JSONArray.fromList(List(JSONInt(p._1),JSONInt(p._2))))))
  )))
}

case class PartialAlignment(from : GlobalName, to : GlobalName) extends FormalAlignment {
  def altapplicable(t : Term) = false
  override def applicable(t:Term) = false
  def translate(t : Term, cont : StatelessTraverser) = t // cannot ever occur

  def toJSON = (JSONString("Partial"),JSONObject(List(
    (JSONString("from"),JSONString(from.toString)),
    (JSONString("to"),JSONString(to.toString))
  )))
}

case class InformalAlignment(from : GlobalName, link : String) extends Alignment {
  def toJSON = (JSONString("Informal"),JSONObject(List(
    (JSONString("from"),JSONString(from.toString)),
    (JSONString("to"),JSONString(link))
  )))
}
/*
case class Alignment(kind: String, from: GlobalName, to: GlobalName, args: Option[List[(Int,Int)]]) {
  override def toString = s"$kind $from $to"
}
*/

class AlignmentsServer extends ServerExtension("align") {

  override val logPrefix = "align"
  
  private val alignments = mutable.HashSet[Alignment]()
  
  override def start(args:List[String]) {
    args.foreach(a => {
      val file = File(a)
      readAlignments(file)
    })
    controller.extman.addExtension(new AlignQuery)
    controller.extman.addExtension(new CanTranslateQuery)
  }
  override def destroy {
    controller.extman.get(classOf[AlignQuery]) foreach {a =>
      a.destroy
      controller.extman.removeExtension(a)
    }
    controller.extman.get(classOf[CanTranslateQuery]) foreach {a =>
      a.destroy
      controller.extman.removeExtension(a)
    }
  }

  private val nsMap = NamespaceMap.empty
  
  def apply(uriComps: List[String], query: String, body: Body) = {
      val from = Path.parseS(query, nsMap)
      val toS = getAlignments(from)
      Server.TextResponse(toS.mkString("\n"))
  }
  
  def getAlignments(from: GlobalName) = alignments.filter(_.from == from)


  def getFormalAlignments(from: GlobalName) = alignments.collect{
    case a:FormalAlignment if a.from == from => a
  }

  def getAlignmentsTo(from: GlobalName, in : DPath) = alignments.filter(a => a.from == from &&
    a.link.toString.startsWith(in.toString))

  def getFormalAlignmentsTo(from: GlobalName, in : DPath) = alignments.collect{
    case a:FormalAlignment if a.from == from && a.to.doc.toString.startsWith(in.toString) => a
  }

  def translate(t : Term, to : DPath) = try {Some(Translator(to)(t))} catch {
    case CanNotTranslate => None
    case e : Exception => throw e
  }

  def CanTranslateTo(t : Term) : List[DPath] = {
    val head = t.head.getOrElse(return Nil) match {
      case n:GlobalName => n
      case _ => return Nil
    }
    getFormalAlignments(head).map(_.to.doc).toList
  }

  private object CanNotTranslate extends Exception

  private case class Translator(target : DPath) extends StatelessTraverser {

    def apply(t:Term) : Term = apply(t,Context.empty)

    implicit val cont = Some(this)

    def traverse(t: Term)(implicit con: Context, init: Unit): Term = t match {
        // TODO this completely ignores all but the first alignment that matches
      case s@OMS(p) => getFormalAlignmentsTo(p,target).find(_.applicable(s)).map(_.apply(s)).getOrElse(throw CanNotTranslate)
      case s@OMA(f@OMS(fun),args) => getFormalAlignmentsTo(fun,target).find(_.applicable(s)).map(_.apply(s)).getOrElse(Traverser(this,t))
      case _ => Traverser(this,t)
    }
  }


  private def readAlignments(file: File) {
    val json = JSON.parse(File.read(file))
    json match {
      case obj: JSONObject => obj.map foreach {
         case (JSONString("View"), alignmentList:JSONArray) =>
            alignmentList.values foreach {
               case alignmentObject: JSONObject =>            
                 val Some(fromJSON) = alignmentObject("from");
                 val Some(toJSON) =   alignmentObject("to");
     
                 val JSONString(fromString_) = fromJSON
                 val JSONString(toString_) = toJSON
                 val fromString = fromString_.replaceAll("//","/")
                 val toString = toString_.replaceAll("//","/")
                 println("FROM:" + fromString)
                 println("TO:" + toString)
                 val from = Path.parseS(fromString, nsMap)
                 val to = Path.parseS(toString, nsMap)
                 alignments += SimpleAlignment(from, to)
            }
         case (JSONString("Informal"),o:JSONObject) =>
          alignments += InformalAlignment(
            Path.parseS(o("from") match {
              case Some(JSONString(s)) => s
              case _ => ???
            },nsMap),
            o("to") match {
              case Some(JSONString(s)) => s
              case _ => ???
            })
         case (JSONString("Simple"),o:JSONObject) =>
           alignments += SimpleAlignment(
             Path.parseS(o("from") match {
               case Some(JSONString(s)) => s
               case _ => ???
             },nsMap),
             Path.parseS(o("to") match {
               case Some(JSONString(s)) => s
               case _ => ???
             },nsMap)
           )
         case (JSONString("Partial"),o:JSONObject) =>
           alignments += PartialAlignment(
             Path.parseS(o("from") match {
               case Some(JSONString(s)) => s
               case _ => ???
             },nsMap),
             Path.parseS(o("to") match {
               case Some(JSONString(s)) => s
               case _ => ???
             },nsMap)
           )
         case _ =>
      }
      case _ =>
    }
    
    alignments foreach println
  }
  
  /** translation along alignments */
  private class AlignQuery extends QueryExtension("align", ObjType, ObjType) {
    def evaluate(argument: BaseType, params: List[String]) = {
      val dpath = params match {
        case p::Nil => Path.parseD(p, NamespaceMap.empty)
        case Nil => throw ParseError("parameter expected")
        case _ => throw ParseError("exactly one parameter expected")
      }
      val o = argument match {
        case t : Term => t
        case o: Obj => ??? // TODO
        case _ => throw ImplementationError("evaluation of ill-typed query")
      }
      // controller.extman.get(classOf[AlignmentsServer])
      translate(o, dpath).toList
    }
  }

  private class CanTranslateQuery extends QueryExtension("cantranslate",ObjType,PathType) {
    def evaluate(argument : BaseType, params : List[String]) = {
      val o = argument match {
        case t : Term => t
        case o: Obj => ??? // TODO
        case _ => throw ImplementationError("evaluation of ill-typed query")
      }
      CanTranslateTo(o)
    }
  }

}


