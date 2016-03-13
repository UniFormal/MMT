package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import utils._
import web._

import scala.collection.mutable
import QueryTypeConversion._



abstract class Alignment {
  val from : GlobalName
  val to : GlobalName

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

case class SimpleAlignment(from : GlobalName, to : GlobalName) extends Alignment {

  def altapplicable(t : Term) = false
  def translate(t : Term, cont : StatelessTraverser) = t // cannot ever occur
}

case class ArgumentAlignment(from : GlobalName, to : GlobalName, arguments: List[(Int,Int)]) extends Alignment {
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
}

case class PartialAlignment(from : GlobalName, to : GlobalName) extends Alignment {
  def altapplicable(t : Term) = false
  override def applicable(t:Term) = false
  def translate(t : Term, cont : StatelessTraverser) = t // cannot ever occur
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
    if (args.nonEmpty) {
      val file = File(args.head)
      readAlignments(file)
    }
    controller.extman.addExtension(new AlignQuery)
  }
  override def destroy {
    controller.extman.get(classOf[AlignQuery]) foreach {a =>
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
  
  private def getAlignments(from: GlobalName) = alignments.filter(_.from == from)

  private def getAlignmentsTo(from: GlobalName, in : DPath) = alignments.filter(a => a.from == from &&
    a.to.doc.toString.startsWith(in.toString))

  def translate(t : Term, to : DPath) = Translator(to)(t)

  private case class Translator(target : DPath) extends StatelessTraverser {

    def apply(t:Term) : Term = apply(t,Context.empty)

    implicit val cont = Some(this)

    def traverse(t: Term)(implicit con: Context, init: Unit): Term = t match {
        // TODO this completely ignores all but the first alignment that matches
      case s@OMS(p) => getAlignmentsTo(p,target).find(_.applicable(s)).map(_.apply(s)).getOrElse(???)
      case s@OMA(f@OMS(fun),args) => getAlignmentsTo(fun,target).find(_.applicable(s)).map(_.apply(s)).getOrElse(Traverser(this,t))
      case _ => Traverser(this,t)
    }
  }


  private def readAlignments(file: File) {
    val json = JSON.parse(File.read(file))
    json match {
      case obj: JSONObject => obj.map foreach {
         case (jsonstring, alignmentList:JSONArray) =>
            alignmentList.values foreach {
               case alignmentObject: JSONObject =>
                 val alignmentMap = alignmentObject.toList.toMap
                 val from = Path.parseS(alignmentMap(JSONString("from")).toString, nsMap)
                 val to = Path.parseS(alignmentMap(JSONString("to")).toString, nsMap)
                 alignments += SimpleAlignment(from, to)
            }
       case _ =>
      }
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
      controller.extman.get(classOf[AlignmentsServer])
      translate(o, dpath)
    }
  }

}


