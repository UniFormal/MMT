package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import web._

import scala.collection.mutable
import QueryTypeConversion._
import info.kwarc.mmt.api.utils._

import scala.util.Try

sealed abstract class Reference

case class LogicalReference(mmturi : ContentPath) extends Reference {
  override def toString = mmturi.toPath
}
case class PhysicalReference(url : URI) extends Reference {
  override def toString = url.toString
}

abstract class Alignment {
  val from : Reference
  val to : Reference

  def ->(that : Alignment) : Alignment

  val props : List[(String,String)]

  def toJSON : (JSONString,JSONObject)

  def reverse : Alignment
}

abstract class FormalAlignment extends Alignment {
  val from : LogicalReference
  val to : LogicalReference
  val invertible : Boolean

  def toTerm = to match {
    case LogicalReference(t:GlobalName) => OMS(t)
    case LogicalReference(t: MPath) => OMMOD(t)
  }

  def applicable(t:Term) : Boolean = t match {
    case OMS(f) if f==from.mmturi => true
    case _ => altapplicable(t)
  }
  protected def altapplicable(t : Term) : Boolean

  def apply(t : Term)(implicit cont : Option[StatelessTraverser] = None) : Term = {
    assert(applicable(t))
    t match {
      case OMS(f) if f==from.mmturi => toTerm
      case _ if cont.isDefined => translate(t,cont.get)
      case _ => ???
    }
  }
  protected def translate(t: Term,cont : StatelessTraverser) : Term
}

case class SimpleAlignment(from : LogicalReference, to : LogicalReference, invertible : Boolean, props : List[(String,String)]) extends FormalAlignment {

  def altapplicable(t : Term) = false
  def translate(t : Term, cont : StatelessTraverser) = t // cannot ever occur

  def toJSON = (JSONString("Simple"),JSONObject(List(
    (JSONString("from"),JSONString(from.toString)),
    (JSONString("to"),JSONString(to.toString))
  )))

  def reverse = if (invertible) SimpleAlignment(to,from,true,Nil) else InformalAlignment(to,from)

  def ->(that:Alignment) = that match {
    case SimpleAlignment(a,b,inv,props2) => SimpleAlignment(from,b,inv && invertible,Nil)
    case ArgumentAlignment(a,b,inv,args,props2) => ArgumentAlignment(from,b,inv && invertible,args,Nil)
    case InformalAlignment(a,b,props2) => InformalAlignment(from,b,Nil)
  }
}

case class ArgumentAlignment(from : LogicalReference, to : LogicalReference, invertible : Boolean, arguments: List[(Int,Int)], props : List[(String,String)]) extends FormalAlignment {

  def altapplicable(t : Term) = t match {
    case OMA(OMS(f),args) if f == from.mmturi => true
    case _ => false
  }
  private def reorder(args:List[Term]) = {
    // TODO what if the last arguments are implicit?
    val max = arguments.sortBy(_._2).last._2
    (1 to max).map(i => args(arguments.find(p => p._2==i).map(_._1).getOrElse(???))).toList // TODO insert variables
  }
  def translate(t:Term, cont : StatelessTraverser) = t match {
    case OMA(OMS(f),args) if f == from.mmturi =>
      OMA(toTerm,reorder(args).map(cont.apply(_,Context.empty))) // TODO insert variables
  }

  def toJSON = (JSONString("Argument"),JSONObject(List(
    (JSONString("from"),JSONString(from.toString)),
    (JSONString("to"),JSONString(to.toString)),
    (JSONString("args"),JSONArray(arguments.map(p => JSONArray.fromList(List(JSONInt(p._1),JSONInt(p._2))))))
  )))

  def reverse = if (invertible) ArgumentAlignment(to,from,true,arguments.map(p => (p._2,p._1)),props) else
    InformalAlignment(to,from)

  def ->(that:Alignment) = that match {
    case SimpleAlignment(a,b,inv,props2) => ArgumentAlignment(from,b,inv&&invertible,arguments,Nil)
    case ArgumentAlignment(a,b,inv,args2,props2) => ArgumentAlignment(from,b,inv&&invertible,{
      arguments.map(p => {
        val other = args2.find(q => p._2 == q._1).getOrElse(throw new Exception("Can not compose " + this + " with " + that))
        (p._1,other._2)
      })
      },Nil)
    case InformalAlignment(a,b,props2) => InformalAlignment(from,b)
  }
}

case class InformalAlignment(from : Reference, to : Reference, props : List[(String,String)] = Nil) extends Alignment {
  def toJSON = (JSONString("Informal"),JSONObject(List(
    (JSONString("from"),JSONString(from.toString)),
    (JSONString("to"),JSONString(to.toString))
  )))

  def reverse = InformalAlignment(to,from)

  def ->(that:Alignment) = that match {
    case f: FormalAlignment => InformalAlignment(from,f.to)
    case f: InformalAlignment => InformalAlignment(from,f.to)
  }
}

object SimpleAlignment {
  def apply(from : ContentPath, to : ContentPath, invertible : Boolean, props : List[(String,String)] = Nil) : SimpleAlignment
    = SimpleAlignment(LogicalReference(from),LogicalReference(to),invertible,props)
}
object ArgumentAlignment {
  def apply(from : ContentPath, to : ContentPath, invertible : Boolean, args: List[(Int,Int)], props : List[(String,String)] = Nil) : ArgumentAlignment
  = ArgumentAlignment(LogicalReference(from),LogicalReference(to),invertible,args,props)
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
      if (file.getExtension match {case Some("json") => true case _ => false})
        readJSON(file)
      else readFile(file)
    })
    controller.extman.addExtension(new AlignQuery)
    controller.extman.addExtension(new CanTranslateQuery)
    /*
    alignments += SimpleAlignment(
      Path.parseS("http://pvs.csl.sri.com/Prelude?list_props?append",nsMap),
      Path.parseS("http://code.google.com/p/hol-light/source/browse/trunk?lists?APPEND",nsMap)
    )
    alignments += SimpleAlignment(
      Path.parseS("http://code.google.com/p/hol-light/source/browse/trunk?lists?APPEND",nsMap),
      Path.parseS("http://pvs.csl.sri.com/Prelude?list_props?append",nsMap)
    )
    alignments += InformalAlignment(
      Path.parseS("http://pvs.csl.sri.com/Prelude?list_props?append",nsMap),
      URI("""https://en.wikipedia.org/wiki/List_(abstract_data_type)#Operations""")
    )
    alignments += InformalAlignment(
      Path.parseS("http://code.google.com/p/hol-light/source/browse/trunk?lists?APPEND",nsMap),
      URI("""https://en.wikipedia.org/wiki/List_(abstract_data_type)#Operations""")
    )
    alignments += SimpleAlignment(
      Path.parseS("http://latin.omdoc.org/foundations/hollight?Kernel?bool",nsMap),
      Path.parseS("http://pvs.csl.sri.com/?PVS?boolean",nsMap)
    )
    alignments += InformalAlignment(
      Path.parseS("http://latin.omdoc.org/foundations/hollight?Kernel?bool",nsMap),
      URI("https://en.wikipedia.org/wiki/Boolean_data_type")
    )
    alignments += InformalAlignment(
      Path.parseS("http://pvs.csl.sri.com/?PVS?boolean",nsMap),
      URI("https://en.wikipedia.org/wiki/Boolean_data_type")
    )
    */
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

  private var nsMap = NamespaceMap.empty
  
  def apply(path: List[String], query: String, body: Body) = {
    path match {
      case List("from") =>
        val path = Path.parseS(query,nsMap)
        val toS = getAlignments(LogicalReference(path)).map(_.to.toString)
        //println("Alignment query: " + query)
        //println("Alignments from " + path + ":\n" + toS.map(" - " + _).mkString("\n"))
        Server.TextResponse(toS.mkString("\n"))
      case _ =>
        println(path) // List(from)
        println(query) // an actual symbol path
        println(body) //whatever
        Server.TextResponse("")
    }
  }

  // val dones : mutable.HashMap[ContentPath,List[Alignment]] = mutable.HashMap.empty
  
  def getAlignments(from: Reference) : List[Alignment] = {

    var res : List[Reference] = Nil
    def recurse(c : Reference) : List[Alignment] = {
      //if (res.contains(c)) return Nil
      res ::= c
      val ret = (alignments.filter(a => a.from == c).toList :::
        alignments.collect{
          case a if a.to == c => a.reverse
        }.toList).filter(a => !res.contains(a.to))
      ret ::: ret.flatMap(a => recurse(a.to).map(a -> _)) //a => recurse(a.from).map(a -> _))
    }
      recurse(from)

  }

  def getAlignments(from: ContentPath) : List[Alignment] = getAlignments(LogicalReference(from))

  def getFormalAlignments(from: ContentPath) = getAlignments(LogicalReference(from)).collect{
    case a:FormalAlignment => a
  }

  def getAlignmentsTo(from: ContentPath, in : DPath) = getAlignments(LogicalReference(from)).filter(a =>
    a.to.toString.startsWith(in.toString))

  def getFormalAlignmentsTo(from: ContentPath, in : DPath) = getFormalAlignments(from).filter(a =>
    a.to.toString.startsWith(in.toString))

  def translate(t : Term, to : DPath) = try {Some(Translator(to)(t))} catch {
    case CanNotTranslate => None
    case e : Exception => throw e
  }

  def CanTranslateTo(t : Term) : List[DPath] = {
    val head = t.head.getOrElse(return Nil) match {
      case n:GlobalName => n
      case _ => return Nil
    }
    getFormalAlignments(head).map(_.to.mmturi.doc).toList
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

  private def readFile(file : File) {
    val param = """(.+)\s*=\s*\"(.+)\"\s*(.*)""".r
    val argls = """\((\d+),(\d+)\)(.*)""".r
    val cmds = File.read(file).split("\n").map(_.trim).filter(_.nonEmpty)
    cmds foreach (s => {
      var rest = s
      if (rest.startsWith("namespace")) {
        val (abbr,path) = {
          val s = rest.substring(10).split("""\s""").map(_.trim).filter(_.nonEmpty)
          (s.head,s(1))
        }
        nsMap = nsMap.add(abbr,URI(path))
        //println("added namespace: " + abbr +" -> " + path)
        //println(nsMap)
      } else if (s.nonEmpty && !s.startsWith("//")) {
        val (p1,p2) = {
          val s = rest.split("""\s""").map(_.trim).filter(_.nonEmpty)
          //println(s.head)
          //println(s(1))
          rest = rest.substring(s.head.length).trim.substring(s(1).length).trim
          (s.head,s(1))
        }
        var pars : List[(String,String)] = Nil
        while (rest != "") rest match {
          case param(key,value,r) =>
            pars ::= (key,value)
            rest = r.trim
          case _ => throw new Exception("Malformed alignment: " + s)
        }
        val direction = pars.find(p => p._1=="direction")
        if (direction.isDefined) {
          if (pars.exists(_._1 == "arguments")) {
            var args : List[(Int,Int)] = Nil
            val item = pars.find(_._1 == "arguments").get
            var read = item._2.trim
            while (read != "") read match {
              case argls(i,j,r) =>
                args ::= (i.toInt,j.toInt)
                rest = r.trim
              case _ => throw new Exception("Malformed alignment: " + s)
            }
            val ret = if (direction.get._2 == "forward")
              ArgumentAlignment(Path.parseMS(p1,nsMap),Path.parseMS(p2,nsMap),false,args,pars)
            else if (direction.get._1 == "backward")
              ArgumentAlignment(Path.parseMS(p2,nsMap),Path.parseMS(p1,nsMap),false,args,pars)
            else
              ArgumentAlignment(Path.parseMS(p1,nsMap),Path.parseMS(p2,nsMap),true,args,pars)
            alignments += ret
          } else {
            val ret = if (direction.get._2 == "forward")
              SimpleAlignment(Path.parseMS(p1,nsMap),Path.parseMS(p2,nsMap),false,pars)
            else if (direction.get._1 == "backward")
              SimpleAlignment(Path.parseMS(p2,nsMap),Path.parseMS(p1,nsMap),false,pars)
            else if (direction.get._2 == "both")
              SimpleAlignment(Path.parseMS(p1,nsMap),Path.parseMS(p2,nsMap),true,pars)
            else throw new Exception("unknown alignment direction: " + direction.get._2)
            alignments += ret
          }
        } else {
          val from : Reference = Try(LogicalReference(Path.parseMS(p1,nsMap))).getOrElse(PhysicalReference(URI(p1)))
          val to : Reference = Try(LogicalReference(Path.parseMS(p2,nsMap))).getOrElse(PhysicalReference(URI(p2)))
          alignments += InformalAlignment(from,to,pars)
        }
      }
    })
    // alignments foreach println
  }
  // TODO needs reworking
  private def readJSON(file: File) {
    /*
    val json = JSON.parse(File.read(file))
    json match {
      case obj: JSONObject => obj.map foreach {
         case (JSONString("View"), alignmentList:JSONArray) =>
            alignmentList.values foreach {
               case o: JSONObject =>
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
            }
         case (JSONString("Informal"),o:JSONObject) =>
          alignments += InformalAlignment(
            Path.parseS(o("from") match {
              case Some(JSONString(s)) => s
              case _ => ???
            },nsMap),
            o("to") match {
              case Some(JSONString(s)) => URI(s)
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
    */
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


