package info.kwarc.mmt.api.parser

import info.kwarc.mmt.api._
import NotationConversions._
import info.kwarc.mmt.api.utils.MyList._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects._

/** Objects of type Marker make up the pattern of a Notation */
sealed abstract class Marker {
  def toNode : scala.xml.Node
  override def toString : String
}

/** Markers that are delimiters */
sealed abstract class Delimiter extends Marker {
   val s: String
}

/** a delimiter
 * @param s the delimiting String
 */
case class Delim(s: String) extends Delimiter {
   override def toString = s
   def toNode = <delim>{s}</delim>
}
/** a delimiter
 * @param s the delimiting String
 */
case class SecDelim(s: String, wsAllowed: Boolean = true) extends Delimiter {
   override def toString = (if (wsAllowed) "" else "_") + s
   def toNode = <sec-delim wsAllowed={wsAllowed.toString}>{s}</sec-delim>
}

/** an argument
 * @param n absolute value is the argument position, negative iff it is in the binding scope
 */
case class Arg(n: Int) extends Marker {
   override def toString = n.toString
   def by(s:String) = SeqArg(n,Delim(s))
   def toNode = <arg>{n}</arg>
}

/** a sequence argument 
 * @param n absolute value is the argument position, negative iff it is in the binding scope
 * @param sep the delimiter between elements of the sequence 
 */
case class SeqArg(n: Int, sep: Delim) extends Marker {
   override def toString = n.toString + sep + "..."
   def toNode = <seq-arg>{n}{sep.toNode}</seq-arg>
}

/** a variable binding 
 * @param n the number of the variable
 * @param key the delimiter between the variable name and its type 
 */
case class Var(n: Int, key: Delim) extends Marker {
   override def toString = n.toString + key + "_"
   def toNode = <var>{n}{key.toNode}</var>
}

/** a sequence variable binding 
 * @param n the number of the sequence variable
 * @param key the delimiter between the variable name and its type
 * @param sep the delimiter between two consecutive variable bindings 
 */
//TODO: currently not parsed
case class SeqVar(n: Int, key: Delim, sep: Delim) extends Marker {
   override def toString = Var(n,key).toString + sep + "..."
   def toNode = <seq-var>{n}{key.toNode}{sep.toNode}</seq-var>
}

/**
 * helper object 
 */
object Arg {
   private def splitAux(ns: List[Int], ms: List[Marker]) : (List[Int], List[Marker]) = ms match {
      case Arg(n) :: rest => splitAux(n :: ns, rest)
      case rest => (ns.reverse, rest)
   }
   /** splits a List[Marker] into
    *  a List[Int] (possibly Nil) that corresponds to a List[Arg]
    * and the remaining List[Marker]
    */
   def split(ms: List[Marker]) = splitAux(Nil,ms)
}

/** defines some implicit conversions to produce Markers */
object NotationConversions {
   /** integers are converted to argument markers */
   implicit def fromInt(n:Int) = Arg(n)
   /** strings are converted to delimiters */
   implicit def fromString(s:String) = Delim(s)
}

class TextNotation(val name: GlobalName, val markers: List[Marker], val priority: Int) extends presentation.Notation {
  val wrap = false 
  val oPrec = Some(presentation.Precedence.integer(priority))

  //TODO add other cases & check presentation
  lazy val pres = {  
    val tokens = markers map {
      case Delim(s) => presentation.Fragment("constant", presentation.Text(name.toPath), presentation.Text(s))
      case Arg(p) => presentation.Component(presentation.NumberedIndex(p + 1),oPrec.map(_.weaken))
      case SeqArg(p,sep) => presentation.Iterate(presentation.NumberedIndex(1),
    		  									 presentation.NumberedIndex(-1),
    		  									 presentation.OpSep() + presentation.Fragment("constant", presentation.Text(name.toPath), presentation.Text(sep.s)) + presentation.OpSep(),
    		  									 oPrec.map(_.weaken))
    }
    /*
    if (isBinder) {
      presentation.PList(tokens) + presentation.Component(presentation.NumberedIndex(-1),None)
    } else {
      presentation.PList(tokens)
    }
    */
    presentation.PList(tokens)
  }
  val key = presentation.NotationKey(Some(name), Role_Notation)
  val nset = name.module.toMPath
  
  
   override def toString = "Notation for " + name + ": " + markers.map(_.toString).mkString(" ")
   // the first delimiter of this notation
   private val firstDelimString : Option[String] = markers mapFind {
      case d: Delimiter => Some(d.s)
      case SeqArg(_, Delim(s)) => Some(s)
      case _ => None
   }
   /** @return firstDelimToken == next */
   def applicable(next: Token): Boolean = {
      firstDelimString == Some(next.word)
   }
   /** creates a new ActiveNotation with this notation's markers */
   def open(scanner: Scanner, firstToken: Int): ActiveNotation = {
      val an = new ActiveNotation(scanner, this, firstToken)
      an
   }
   
   def toNode = 
     <text-notation>
	   <name>{name}</name>
   	   <markers>{markers.map(_.toNode)}</markers>
       <priority>{priority}</priority>
     </text-notation>
}

object TextNotation {
  
   def apply(name: GlobalName, priority: Int)(ms: Any*): TextNotation = {
      val markers : List[Marker] = ms.toList map {
         case i: Int => Arg(i)
         case "" => throw ParseError("not a valid marker")
         case s: String if s.endsWith("...") =>
            var i = 0
            while (s(i).isDigit) {i+=1}
            val n = s.substring(0,i).toInt
            val rem = s.substring(i,s.length-3)
            val p = rem.indexOf("_")
            if (p == -1)
               SeqArg(n, Delim(rem))
            else {
               val key = rem.substring(0,p)
               val sep = rem.substring(p+1)
               SeqVar(n, Delim(key), Delim(sep))
            }
         case s: String if s.endsWith("_") =>
            var i = 0
            while (s(i).isDigit) {i+=1}
            val n = s.substring(0,i).toInt
            val d = s.substring(i,s.length-1)
            Var(n, Delim(d))
         case s:String => Delim(s)
         case m: Marker => m
         case m => throw ParseError("not a valid marker" + m)
      }
      new TextNotation(name, markers, priority)
   }
   
   /** XML parsing methods */
   def parse(n : scala.xml.Node, name : GlobalName) : TextNotation = n match {
    case <text-notation>{xmlName}{xmlMarkers}{xmlPriority}</text-notation> =>
      val name = parseName(xmlName)
      val markers = parseMarkers(xmlMarkers)
      val priority = parsePriority(xmlPriority)
      new TextNotation(name, markers, priority)
    case _ => throw ParseError("Invalid XML representation of notation in \n" + n)
  }
  
  private def parseName(n : scala.xml.Node) : GlobalName = n match {
    case <name>{value}</name> => Path.parseS(value.toString, utils.mmt.mmtbase)
    case _ => throw ParseError("Invalid XML representation of Notation name in: \n" + n)
  }  
    
   
  private def parsePriority(n : scala.xml.Node) : Int = n match {
    case <priority>{value}</priority> =>
      try {
        value.toString.toInt
      } catch {
        case _ => throw ParseError("Invalid XML representation of notation priority (not an integer) in: \n" + n)
      }
    case _ => throw ParseError("Invalid XML representation of notation priority in: \n" + n)
  }

  private def parseMarkers(n : scala.xml.Node) : List[Marker] = n match {
    case <markers>{xmlMarkers @ _*}</markers> =>
      xmlMarkers.map(x => parseMarker(x)).toList
    case _ => throw ParseError("Invalid XML representation of marker list in: \n" + n)
  }
  
  private def parseMarker(n : scala.xml.Node) : Marker = n match {
    case <delim>{value @ _*}</delim> => Delim(value.text)
    case <sec-delim>{value @ _*}</sec-delim> => SecDelim(value.text, (n \ "@wsAllowed").text == "true")
    case <arg>{value}</arg> =>
      try {
        Arg(value.toString.toInt)
      } catch {
        case _ => throw ParseError("Invalid XML representation of Arg (not an integer) in: \n" + n)
      }
    case <seq-arg>{value}{sep}</seq-arg> => 
       try {
        val nr = value.toString.toInt
        val dlm = parseMarker(sep) match {
          case d : Delim => d
          case _ => throw ParseError("Invalid XML representation of Delim in SeqArg : \n" + n)
        }
        SeqArg(nr, dlm)
      } catch {
        case e => throw ParseError("Invalid XML representation of SeqArg in: \n" + n + " with error : \n" + e.getMessage)
      }
      
    case <var>{value}{key}</var> => 
      try {
        val nr = value.toString.toInt
        val keyM = parseMarker(key) match {
          case d : Delim => d
          case _ => throw ParseError("Invalid XML representation of Delim in Var : \n" + n)
        }
        Var(nr, keyM)
      } catch {
        case _ => throw ParseError("Invalid XML representation of Var (not an integer) in: \n" + n)
      }
    case <seq-var>{value}{key}{sep}</seq-var> => 
      try {
        val nr = value.toString.toInt
        val keyM = parseMarker(key) match {
          case d : Delim => d
          case _ => throw ParseError("Invalid XML representation of key Delim in SeqVar : \n" + n)
        }
        val sepM = parseMarker(sep) match {
          case d : Delim => d
          case _ => throw ParseError("Invalid XML representation of sep Delim in SeqVar : \n" + n)
        }
        Var(nr, keyM)
      } catch {
        case _ => throw ParseError("Invalid XML representation of SeqVar (not an integer) in: \n" + n)
      }
    case _ => throw ParseError("Invalid XML representation of Notation Marker in: \n" + n)
      
    
  }
  
  /** String parsing methods */
   def parse(str : String, conPath : GlobalName) : TextNotation = {
    str.split(",").toList match {
      case not :: prec :: Nil =>        
        val priority = prec.toInt
        parseNot(not, priority, conPath)
      case not :: Nil => 
        parseNot(not, 0, conPath)
      case _ =>
        throw ParseError("Invalid notation declaration : " + str)
    }
  }
   
  private def parseNot(str : String, priority : Int, conPath : GlobalName) : TextNotation = {
    val protoMks = str.split(" ") map {s =>
      try {
        s.toInt
      } catch {
        case e => s
      }
    }
    apply(conPath, priority)(protoMks :_*)
  }
   
  /*
  private def parseMarkers(not : String) : List[Marker] = {
    val tokens = not.split(" ").filter(_ != "").toList
    val markers = tokens map {tk =>
      tk.split("/").toList match {
        case Nil => throw ImplementationError("unexpected error: string split returned empty result list")
        case value :: Nil => //std arg or delimiter
          try {
            Arg(value.toInt - 1)
          } catch {
            case _ => Delim(value)
          }
        case pos :: sep :: Nil => //seq arg
          try {
            SeqArg(pos.toInt - 1, Delim(sep))
          } catch {
             case _ => throw ParseError("Invalid arg position in seq notation : " + not)
          }
      }
    }
    markers
  }
  */
   
  /** Presenting methods*/
    //TODO add logging instead of print
  def present(con : Content, operators : List[TextNotation]) : String = {
    println("current con : " + con.toString)
    con match {
      case d : DeclaredTheory =>
        val namespace = "%namespace \"" +  d.path.parent  + "\"." 
        println(d.path)
        val sig = "%sig " + d.path.last + " = {\n" + d.innerComponents.map(c => "  " + present(c, operators)).filterNot(_ == "  ").mkString("\n")+ "\n}."
        namespace + "\n\n" + sig
//      case OMMOD(meta : MPath) => 
//        "%meta " + meta.doc.last + "?" + meta.name + "."
        
      case Include(from, to) => "%include " + to.toMPath.last + "."
      case c : Constant =>
        println("constant : " + operators)
        val tp = c.tp match {
          case None => ""
          case Some(t) => " : " + presentTerm(t, operators)
        }
        val df = c.df match {
          case None => ""
          case Some(t) => " = " + presentTerm(t, operators)
        }
        val not = c.not match {
          case None => ""
          case Some(n) => " # " + n.toString
        }
        c.path.last + tp + df + not + "."

//      case s : Structure => "%include " + s.from.toString
     
      case t : Term => presentTerm(t, operators)
      case _ =>
        println("unsupported content element for text presentation " + con.toNode)
        ""
    }
  }

  private def presentTerm(t : Term, notations : List[TextNotation]) : String = t match {
    case OMA(OMID(p), args) =>
      println("found : " + t.toString)
      notations.find(op => op.name == p) match {
        case None =>  //using default notations
          println("not found notation for constant with path " + p)
          println("using : " + p.last + "  " + args.map(x => presentTerm(x, notations)).mkString(" "))
          "(" + p.last + "  " + args.map(x => presentTerm(x, notations)).mkString(" ") + ")" 
        case Some(notation) =>
          println("found notation : " + notation.toString)
          val argMks = notation.markers collect {
            case a : Arg => a
          }
          if (argMks.length == args.length) {
        	val l =  notation.markers map {
        	  case a : Arg => presentTerm(args(a.n), notations)
        	  case Delim(s) => s
        	}
        	println("using : " + l.mkString(" "))
        	l.mkString("("," ",")")
          } else { // a seq arg with arbitrary length
            val seqLen = args.length - argMks.length + 1
            var foundSeq = false
            def getPos(pos : Int) : Int = {
              if (foundSeq)
                pos + seqLen - 1
              else
                pos
            }    
            val l = notation.markers map {
              case Delim(s) => s
              case Arg(pos) => presentTerm(args(getPos(pos)), notations)
              case SeqArg(pos, sep) =>
                foundSeq = true
                args.slice(pos, pos + seqLen).map(presentTerm(_, notations)).mkString(" " + sep.s + " ")
            }     
            l.mkString("(", " ",")")                 
          }
      }

    case OMBINDC(OMID(p), context, None, body) =>
      println(t.toNode)
      val tmpargs = context.variables collect {
        case VarDecl(s, _, _, _*) => s :: Nil //TODO handle var type and def
      }
      println("context : " + context.variables)
      println("tmpargs : " + tmpargs)
      val args = tmpargs.flatten
      
      notations.find(op => op.name == p) match {
        case None => presentTerm(body, notations) //assuming implicit binder
        case Some(notation) =>
          println("found notation " + notation.toString)
          println("with args" + args.toString)
          val l =  notation.markers map {
            case a : Arg => args(a.n)
            case Delim(s) => s
          }
          "(" + l.mkString(" ") + " " + presentTerm(body, notations) + ")"
      }

    case OMV(s) => s.toPath

    case OMID(p) =>
      println("got here with " + p.toString)
      p.last
    case _ =>
      println("unsupported content element for text presentation " + t.toString)
      ""
  }
  
}