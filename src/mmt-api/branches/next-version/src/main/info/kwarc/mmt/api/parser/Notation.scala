package info.kwarc.mmt.api.parser

import scala.Int
import info.kwarc.mmt.api.metadata.HasMetaData
import info.kwarc.mmt.api.{Content, ImplementationError, GlobalName}
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api._
/**
 * An operator is something that takes arguments, e.g. and, or, forall
 */
class Operator(val name : GlobalName, val notation : Notation) {

  def extend(not : Notation) : Operator = new Operator(name, notation.extend(not))

  def toString(args : List[Formal]) = {
    var s = ""
    var i = 0
    var j = 0

    val markers = notation.mrks

    while (i < markers.length) {
      s += {markers(i) match {
        case StrMk(value) => value + " "
        case ArgMk(pos) =>
          val ret = args(j)
          j += 1
          ret
      }
      }
      i += 1
    }
    s

  }

  override def toString = notation.toString

  def isBinder = false
}

/**
 * a binder is a operator that aditionally binds some context to some body
 * @param name the global name of the binder
 * @param notation the notation of the binder
 * @param context a function that extracts the bound variables from the binder arguments
 * @param binding a binding direction representing the body of the object
 * e.g. if var : type is a declaration then the standard Pi binder denoted by {var : type} body can be written as
 * Notation is List("{", 1, ":", 2, "}"), context(l) = l.head, and binding is right.
 */
class Binder(name : GlobalName, notation : Notation, val context : List[Token] => Context, val nameArgNr : Int, val binding : Binding) extends Operator(name, notation) {
  override def isBinder = true
}

/**
 * A notation is a list of markers which represent either strings to be matched or argument positions to be filled.
 * It also has notation properties encoding things such as associativity or precedence.
 * e.g. given the binary operator log(nr,base) one can define the notation with markers List("log",2,1)
 *      where "log" is a string marker, 2 is a argument marker for argument number 2 of log (base)
 *      and 1 for argument number 1 (nr). Then, log 2 16 is parsed as log(16,2)
 * @param mrks the list of markers
 * @param props  the notation properties
 */
case class Notation(mrks : List[Marker], props : NotationProperties) extends HasMetaData {

  def markers : List[StrMk] = mrks collect {case s : StrMk => s}
  def args : List[List[ArgMk]] = mrks.foldLeft[List[List[ArgMk]]](Nil :: Nil)((r,m) => m match {case s : StrMk => Nil :: (r.head.reverse :: r.tail) case a : ArgMk => (a :: r.head) :: r.tail}).reverse

  val argNr = args.flatten.length

  def extend(not : Notation) : Notation = {
    def _processMarkers(mrks : List[Marker]) : List[Marker] = mrks match {
      case Nil => Nil
      case StrMk(value) :: l => StrMk(value) :: _processMarkers(l)
      case ArgMk(pos) :: l => ArgMk(pos + argNr - 1) :: _processMarkers(l)  // the args of the base minus the deleted shared one
    }
    Notation(mrks ::: _processMarkers(not.mrks.tail), props)  //.tail to remove the shared argument
  }
  def getStrMarkers : List[String] = markers.flatMap( _  match {case StrMk(s) => List(s) case _ => Nil})

  def precedence = props.precedence.value
  def assoc = props.assoc

  override def toString = mrks.mkString(" ") + "," + precedence + "," + assoc

  def toNode =
    <TextNotation>
      <Markers>
        {mrks.map(m => m.toNode)}
      </Markers>
      <Precedence>
        {precedence}
      </Precedence>
      <Assoc>
        {assoc}
      </Assoc>
    </TextNotation>

}


object TextNotation {
  def parse(s : scala.xml.Node) : Notation = s match {
    case <TextNotation>{xmlMarkers}{xmlPrecedence}{xmlAssoc}</TextNotation> =>
      val markers = parseMarkers(xmlMarkers)
      val precedence = parsePrecedence(xmlPrecedence)
      val assoc = parseAssoc(xmlAssoc)
      new Notation(markers, NotationProperties(precedence, assoc))
  }

  private def parseAssoc(n : scala.xml.Node) : Assoc = n match {
    case <Assoc>{s}</Assoc> => s.toString match {
      case "Left"  => AssocLeft()
      case "Right" => AssocRight()
      case "None" => AssocNone()
      case "Seq" => AssocSeq()
      case _ => throw ParseError("Invalid assoc value : " + s)
    }
    case _ => throw ParseError("Invalid XML representation of Assoc in: \n" + n)

  }

  private def parsePrecedence(n : scala.xml.Node) : Precedence = n match {
    case <Precedence>{value}</Precedence> =>
      try {
        Precedence(value.toString.toInt)
      } catch {
        case _ => throw ParseError("Invalid XML representation of Precedence(value : Int) in: \n" + n)
      }
    case _ => throw ParseError("Invalid XML representation of Precedence in: \n" + n)
  }

  private def parseMarkers(n : scala.xml.Node) : List[Marker] = n match {
    case <Markers>{xmlMarkers @ _*}</Markers> =>
      xmlMarkers.map(x => parseMarker(x)).toList
    case _ => throw ParseError("Invalid XML representation of marker list in: \n" + n)
  }

  private def parseMarker(n : scala.xml.Node) : Marker = n match {
    case <StrMk>{s}</StrMk> => StrMk(s.toString)

    case <ArgMk>{s}</ArgMk> =>
      try {
        ArgMk(s.toString.toInt)
      } catch {
        case _ => throw ParseError("Invalid XML representation of ArgMk(pos : Int) in: \n" + n)
      }
    case _ => throw ParseError("Invalid XML representation of marker in: \n" + n)

  }


  def parse(s : String) : Notation = {
    s.split(",").toList match {
      case not :: prec :: asc :: Nil =>
        val precedence = Precedence(prec.toInt)
        val assoc = asc match {
          case "None" => AssocNone()
          case "Left" => AssocLeft()
          case "Right" => AssocRight()
          case "Seq" => AssocSeq()
        }

        val markers = not.split(" ") map {s =>
          try {
           ArgMk(s.toInt)
          } catch {
            case _ => StrMk(s)
          }
        }

        println(Notation(markers.toList, NotationProperties(precedence, assoc)))
        Notation(markers.toList, NotationProperties(precedence, assoc))
      case _ =>
        throw ParseError("Invalid notation declaration : " + s)
    }
  }


  def present(con : Content, operators : List[Operator]) : String = {
    println("current con : " + con.toString)
    con match {
      case d : DeclaredTheory =>
        println(d.path)
        "%sig " + d.path.last + " = {\n" + d.components.map(c => "  " + present(c, operators)).filterNot(_ == "  ").mkString("\n")+ "\n}."
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


      case _ =>
        println("unsupported content element for text presentation " + con.toString)
        ""

    }
  }


  private def presentTerm(t : Term, operators : List[Operator]) : String = t match {
    case OMA(OMID(p), args) =>
      operators.find(op => op.name == p) match {
        case None =>
          println("not found notation for constant with path " + p)
          p.last + "  " + args.map(x => presentTerm(x, operators)).mkString(" ")
        case Some(op) =>
          val l =  op.notation.mrks map {
            case ArgMk(pos) => presentTerm(args(pos), operators)
            case StrMk(s) => s
          }
          l.mkString(" ")
      }

    case OMBINDC(OMID(p), context, None, body) =>
      val tmpargs = context.variables collect {
        case TermVarDecl(s, Some(tp),_,_) => s :: presentTerm(tp, operators) :: Nil
      }
      val args = tmpargs.flatten
      operators.find(op => op.name == p) match {
        case None => p.last + "  " + args.mkString(" ")
        case Some(op) =>
          val l =  op.notation.mrks map {
            case ArgMk(pos) => args(pos)
            case StrMk(s) => s
          }
          l.mkString(" ")
      }

    case OMV(s) => s

    case OMID(p) =>
      println("got here with " + p.toString)
      p.last
    case _ =>
      println("unsupported content element for text presentation " + t.toString)
      ""
  }
}


/**
 * A Marker is an element of a notation
 */
trait Marker {
  def toNode : scala.xml.Node
}

/**
 * An Argument Marker is a marker that represents an argument for the operator encoded by the notation.
 * It should be filled with a term during parsing and that term will become the argument at position pos for the operator
 *@param pos the number of the operator argument
 */
case class ArgMk(pos : Int) extends Marker {
  override def toString = pos.toString
  def toNode =
    <ArgMk> {pos.toString} </ArgMk>
}

/**
 * A String Marker is a marker that represents a fixed part of a notation.
 * @param value the string that the marker represents
 */
case class StrMk(value : String) extends Marker {
  override def toString = value
  def toNode =
    <StrMk> {value} </StrMk>
}


case class NotationProperties(precedence : Precedence, assoc : Assoc)

/**
 * Precedence of a notation is used for resolving ambiguities during parsing. Higher precedence has higher priority.
 * @param value the integer precedence value
 */
case class Precedence(value : Int) {
  override def toString = value.toString
}

/**
 * Association of a notation is used during parsing to deal with consecutive, (partially) overlapping occurrences of the same notation
 * e.g. a + b + c
 */
sealed trait Assoc

/**
 * Left Association means the left-most occurrence has priority
 * e.g. a + b + c is parsed as +(+(a,b),c)
 */
case class AssocLeft() extends Assoc {
  override def toString = "Left"
}

/**
 * Right Association means the right-most occurrence has priority
 * e.g. a + b + c is parsed as +(a,+(b,c))
 */
case class AssocRight() extends Assoc {
  override def toString = "Right"
}

/**
 * None Association means consecutive (partially) overlapping occurrences of the same notation are not accepted
 * e.g. a + b + c gives a parsing error.
 */
case class AssocNone() extends Assoc {
  override def toString = "None"
}

/**
 * Sequence Association means the (several) consecutive notations are treated as one operator with more (all found) arguments
 * e.g. a + b + c  is parsed as +(a,b,c)
 */
case class AssocSeq() extends Assoc {
  override def toString = "Seq"
}

/**
 * Binding is the property of a notation of eagerly seizing an entire chunk of markers as one argument and parse them afterwards.
 * (basically adds parentheses)
 * It is related to precedences in the sense that both binding and precedences as defined here are specializations of generalized precedences which
 * have input and output precedences for each argument.
 * e.g. Pi and Lambda are example (right) binders. We denote Pi as "{context} body" and lambda as "[context] body"
 */
sealed trait Binding

/**
 * Right Binding means this notation when matched will take the all arguments in the right side of it and treat them as one expression
 * e.g. {x} x + x  maps to Pi(x, +(x,x)) even if Pi has a higher precedence than +.
 */
case class BindRight() extends Binding
/**
 * Left Binding means this notation when matched will take the all arguments in the left side of it and treat them as one expression
 */
case class BindLeft() extends Binding



