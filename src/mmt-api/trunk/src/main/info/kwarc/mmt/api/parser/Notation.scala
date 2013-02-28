package info.kwarc.mmt.api.parser

import info.kwarc.mmt.api._
import NotationConversions._
import utils.MyList._
import modules._
import symbols._
import objects._
import presentation._ 

/** Objects of type Marker make up the pattern of a Notation */
sealed abstract class Marker {
   override def toString : String
}

/** Markers that are delimiters */
sealed abstract class Delimiter extends Marker {
   val s: String
   def text = s
}

/** a delimiter
 * @param s the delimiting String
 */
case class Delim(s: String) extends Delimiter {
   override def toString = s
}
/** a delimiter
 * @param s the delimiting String
 */
case class SecDelim(s: String, wsAllowed: Boolean = true) extends Delimiter {
   override def toString = (if (wsAllowed) "" else "_") + s
}

/** special delimiters that are expanded to a string based on the declaration that the notation occurs in
 * 
 * These should never occur in notations that are used by te Scanner. They should be expanded first.
 */
abstract class PlaceholderDelimiter extends Delimiter

/**
 * expands to the name of the instance
 * 
 * only legal for notations within patterns
 */
case class InstanceName(path: GlobalName) extends PlaceholderDelimiter {
   val s = if (path.name.length <= 1) "" else path.name.init.toPath
}

/**
 * expands to the name of the symbol
 * 
 * useful for repetitive notations that differ only in the name
 */
case class SymbolName(path: GlobalName) extends PlaceholderDelimiter {
   val s = if (path.name.length < 1) "" else path.name.last.toPath
}

/** an argument
 * @param n absolute value is the argument position, negative iff it is in the binding scope
 */
case class Arg(number: Int) extends Marker {
   override def toString = number.toString
   def by(s:String) = SeqArg(number,Delim(s))
}

/** a sequence argument 
 * @param n absolute value is the argument position, negative iff it is in the binding scope
 * @param sep the delimiter between elements of the sequence 
 */
case class SeqArg(number: Int, sep: Delim) extends Marker {
   override def toString = number.toString + sep + "…"
}

/** a variable binding 
 * @param n the number of the variable
 * @param typed true if the variable carries a type (to be inferred if omitted)
 * @param sep if given, this is a variable sequence with this separator;
 *   for typed variables with the same type, only the last one needs a type 
 */
case class Var(number: Int, typed: Boolean, sep: Option[Delim]) extends Marker {
   override def toString = "V" + number.toString + (if (typed) "T" else "") + (sep.map(_.toString + "…").getOrElse(""))
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

sealed abstract class NotationType
case object OMSNotation extends NotationType
/** type of notations for applications
 * @param concretePositions the positions of the arguments in the concrete syntax
 * ordered according to the abstract syntax
 */ 
case class OMANotation(concretePositions: List[Int]) extends NotationType
case class OMBINDNotation(vars: List[Int], scope: Int) extends NotationType

/**
 * @param name the symbol to which this notation applies
 * @param markers the Markers making up the notation
 * @param precendence the precedence, notations with higher precedence are used first, thus grab larger subterms
 * 
 * a typed Var must be preceded by a Delim because Var.key does not trigger the notation
 * 
 * not all markers may be Arg because such notations cannot be parsed
 * 
 * if the only marker is SeqArg, it must hold that OMA(name, List(x)) = x because sequences of length 1 are parsed as themselves 
 */
class TextNotation(val name: GlobalName, val markers: List[Marker], val precedence: Precedence) extends Notation {
   val wrap = false 
   val oPrec = Some(precedence)

   def getType = {
      val arguments = markers mapPartial {
         case Arg(n) if n > 0 => Some(n)
         case SeqArg(n,_) if n > 0 => Some(n)
         case _ => None
      }
      val boundVars = markers mapPartial {
         case Var(n, _, _) => Some(n)
         case _ => None
      }
      val scopes = markers mapPartial {
         case Arg(n) if n < 0 => Some(-n)
         case _ => None
      }
      if (arguments == Nil && boundVars == Nil && scopes == Nil)
         Some(OMSNotation)
      else if (arguments != Nil && boundVars == Nil && scopes == Nil) {
         val order = arguments.zipWithIndex.sortBy(_._1).map(_._2) 
         Some(OMANotation(order))
      } else if (arguments == Nil && boundVars != Nil && scopes.length == 1)
         Some(OMBINDNotation(boundVars, scopes(0)))
      else
         None
   }
   
   //TODO add other cases & check presentation
   lazy val pres = {  
     val tokens = markers map {
       case d : Delimiter => Fragment("constant", presentation.Text(name.toPath), presentation.Text(d.s))
       case Arg(p) => Component(NumberedIndex(p.abs),oPrec.map(_.weaken))
       case SeqArg(p,sep) => Iterate(NumberedIndex(1),
    		  						 NumberedIndex(-1),  
    		  						 OpSep() + Fragment("constant", presentation.Text(name.toPath), presentation.Text(sep.s)) + OpSep(),
    		  						 oPrec.map(_.weaken)) //TODO fix indexes for sequence arguments 
       case Var(n, typed, sep) => Component(NumberedIndex(n), None) //TODO Use Key
       /*case SeqVar(n, key, sep) => Iterate(
           NumberedIndex(2),
           NumberedIndex(-2), 
           OpSep() + Fragment("constant", presentation.Text(name.toPath), presentation.Text(sep.s)) + OpSep(),
           oPrec.map(_.weaken)) */
     }
     presentation.PList(tokens)
   }
   val key = presentation.NotationKey(Some(name), Role_Notation)
   val nset = name.module.toMPath
  
   override def toString = "Notation for " + name + ": " + markers.map(_.toString).mkString(" ")
   def toNode = 
     <text-notation name={name.toPath} precedence={precedence.toString} markers={markers.map(_.toString).mkString(" ")}/>
   // the first delimiter of this notation
   private val firstDelimString : Option[String] = markers mapFind {
      case d: Delimiter => Some(d.s)
      case SeqArg(_, Delim(s)) => Some(s)
      case _ => None
   }
   def openLeftArgs : Int = {
      var i = 0
      markers foreach {
         case a: Arg => i += 1
         case _:SeqArg => return i+1
         case _: Var => return i+1
         case d: Delimiter => return i
      }
      i
   }
   def isLeftOpen = openLeftArgs > 0
   /** @return true if next is first delimiter Token */
   def applicable(pickableTokens: Int, next: Token): Boolean = {
      firstDelimString == Some(next.word) && openLeftArgs <= pickableTokens
   }
   /** creates a new ActiveNotation with this notation's markers */
   def open(scanner: Scanner, firstToken: Int): ActiveNotation = {
      val an = new ActiveNotation(scanner, this, firstToken)
      an
   }
}

object TextNotation {
   def apply(name: GlobalName, prec: Precedence)(ms: Any*): TextNotation = {
      val markers : List[Marker] = ms.toList map {
         case i: Int => Arg(i)
         case m: Marker => m
         case "%i" => InstanceName(name)
         case "%n" => SymbolName(name)
         case s: String if s.startsWith("V") =>
            //Vn ---> variable
            var i = 1
            while (s(i).isDigit) {i+=1}
            val n = s.substring(1,i).toInt
            val d = s.substring(i)
            if (d == "")
               //Vn ---> untyped, no sequence
               Var(n, false, None)
            else if (d == "T")
               //VnT ---> typed, no sequence
               Var(n, true, None)
            else if (d.startsWith("T") && d.endsWith("…")) {
               //VnTsep… ---> typed, sequence
               val sep = d.substring(1,d.length-1)
               Var(n, true, Some(Delim(sep)))
            } else if (d.endsWith("…")) {
               //Vnsep… ---> untyped, sequence
               val sep = d.substring(0,d.length-1)
               Var(n, false, Some(Delim(sep)))
            } else
               throw ParseError("not a valid marker " + s)
         case s: String if s.endsWith("…") =>
            //nsep… ---> sequence argument/scope
            var i = 0
            while (s(i).isDigit) {i+=1}
            val n = s.substring(0,i).toInt
            val rem = s.substring(i,s.length-1)
            SeqArg(n, Delim(rem))
         case "" => throw ParseError("not a valid marker")
         case s:String =>
            try {
               val n = s.toInt
               //n ---> arguments, no sequence
               Arg(n)
            } catch {case e: Throwable =>
               //other string ---> delimiter
               Delim(s)
            }
         case m => throw ParseError("not a valid marker " + m.toString)
      }
      new TextNotation(name, markers, prec)
   }
   
   /** the precedence of the notation ( 1 )
    * 
    * notations with round brackets must have a higher precedence than this to be recognized
    */
   val bracketLevel = Precedence.integer(1000000)
   /** 
    * a special Notation for utils.mmt.brackets
    * matches ( 1 ) with precedence bracketLevel
    */
   val bracketNotation = new TextNotation(utils.mmt.brackets, List(Delim("("),Arg(1),Delim(")")), bracketLevel)
   
   /** XML parsing methods */
   def parse(n : scala.xml.Node, name : GlobalName) : TextNotation = n match {
    case <text-notation/> =>
      val name = Path.parseS(utils.xml.attr(n,"name"), utils.mmt.mmtbase)
      val markers = utils.xml.attr(n, "markers").split("\\s").toList
      if (markers == List(""))
         throw ParseError("invalid notation in: " + n)
      val precedence = Precedence.parse(utils.xml.attr(n, "precedence"))
      apply(name, precedence)(markers : _*)
    case _ => throw ParseError("invalid notation in \n" + n)
  }
  
   /** String parsing methods */
   def parse(str : String, conPath : GlobalName) : TextNotation = {
    str.split("prec").toList match {
      case not :: p :: Nil =>        
        val prec = Precedence.parse(p)
        parseNot(not, prec, conPath)
      case not :: Nil => 
        parseNot(not, Precedence.integer(0), conPath)
      case _ =>
        throw ParseError("Invalid notation declaration : " + str)
    }
  }
   
  private def parseNot(str : String, prec : Precedence, conPath : GlobalName) : TextNotation = {
    val protoMks = str.split("\\s") map {s =>
      try {
        s.toInt
      } catch {
        case _ : Throwable => s
      }
    }
    apply(conPath, prec)(protoMks :_*)
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
        
      case Include(from, to) => "%include " + to.last + "."
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
        	  case a : Arg => presentTerm(args(a.number), notations)
        	  case d: Delimiter => d.text
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
              case d: Delimiter => d.text
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
            case a : Arg => args(a.number)
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