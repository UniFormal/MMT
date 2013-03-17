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

sealed abstract class ArgumentMarker extends Marker with ArgumentComponent with ScopeComponent

/** an argument
 * @param n absolute value is the argument position, negative iff it is in the binding scope
 */
case class Arg(number: Int) extends ArgumentMarker {
   override def toString = number.toString
   def by(s:String) = SeqArg(number,Delim(s))
}

/** a sequence argument 
 * @param n absolute value is the argument position, negative iff it is in the binding scope
 * @param sep the delimiter between elements of the sequence 
 */
case class SeqArg(number: Int, sep: Delim) extends ArgumentMarker {
   override def toString = number.toString + sep + "…"
   override def isSequence = true
}

/** a variable binding 
 * @param n the number of the variable
 * @param typed true if the variable carries a type (to be inferred if omitted)
 * @param sep if given, this is a variable sequence with this separator;
 *   for typed variables with the same type, only the last one needs a type 
 */
case class Var(number: Int, typed: Boolean, sep: Option[Delim]) extends Marker with VariableComponent {
   override def toString = "V" + number.toString + (if (typed) "T" else "") + (sep.map(_.toString + "…").getOrElse(""))
   override def isSequence = sep.isDefined
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

/**
 * see [[Arity]]
 * 
 * Almost all ArityComponents arise as the non-Delimiter NotationMarkers 
 */
sealed trait ArityComponent {
   val number: Int
   def isSequence: Boolean = false
}
sealed trait ArgumentComponent extends ArityComponent
sealed trait VariableComponent extends ArityComponent
sealed trait ScopeComponent extends ArityComponent
/** implicit argument, corresponds to an ArgumentComponent not mentioned in the notation */
case class ImplicitArg(number: Int) extends ArgumentComponent

/**
 * the arity of a symbol describes what kind of objects can be formed from it
 * 
 * special cases and analogs in the OpenMath role system for a symbol s:
 *  no arguments, variables, scopes: constant, s
 *  some arguments, no variables, scopes: application OMA(s, args)
 *  no arguments, some variables: binder, OMBINDC(s, vars, scopes)
 *  some arguments, some variables: application+binder as in OMBINDC(OMA(s, args), vars, scopes), as suggested by Kohlhase, Rabe, MiCS 2012
 *  as above but exactly 1 scope: usual binders, generalized binders as suggested by Davenport, Kohlhase, MKM 2010
 */
case class Arity(arguments: List[ArgumentComponent], variables: List[VariableComponent], scopes: List[ScopeComponent]) {
   def components = arguments ::: variables ::: scopes
   def length = components.length
   def isConstant    =    arguments.isEmpty  &&    variables.isEmpty  && scopes.isEmpty
   def isApplication = (! arguments.isEmpty) &&    variables.isEmpty  && scopes.isEmpty
   def isBinder      =    arguments.isEmpty  && (! variables.isEmpty)
   def isPlainBinder =    arguments.isEmpty  && (! variables.isEmpty) && scopes.length == 1
   def isApplBinder  = (! arguments.isEmpty) && (! variables.isEmpty)
   def isPlainApplBinder  = (! arguments.isEmpty) && (! variables.isEmpty) && scopes.length == 1
   def numSeqArgs = arguments.count(_.isInstanceOf[SeqArg])
   def numSeqVars = variables.count {
      case Var(_,_, Some(_)) => true
      case _ => false
   }
}

object Arity {
   def constant = Arity(Nil,Nil,Nil)
   def plainApplication = Arity(List(SeqArg(1,Delim(""))), Nil, Nil)
   def plainBinder = Arity(Nil, List(Var(1,false,Some(Delim("")))), List(Arg(-2)))
}

case class InvalidNotation(msg: String) extends java.lang.Throwable

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

   def getArity = {
      var args: List[ArgumentComponent] = Nil
      var vars : List[VariableComponent] = Nil
      var scopes : List[ScopeComponent] = Nil
      //collect components from markers
      markers foreach {
         case a : ArgumentMarker =>
            if (a.number > 0)
               args ::= a
            else if (a.number < 0)
               scopes ::= a
            else
               throw InvalidNotation("illegal marker: " + a)
         case v: VariableComponent =>
            vars ::= v
         case _ =>
      }
      // sort by component number
      args = args.sortBy(_.number)
      vars = vars.sortBy(_.number)
      scopes = scopes.sortBy(_.number)
      // args with all implicit argument components added
      var argsWithImpl: List[ArgumentComponent] = Nil
      var i = 1
      args foreach {a =>
         while (i < a.number) {
            //add implicit argument in front of the current one
            argsWithImpl ::= ImplicitArg(i)
            i += 1
         }
         argsWithImpl ::= a
         i += 1
      }
      //TODO: check all args.number < vars.number < scopes.numbers; no gaps in variables or scopes
      //add implicit arguments after the last one (the first variable tells us if there are implicit arguments after the last one)
      val totalArgs = vars.headOption.map(_.number).getOrElse(i) - 1
      while (i <= totalArgs) {
         argsWithImpl ::= ImplicitArg(i)
         i += 1
      }
      args = argsWithImpl.reverse
      Arity(args, vars, scopes)
   }

   /**
    * flattens all sequence arguments/variables
    * 
    * @param args number of arguments
    * @param vars number of variables
    * @param scs number of scopes
    * 
    * if there is more than 1 sequence arguments, the available arguments are evenly distributed over the sequences
    * remaining arguments are distributed in order of content position
    * 
    * multiple sequence variables are treated accordingly
    * it is assumed there are no sequences in the scopes
    */
   def flatten(args: Int, vars: Int, scs: Int) : List[Marker] = {
      // distributes available components to numTotal normal/sequence components where the positions of the sequences are given by seqs
      // returns: number of arguments per sequence and cutoff below which sequences get one extra argument
      def distribute(available: Int, numTotal: Int, seqs: List[Int]):(Int,Int) = {
         val numSeqs = seqs.length
         if (numSeqs == 0) return (0,0)
         val numSingle = numTotal - numSeqs
         val availableForSeqs = available - numSingle
         //the number of arguments that every sequence argument gets
         val perSeq = availableForSeqs / numSeqs
         //the first sequence that does not get an extra argument
         val cutoff = seqs.sortWith(_<_).apply(availableForSeqs % numSeqs)
         (perSeq,cutoff)
      }
      val arity = getArity
      val seqArgPositions = arity.arguments.flatMap {
         case SeqArg(n,_) if n > 0 => List(n)
         case _ => Nil
      }
      val (perSeqArg, seqArgCutOff) = distribute(args, arity.arguments.length, seqArgPositions)
      val seqVarPositions = arity.variables.flatMap {
         case Var(n,_,Some(_)) => List(n)
         case _ => Nil
      }
      val (perSeqVar, seqVarCutOff) = distribute(vars, arity.variables.length, seqVarPositions)
      //maps component positions to position in flattened notation, by including the arguments of the preceding sequences
      def remap(p: Int): Int = {
         var i = p.abs
         markers.foreach {
            case SeqArg(n,_) if n < p || p < 0 =>
               i += perSeqArg - 1
               if (n < seqArgCutOff) i += 1
            case Var(n,_,Some(_)) if n < p || p < 0 => 
               i += perSeqVar - 1
               if (n < seqVarCutOff) i += 1
            case _ =>
         }
         if (p > 0) i else -i
      }
      markers.flatMap {
         case Arg(n) =>
            List(Arg(remap(n)))
         case SeqArg(n, sep) if n > 0 =>
            val length = if (n < seqArgCutOff) perSeqArg+1 else perSeqArg
            val first = remap(n)
            if (length == 0) Nil
            else Range(1,length).toList.flatMap(i => List(Arg(first+i-1), sep)) ::: List(Arg(first+length-1))
         case Var(n, tpd, None) =>
            List(Var(remap(n), tpd, None))
         case v @ Var(n, tpd, Some(sep)) =>
            val length = if (n < seqVarCutOff) perSeqVar+1 else perSeqVar
            val first = remap(n)
            if (length == 0) Nil
            else Range(1,length).toList.flatMap(i => List(Var(first+i-1, tpd, None), sep)) ::: List(Var(first+length-1, tpd, None))
         case d: Delimiter =>
            List(d)
      }
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
  
   private def markerString = markers.map(_.toString).mkString(" ")
   def toText = markerString + (if (precedence != Precedence.integer(0)) " prec " + precedence.toString else "")
   override def toString = "Notation for " + name + ": " + markerString
   def toNode = 
     <text-notation name={name.toPath} precedence={precedence.toString} markers={markerString}/>
   // the first delimiter of this notation
   def firstDelimString : Option[String] = markers mapFind {
      case d: Delimiter => Some(d.text)
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
            while (i < s.length && s(i).isDigit) {i+=1}
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
   //val contextNotation = new TextNotation(utils.mmt.brackets, List(Var(1,true,Some(Delim(",")))), bracketLevel)
   
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
  
   /** String parsing methods
    *
    * the default precedence is 0; exception: if the notation contains (, it is above bracketLevel
    */
   def parse(str : String, conPath : GlobalName) : TextNotation = {
    str.split("prec").toList match {
      case not :: p :: Nil =>        
        val prec = Precedence.parse(p)
        parseNot(not, prec, conPath)
      case not :: Nil =>
        val defaultPrec = if (not.contains("(")) Precedence.integer(1000001) else Precedence.integer(0)
        parseNot(not, defaultPrec, conPath)
      case _ =>
        throw ParseError("Invalid notation declaration : " + str)
    }
  }
   
  private def parseNot(str : String, prec : Precedence, conPath : GlobalName) : TextNotation = {
    val protoMks = str.split("\\s+").filterNot(_ == "") map {s =>
      try {
        s.toInt
      } catch {
        case _ : Throwable => s
      }
    }
    apply(conPath, prec)(protoMks :_*)
  }
}

/** defines some implicit conversions to produce Markers */
object NotationConversions {
   /** integers are converted to argument markers */
   implicit def fromInt(n:Int) = Arg(n)
   /** strings are converted to delimiters */
   implicit def fromString(s:String) = Delim(s)
}

