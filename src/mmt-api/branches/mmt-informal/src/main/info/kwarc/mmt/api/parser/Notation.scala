package info.kwarc.mmt.api.parser

import info.kwarc.mmt.api._
import NotationConversions._
import utils.MyList._
import modules._
import symbols._
import presentation.{Text => PText, _}
import objects._

case class InvalidNotation(msg: String) extends java.lang.Throwable

/**
 * A TextNotation is a Notation that can be used for parsing objects in text syntax
 * @param name the symbol to which this notation applies
 * @param allMarkers the Markers making up the notation
 * @param precendence the precedence, notations with lower precedence are tried first, thus grab larger subterms
 * 
 * a typed Var must be preceded by a Delim because Var.key does not trigger the notation
 * 
 * not all markers may be Arg because such notations cannot be parsed
 * 
 * if the only marker is SeqArg, it must hold that OMA(name, List(x)) = x because sequences of length 1 are parsed as themselves 
 */
class TextNotation(val name: GlobalName, fixity: Fixity, val precedence: Precedence) extends ComplexNotation {
   /** the markers of this notation that should be used for parsing */
   val markers = fixity.markers
   val key = NotationKey(Some(name), Role_application(None))
   val nset = name.module.toMPath
  
   def toText = fixity.toString + (if (precedence != Precedence.integer(0)) " prec " + precedence.toString else "")
   override def toString = "notation for " + name.toString + ": " + toText + " (markers are: " + markers.map(_.toString).mkString(" ") + ")" 
   def toNode = 
     <text-notation name={name.toPath} precedence={precedence.toString} fixity={fixity.fixityString} arguments={fixity.argumentString}/>

   def getArity = {
      var args: List[ArgumentComponent] = Nil
      var vars : List[VariableComponent] = Nil
      var scopes : List[ScopeComponent] = Nil
      //collect components from markers
      fixity.allMarkers foreach {
         case a : ArgumentMarker =>
            if (a.number > 0)
               args ::= a
            else if (a.number < 0)
               scopes ::= a
            else {
               throw InvalidNotation("illegal marker: " + a)
            }
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

   /** @return true if ComplexTerm(name, args, vars, scs) has enough components for this notation */
   def canHandle(args: Int, vars: Int, scs: Int) = {
      val arity = getArity
      arity.numNormalArgs <= args &&
      arity.numNormalVars <= vars &&
      arity.numNormalScopes <= scs
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
    * 
    * pre: canHandle(args, vars, scs) == true
    */
   def flatten(args: Int, vars: Int, scs: Int) : (List[Marker], List[ImplicitArg]) = {
      val arity = getArity
      val (perSeqArg, seqArgCutOff) = arity.distributeArgs(args)
      val (perSeqVar, seqVarCutOff) = arity.distributeVars(vars)
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
      val implicits = arity.arguments.flatMap {
         case ImplicitArg(n) => List(ImplicitArg(remap(n)))
         case _ => Nil
      }
      val flatMarkers = markers.flatMap {
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
      (flatMarkers, implicits)
   }
   
   /**
    * flattens and transforms markers into Presentation
    */
   def presentation(args: Int, vars: Int, scopes: Int) = {
     val (flatMarkers, implicits) = flatten(args, vars, scopes)
     val implicitsP = implicits map {
        case ImplicitArg(n) =>
           Fragment("implicit", Component(NumberedIndex(n), BracketInfo())) + ArgSep()
     }
     val numDelims = flatMarkers.count(_.isInstanceOf[parser.Delimiter])
     var numDelimsSeen = 0
     val tokens = flatMarkers.map {
       case d : Delimiter =>
          numDelimsSeen += 1
          val delim = ArgSep() + Fragment("constant", PText(name.toPath), PText(d.text)) + ArgSep()
          if (numDelimsSeen == 1) {
             // add implicit arguments to the first delimiter
             delim + PList(implicitsP)
          } else
             delim
       case Arg(p) =>
          val delimitation = if (numDelimsSeen == 0) -1 else if (numDelimsSeen == numDelims) 1 else 0
          Component(NumberedIndex(p.abs), BracketInfo(Some(precedence), Some(delimitation)))
       case Var(n, _, None) => Component(NumberedIndex(n), BracketInfo())
       case SeqArg(n,sep) => throw ImplementationError("non-flat marker")
       case Var(n,_,Some(sep)) => throw ImplementationError("non-flat marker")
     }
     PList(tokens)
   }
   // the first delimiter of this notation
   def firstDelimString : Option[String] = markers mapFind {
      case d: Delimiter => Some(d.text)
      case SeqArg(_, Delim(s)) => Some(s)
      case _ => None
   }
   def openArgs(fromRight: Boolean) : Int = {
      var i = 0
      val ms = if (fromRight) markers.reverse else markers
      ms foreach {
         case a: Arg => i += 1
         case a: ImplicitArg =>
         case _:SeqArg => return i+1
         case _: Var => return i+1
         case d: Delimiter => return i
      }
      i
   }
   def isLeftOpen = openArgs(false) > 0
   def isRightOpen = openArgs(true) > 0
}

object TextNotation {
   def apply(name: GlobalName, prec: Precedence)(ms: Any*): TextNotation = {
      val markers : List[Marker] = ms.toList map {
         case i: Int => Arg(i)
         case m: Marker => m
         case s: String => Marker.parse(name, s)
      }
      new TextNotation(name, Mixfix(markers), prec)
   }
   
   /** the precedence of the notation ( 1 )
    * 
    * notations with round brackets must have a higher precedence than this to be recognized
    */
   val bracketLevel = Precedence.integer(-1000000)
   /** 
    * a special Notation for utils.mmt.brackets
    * matches ( 1 ) with precedence bracketLevel
    */
   val bracketNotation = new TextNotation(utils.mmt.brackets, Mixfix(List(Delim("("),Arg(1),Delim(")"))), bracketLevel)
   val contextNotation = new TextNotation(utils.mmt.context, Mixfix(List(Delim("["), Var(1,true,Some(Delim(","))), Delim("]"))), bracketLevel)
   
   /** XML parsing methods */
   def parse(n : scala.xml.Node, name : GlobalName) : TextNotation = n match {
    case <text-notation/> =>
      val nameP = Path.parseS(utils.xml.attr(n,"name"), name)
      val precedence = utils.xml.attr(n, "precedence") match {
         case "" => Precedence.integer(0)
         case s => Precedence.parse(s)
      }
      val (fixityString, arguments) = {
         val markers = utils.xml.attr(n, "markers")
         // default: markers given directly
         if (markers != "") {
            val args = markers.split("\\s+").toList.filter(_!="")
            ("mixfix", args)
         } else {
            val fix = utils.xml.attr(n, "fixity")
            // temporary for legacy Twelf export (2013-05-30)
            val impl = utils.xml.attr(n, "implicit")
            if (impl != "")
               (fix, List(impl))
            // standard case: arbitrary fixity and arguments
            else {
               val args = utils.xml.attr(n,"arguments").split("\\s+").toList.filter(_!="")
               (fix,args)
            }
         }
      }
      val fixity = FixityParser.parse(name, fixityString, arguments)
      new TextNotation(name, fixity, precedence)
    case _ => throw ParseError("invalid notation:\n" + n)
  }
  
   /**
    * String parsing method
    *
    * the default precedence is 0; exception: if the notation contains (, it is above bracketLevel
    */
   def parse(str : String, name : GlobalName) : TextNotation = {
       var tokens = str.split("\\s+").toList.filter(_ != "")
       val i = tokens.indexOf("prec")
       val prec = if (i != -1) {
          val rest = tokens.drop(i)
          tokens = tokens.take(i)
          rest match {
               case _ :: p :: Nil => Precedence.parse(p)
               case _ => throw ParseError("precedence not found: " + str)
           }
       } else if (str.contains("("))
           Precedence.integer(-1000001)
       else
           Precedence.integer(0)
       val (fixityString, arguments) = if (! tokens.isEmpty && tokens.head.startsWith("%%")) {
          val fix = tokens.head.substring(2)
          val args = tokens.tail
          (fix, args)
       } else {
          ("mixfix", tokens)
       }
       val fixity = FixityParser.parse(name, fixityString, arguments)
       new TextNotation(name, fixity, prec)
  }
}

/** defines some implicit conversions to produce Markers */
object NotationConversions {
   /** integers are converted to argument markers */
   implicit def fromInt(n:Int) = Arg(n)
   /** strings are converted to delimiters */
   implicit def fromString(s:String) = Delim(s)
}

