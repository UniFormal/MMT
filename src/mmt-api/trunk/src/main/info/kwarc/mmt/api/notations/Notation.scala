package info.kwarc.mmt.api.notations

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
class TextNotation(val name: GlobalName, val fixity: Fixity, val precedence: Precedence, val meta: Option[MPath]) extends ComplexNotation {
   /** @return the list of markers used for parsing/presenting with this notations */
   lazy val markers: List[Marker] = fixity.markers
   /** @return the arity of this fixity */
   lazy val arity = {
      var args: List[ArgumentComponent] = Nil
      var vars : List[VariableComponent] = Nil
      var attrib : Int = 0
      //collect components from markers
      markers foreach {
         case a: ArgumentComponent =>
            args ::= a
         case v: VariableComponent =>
            vars ::= v
         case AttributedObject =>
            attrib += 1
         case _ =>
      }
      // sort by component number
      vars = vars.sortBy(_.number)
      args = args.sortBy(_.number)
      // args with all implicit argument components added
      var argsWithImpl: List[ArgumentComponent] = Nil
      var i = vars.lastOption.map(_.number).getOrElse(0) + 1 // the first expected argument position
      args foreach {a =>
         while (i < a.number) {
            //add implicit argument in front of the current one
            argsWithImpl ::= ImplicitArg(i)
            i += 1
         }
         argsWithImpl ::= a
         i += 1
      }
      args = argsWithImpl.reverse
      // subargs with all implicit argument components added
      // the last expected subarg position
      var lastSubArg = vars.headOption match {
         case Some(v) => v.number-1
         case None => args.headOption match {
            case Some(a) => a.number-1
            case None => 0
         }
      }
      val subargs = (0 until lastSubArg).toList.map {i => ImplicitArg(i+1)}
      //TODO: check all args.number < vars.number < scopes.numbers; no gaps in variables or scopes
/*    arguments are behind the variables now
      //add implicit arguments after the last one (the first variable tells us if there are implicit arguments after the last one)
      val totalArgs = vars.headOption.map(_.number).getOrElse(i) - 1
      while (i <= totalArgs) {
         argsWithImpl ::= ImplicitArg(i)
         i += 1
      }*/
      // the attribution
      val attribution = attrib > 0
      Arity(subargs, vars, args, attribution)
   }
   /** @return the list of markers that should be used for parsing */
   lazy val parsingMarkers = markers.filter {
      case _:PresentationMarker => false // there should not be any presentation markers in notations used for parsing
      case _:ImplicitArg => false // remove the implicit markers
      case AttributedObject => false // attributed variables are handles explicitly by variable parsing
      case _ => true
   }
   lazy val presentationMarkers = PresentationMarker.introducePresentationMarkers(markers)
   val key = NotationKey(Some(name), Role_application(None))
   val nset = name.module.toMPath
  
   def toText = {
      val (fixityString, argumentString) = fixity.asString
      val metaStr = meta.map("meta " + _.toPath).getOrElse("")
      val precStr = if (precedence != Precedence.integer(0)) " prec " + precedence.toString else ""
      val fixStr = if (fixityString == "mixifx") "" else "%%"+fixityString
      metaStr + precStr + fixStr + " " + argumentString
   }
   override def toString = toText + " (markers are: " + markers.map(_.toString).mkString(" ") + ")" 
   def toNode = {
     val (fixityString, argumentString) = fixity.asString
     <notation name={name.toPath} precedence={precedence.toString}
         meta={meta.map(_.toPath).getOrElse(null)} fixity={fixityString} arguments={argumentString}/>
   }

   /**
    * flattens and transforms markers into Presentation
    * @param args number of arguments
    * @param vars number of variables
    * @param scopes number of scopes
    * @return Presentation that can be rendered using the [[presentation.StyleBasedPresenter]]  
    */
   def presentation(vars: Int, args: Int, attrib: Boolean) = {
     val flatMarkers = arity.flatten(presentationMarkers, vars, args, attrib)
     val implicitsP = arity.flatImplicitArguments(args) flatMap {
        case i @ ImplicitArg(n) =>
           if (markers contains i)
              Nil // skip arguments that are explicitly placed by the notation
           else
              List(ArgSep(), Component(NumberedIndex(n), BracketInfo()))
     }
     var implicitsToDo = ! implicitsP.isEmpty
     /** translates a list of Markers into old-style presentation that can be handed off */
     def translateMarkers(ms: List[Marker], suppressBrackets: Boolean = false): List[Presentation] = {
        val numDelims = ms.count(_.isInstanceOf[Delimiter])
        var numDelimsSeen = 0
        ms.zipWithIndex map {case (m,i) => m match {
          case d : Delimiter =>
             numDelimsSeen += 1
             var delim : Presentation = Fragment("constant", PText(name.toPath), PText(d.text))
             if (implicitsToDo && numDelimsSeen == 1) {
                implicitsToDo = false
                // add all implicit arguments after the first delimiter
                delim = NoBrackets(delim + Fragment("implicit", PList(implicitsP)))
             }
             if (i+1<ms.length)
                delim += ArgSep()
             delim
          case Arg(p) =>
             val bi = if (suppressBrackets) BracketInfo(Some(Precedence.neginfinite))
                else { 
                   val delimitation = if (numDelimsSeen == 0) -1 else if (numDelimsSeen == numDelims) 1 else 0
                   BracketInfo(Some(precedence), Some(delimitation))
                }
             Component(NumberedIndex(p), bi)
          case ImplicitArg(n) =>
             val bi = if (suppressBrackets) BracketInfo(Some(Precedence.neginfinite))
                else { 
                   val delimitation = if (numDelimsSeen == 0) -1 else if (numDelimsSeen == numDelims) 1 else 0
                   BracketInfo(Some(precedence), Some(delimitation))
                }
             Fragment("implicit", Component(NumberedIndex(n), bi))
          case Var(n, _, None) => Component(NumberedIndex(n), BracketInfo())
          case AttributedObject =>
                Component(NumberedIndex(0), BracketInfo(Some(Precedence.neginfinite)))
          case SeqArg(n,sep) => throw ImplementationError("non-flat marker")
          case Var(n,_,Some(sep)) => throw ImplementationError("non-flat marker")
          case GroupMarker(ms) => aux(ms)
          case ScriptMarker(main, sup, sub, over, under) =>
             Fragment("scripted", aux(List(main)),
                                  aux(under.toList), aux(over.toList), aux(sub.toList), aux(sup.toList))
          case FractionMarker(a,b,_) =>
             Fragment("fraction", aux(a, true), aux(b, true))
          case TdMarker(a) => 
             Fragment("mtd", aux(a))
          case TrMarker(a) => 
             Fragment("mtr", aux(a))
          case TableMarker(a) => 
             Fragment("table", aux(a))
          case InferenceMarker => Compute(None, "infer")
        }}
     }
     /**
      * @param ms markers to translate
      * @param separated if true, inserts separator in between all translated markers
      * @param translated markers, separator if empty
      */
     def aux(ms: List[Marker], separated: Boolean = false) : Presentation = {
        val msT = translateMarkers(ms, true)
        msT match {
           case Nil => ArgSep()
           case hd::Nil => hd
           case l =>
              val msTS = if (separated) msT.head :: msT.tail.flatMap(p => List(ArgSep(), p)) else msT
              NoBrackets(PList(msTS))
        }
     }
     val tokens = translateMarkers(flatMarkers)
     PList(tokens)
   }
   // the first delimiter of this notation
   def firstDelimString : Option[String] = parsingMarkers mapFind {
      case d: Delimiter => Some(d.text)
      case SeqArg(_, Delim(s)) => Some(s)
      case _ => None
   }
   def openArgs(fromRight: Boolean) : Int = {
      var i = 0
      val ms = if (fromRight) parsingMarkers.reverse else parsingMarkers
      ms foreach {
         case a: Arg => i += 1
         case a: ImplicitArg =>
         case _:SeqArg => return i+1
         case _: Var => return i+1
         case AttributedObject =>
         case d: Delimiter => return i
         case _:PresentationMarker => // impossible
      }
      i
   }
   def isLeftOpen = openArgs(false) > 0
   def isRightOpen = openArgs(true) > 0
}

object TextNotation {
   def apply(name: GlobalName, prec: Precedence, meta: Option[MPath])(ms: Any*): TextNotation = {
      val markers : List[Marker] = ms.toList map {
         case i: Int => Arg(i)
         case m: Marker => m
         case s: String => Marker.parse(name, s)
      }
      new TextNotation(name, Mixfix(markers), prec, meta)
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
   val bracketNotation = new TextNotation(utils.mmt.brackets, Mixfix(List(Delim("("),Arg(1),Delim(")"))), bracketLevel, None)
   val contextNotation = new TextNotation(utils.mmt.context, Mixfix(List(Delim("["), Var(1,true,Some(Delim(","))), Delim("]"))), bracketLevel, None)
   
   /** XML parsing methods */
   def parse(n : scala.xml.Node, name : GlobalName) : TextNotation = n match {
    case <text-notation/> | <notation/> =>  // TODO text-notation is deprecated
      val nameP = Path.parseS(utils.xml.attr(n,"name"), name)
      val precedence = utils.xml.attr(n, "precedence") match {
         case "" => Precedence.integer(0)
         case s => Precedence.parse(s)
      }
      val meta = utils.xml.attr(n, "meta") match {
         case "" => None
         case s => Some(Path.parseM(s, name))
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
      new TextNotation(name, fixity, precedence, meta)
    case _ => throw ParseError("invalid notation:\n" + n)
  }
  
   /**
    * String parsing method
    *
    * the default precedence is 0; exception: if the notation contains (, it is above bracketLevel
    */
   def parse(str : String, name : GlobalName) : TextNotation = {
       var tokens = str.split("\\s+").toList.filter(_ != "")
       val meta = tokens match {
          case "meta" :: mt :: rest =>
             tokens = rest
             Some(Path.parseM(mt, name))
          case "meta" :: Nil =>
             throw ParseError("theory expected")
          case _ => None
       }
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
       new TextNotation(name, fixity, prec, meta)
  }
}

/** defines some implicit conversions to produce Markers */
object NotationConversions {
   /** integers are converted to argument markers */
   implicit def fromInt(n:Int) = Arg(n)
   /** strings are converted to delimiters */
   implicit def fromString(s:String) = Delim(s)
}

