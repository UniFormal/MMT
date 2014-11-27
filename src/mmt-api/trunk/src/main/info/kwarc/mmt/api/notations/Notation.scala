package info.kwarc.mmt.api.notations

import info.kwarc.mmt.api._
import NotationConversions._
import utils.MyList._
import modules._
import symbols._
import objects._

case class InvalidNotation(msg: String) extends java.lang.Throwable

/**
 * scope where a notation is applicable
 * variant : optionally the name of this notation variant
 * languages : the languages where this notation is applicable (e.g. tex, mmt, lf, mathml)
 * priority : the priority of this notation when looking for a default notation
 */
case class NotationScope(variant : Option[String], languages : List[String], priority : Int) {
  def toNode =  <scope variant={variant.getOrElse(null)} 
    languages={languages.mkString(" ")} priority={priority.toString}/>
}

object NotationScope {
  def default = NotationScope(None, Nil, 0)
}

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
class TextNotation(val fixity: Fixity, val precedence: Precedence, val meta: Option[MPath],
                   val scope : NotationScope = NotationScope.default) extends metadata.HasMetaData {
   /** @return the list of markers used for parsing/presenting with this notations */
   lazy val markers: List[Marker] = fixity.markers
   /** @return the arity of this notation */
   lazy val arity = {
      // remove presentation markers
      val markersWithoutPres = markers.flatMap(PresentationMarker.flatten)
      // find first and last variable component
      val varPositions = markersWithoutPres.flatMap {
         case v: VariableComponent => List(v.number)
         case _ => Nil
      }
      val firstVar = if (varPositions.isEmpty) 0 else varPositions.min
      val lastVar  = if (varPositions.isEmpty) 0 else varPositions.max
      // collect components from markers
      var subs: List[ArgumentComponent] = Nil // arguments before firstVar
      var vars: List[VariableComponent] = Nil // variables (must be between firstVar and lastVar)
      var args: List[ArgumentComponent] = Nil // arguments after lastVar
      var attrib : Int = 0
      markersWithoutPres foreach {
         case a: ArgumentComponent =>
            if (a.number > lastVar) args ::= a
            else if (a.number < firstVar) subs ::= a
            else {
              throw ParseError("illegal components in notation " + this)
            }
         case v: VariableComponent =>
            vars ::= v
         case AttributedObject =>
            attrib += 1
         case _ =>
      }
      // note: now, if varPositions.isEmpty, then subs.isEmpty
      // sort all 3 lists by component number
      subs = subs.sortBy(_.number)
      vars = vars.sortBy(_.number)
      args = args.sortBy(_.number)
      // add implicit argument components into the gaps in args and subs
      def addImplicits(ac: List[ArgumentComponent], from: Int) = {
         var result: List[ArgumentComponent] = Nil
         var i = from
         ac foreach {a =>
            //add implicit arguments in front of a
            while (i < a.number) {
               result ::= ImplicitArg(i)
               i += 1
            }
            result ::= a
            i += 1
         }
         result.reverse
      }
      subs = addImplicits(subs, 1)
      args = addImplicits(args, lastVar+1)
      // add implicit arguments between subs and vars
      var lastSub = subs.lastOption.map(_.number).getOrElse(0)
      val implsBeforeVar = ((lastSub+1) until firstVar).toList.map {i => ImplicitArg(i)}
      subs = subs ::: implsBeforeVar 
      // the attribution
      val attribution = attrib > 0
      Arity(subs, vars, args, attribution)
   }
   /** @return the list of markers that should be used for parsing */
   lazy val parsingMarkers = markers flatMap {
      case _:PresentationMarker => Nil // there should not be any presentation markers in notations used for parsing
      case _:ImplicitArg => Nil // remove the implicit markers
      case AttributedObject => Nil // attributed variables are handled explicitly by variable parsing
      case v:VerbalizationMarker => v.toParsing
      case m => List(m)
   }
   lazy val presentationMarkers = PresentationMarker.introducePresentationMarkers(markers)
  
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
     <notation precedence={precedence.toString}
         meta={meta.map(_.toPath).getOrElse(null)} fixity={fixityString} 
         arguments={argumentString}> {scope.toNode} </notation>
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
         case _:VerbalizationMarker => //impossible
      }
      i
   }
   def isLeftOpen = openArgs(false) > 0
   def isRightOpen = openArgs(true) > 0

   /*
   val key = NotationKey(Some(name), Role_application(None))
   val nset = name.module.toMPath
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
        case i @ ImplicitArg(n,_) =>
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
          case Arg(p,_) =>
             val bi = if (suppressBrackets) BracketInfo(Some(Precedence.neginfinite))
                else { 
                   val delimitation = if (numDelimsSeen == 0) -1 else if (numDelimsSeen == numDelims) 1 else 0
                   BracketInfo(Some(precedence), Some(delimitation))
                }
             Component(NumberedIndex(p), bi)
          case ImplicitArg(n,_) =>
             val bi = if (suppressBrackets) BracketInfo(Some(Precedence.neginfinite))
                else { 
                   val delimitation = if (numDelimsSeen == 0) -1 else if (numDelimsSeen == numDelims) 1 else 0
                   BracketInfo(Some(precedence), Some(delimitation))
                }
             Fragment("implicit", Component(NumberedIndex(n), bi))
          case Var(n, _, None,_) => Component(NumberedIndex(n), BracketInfo())
          case AttributedObject =>
                Component(NumberedIndex(0), BracketInfo(Some(Precedence.neginfinite)))
          case SeqArg(n,sep,_) => throw ImplementationError("non-flat marker")
          case Var(n,_,Some(sep),_) => throw ImplementationError("non-flat marker")
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
   */
}

object TextNotation {
   def apply(prec: Precedence, meta: Option[MPath], scope : NotationScope = NotationScope.default)(ms: Any*): TextNotation = {
      val markers : List[Marker] = ms.toList map {
         case i: Int => Arg(i)
         case m: Marker => m
         case s: String => Marker.parse(s)
      }
      new TextNotation(Mixfix(markers), prec, meta, scope)
   }
   
   def parseScope(n : scala.xml.Node) : NotationScope = {
       //parsing scope
      val variant = utils.xml.attr(n, "variant") match {
        case "" => None
        case s => Some(s)
      }
      val languages = utils.xml.attr(n, "languages") match {
        case "" => Nil
        case s => s.split(" ").toList
      }
      val priority = utils.xml.attr(n, "priority") match {
        case "" => 0
        case s => s.toInt
      }
      NotationScope(variant, languages, priority)
   }
   
   /** XML parsing methods */
   def parse(n : scala.xml.Node, base : Path) : TextNotation = n.label match {
    case "text-notation" | "notation" =>  // TODO text-notation is deprecated
      val precedence = utils.xml.attr(n, "precedence") match {
         case "" => Precedence.integer(0)
         case s => Precedence.parse(s)
      }
      val meta = utils.xml.attr(n, "meta") match {
         case "" => None
         case s => Some(Path.parseM(s, base))
      }
      val scope = n.child.find(_.label == "scope") match {
        case None => NotationScope.default
        case Some(s) => parseScope(s)
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
      val fixity = FixityParser.parse(fixityString, arguments)
      new TextNotation(fixity, precedence, meta, scope)
    case _ => throw ParseError("invalid notation:\n" + n)
  }
  
   /**
    * String parsing method
    *
    * the default precedence is 0; exception: if the notation contains (, it is above bracketLevel
    */
   def parse(str : String, base : Path) : TextNotation = {
       var tokens = str.split("\\s+").toList.filter(_ != "")
       val meta = tokens match {
          case "meta" :: mt :: rest =>
             tokens = rest
             Some(Path.parseM(mt, base))
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
       val fixity = FixityParser.parse(fixityString, arguments)
       new TextNotation(fixity, prec, meta)
  }
}

/** defines some implicit conversions to produce Markers */
object NotationConversions {
   /** integers are converted to argument markers */
   implicit def fromInt(n:Int) = Arg(n)
   /** strings are converted to delimiters */
   implicit def fromString(s:String) = Delim(s)
}

