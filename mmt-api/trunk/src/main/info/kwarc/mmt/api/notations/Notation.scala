package info.kwarc.mmt.api.notations

import info.kwarc.mmt.api._
import NotationConversions._
import utils._
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
case class TextNotation(fixity: Fixity, precedence: Precedence, meta: Option[MPath],
                        scope : NotationScope = NotationScope.default) extends metadata.HasMetaData {
   /** @return the list of markers used for parsing/presenting with this notations */
   lazy val markers: List[Marker] = fixity.markers
   /** @return the arity of this notation */
   lazy val arity: Arity = {
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
      val fixStr = if (fixityString == "mixfix") "" else " %%"+fixityString
      metaStr + fixStr + " " + argumentString + precStr
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
}

object TextNotation {
   /** helpful when constructing notations programmatically */
   def fromMarkers(prec: Precedence, meta: Option[MPath], scope : NotationScope = NotationScope.default)(ms: Any*): TextNotation = {
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
   def parse(n : scala.xml.Node, nsMap : NamespaceMap) : TextNotation = n.label match {
    case "notation" =>
      val precedence = utils.xml.attr(n, "precedence") match {
         case "" => Precedence.integer(0)
         case s => Precedence.parse(s)
      }
      val meta = utils.xml.attr(n, "meta") match {
         case "" => None
         case s => Some(Path.parseM(s, nsMap))
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
            val args = utils.xml.attr(n,"arguments").split("\\s+").toList.filter(_!="")
            (fix,args)
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
   def parse(str : String, nsMap : NamespaceMap) : TextNotation = {
       var tokens = str.split("\\s+").toList.filter(_ != "")
       val meta = tokens match {
          case "meta" :: mt :: rest =>
             tokens = rest
             Some(Path.parseM(mt, nsMap))
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
   
  /** true if both notations expect the exact same markers */
  def agree(left: TextNotation, right: TextNotation) = {
     left.parsingMarkers == right.parsingMarkers && left.parsingMarkers.forall {
        case _ : PlaceholderDelimiter => false
        case _ => true
     }
  }
}

/** defines some implicit conversions to produce Markers */
object NotationConversions {
   /** integers are converted to argument markers */
   implicit def fromInt(n:Int) = Arg(n)
   /** strings are converted to delimiters */
   implicit def fromString(s:String) = Delim(s)
}

