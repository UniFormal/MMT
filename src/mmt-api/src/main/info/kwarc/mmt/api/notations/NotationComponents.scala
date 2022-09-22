package info.kwarc.mmt.api.notations

import info.kwarc.mmt.api._
import utils._

/** Objects of type Marker make up the pattern of a Notation */
sealed abstract class Marker {
   /** for markers that contain nested markers, this should be overridden to return all leafs of the syntax tree */
   def atomicDescendants: List[Marker] = List(this)
}

/** Markers that are delimiters */
sealed abstract class Delimiter extends Marker {
   def text : String
   /** the value of a delimiter may depend on the name of the operator that owns the notation
    *  expand eliminates such dependencies
    */
   def expand(name: ContentPath, alias: List[LocalName]): Delim
}

/** helper object */
object Delimiter {
   /** matches any Delimiter */
   def unapply(m: Marker): Option[String] = m match {
      case d: Delimiter => Some(d.text)
      case _ => None
   }
}

/** a delimiter
 *
 * @param s the delimiting String, %w for whitespace
 */
case class Delim(s: String, associatesToLeft: Boolean = true) extends Delimiter {
   override def toString = {
      if (s == "%w") s
      else {
        val escape = !associatesToLeft || List('%', 'V').contains(s(0)) || s.endsWith("…") || stringToInt(s).isDefined
        if (escape) {
          val escapePref = if (associatesToLeft) "%D" else "%R"
          escapePref + s
        } else
          s
      }
   }
   def text = if (s == "%w") " " else s
   def expand(name: ContentPath, alias: List[LocalName]) = this
}

/** replace the default unapply method to ignore the argument with default value when pattern-matching */ 
object Delim {
  def unapply(m: Marker) = m match {
    case d: Delim => Some(d.s)
    case _ => None
  }
}

/**
 * special delimiters that use non-trivial implementations of expand
 *
 * These are only useful for presentation, not for parsing.
 */
abstract class PlaceholderDelimiter extends Delimiter {
   /** empty to make them useless for parsing unless expanded */
   def text = ""
}

/**
 * expands by prepending the instance name to a delimiter
 *
 * @param base delimiter independent of instance name
 */
case class InstanceName(base: Delim) extends PlaceholderDelimiter {
   override def toString = "%i" + base
   override def expand(path: ContentPath, alias: List[LocalName]) = {
     val simpleName = path.name.dropComplex
     val d = if (simpleName.length <= 1)
       ""
     else
       simpleName.init.toPath+"/"
     base.copy(s = d+base.s)
   }
}

/**
 * expands to the name of the symbol or an alias (whichever is the shortest)
 *
 * useful for repetitive notations that differ only in the name
 */
case class SymbolName() extends PlaceholderDelimiter {
   override def toString = "%n"
   override def expand(path: ContentPath, alias: List[LocalName]) = {
     val shortest = (path.name.dropComplex :: alias).sortBy(_.length).find(_.nonEmpty)
     Delim(shortest.map(_.toPath).getOrElse(""))
   }
}

object CommonMarkerProperties {
  def noProps = CommonMarkerProperties(None,None)
}
import CommonMarkerProperties._

/** bundles minor properties of markers to make the interfaces more robust */
case class CommonMarkerProperties(precedence: Option[Precedence], localNotations: Option[LocalNotationInfo]) {
  def *(remap: Int => Int) = copy(localNotations = localNotations.map(_ * remap))
  def asStringPrefix = {
    var p = ""
    localNotations.foreach(lni => p = "%L" + lni + "_")
    p
  }
}

object LocalNotationInfo {
  /** determines how an argument is turned into a theory */
  sealed abstract class Role(s: String) {
    override def toString = s 
  }
  /** argument already is a theory */ 
  case object Theory extends Role("")
  /** argument is morphism and we take the domain */ 
  case object Domain extends Role("d")
  /** argument is morphism and we take the codomain */ 
  case object Codomain extends Role("c")
}
/**
 * determines how different notations are to be used for parsing a particular argument
 * @param argument the argument that determines what notations to use
 * @param role how does that argument determine a set of notations
 * @param replace determines if new notations replace or extend existing ones  
 */
case class LocalNotationInfo(argument: Int, role: LocalNotationInfo.Role, replace: Boolean) {
  override def toString = s"${argument}${role.toString}${if (replace) "!" else ""}"
  def *(remap: Int => Int) = copy(argument = remap(argument))
}

/** a child of the term (as opposed to a delimiter) */
sealed abstract class ChildMarker extends Marker with ArityComponent {
  val number: Int
  val properties: CommonMarkerProperties
  def precedence = properties.precedence
}

/** an argument (sequnce) of the term (as opposed to a variable binding) */
sealed abstract class ArgumentMarker extends ChildMarker with ArgumentComponent {
  /** a copy of this marker with the field properties.localNotations set */
  def changeProperty(change: CommonMarkerProperties => CommonMarkerProperties): ArgumentMarker = this match {
    case am: SimpArg     => am.copy(properties = change(properties))
    case am: SimpSeqArg  => am.copy(properties = change(properties))
    case am: LabelArg    => am.copy(properties = change(properties))
    case am: LabelSeqArg => am.copy(properties = change(properties))
    case am: ImplicitArg => am.copy(properties = change(properties))
  }
  /** a copy of this marker with the field properties.localNotations set */
  def addLocalNotationInfo(lni: LocalNotationInfo): ArgumentMarker = changeProperty(_.copy(localNotations = Some(lni)))
}

/** a single argument; these may have an associated SeqArg */
sealed abstract class Arg extends ArgumentMarker {
  override def toString = properties.asStringPrefix + number.toString
  /**
   * @return the corresponding sequence
   * @param s the delimiter
   */
  def by(s:String) : SeqArg

  /** renumbers the markers, used for arity flattening of sequences */
  def *(remap: Int => Int): Arg = {
    this match {
      case am: SimpArg  => am.copy(number = remap(am.number), properties = am.properties*remap)
      case am: LabelArg => am.copy(number = remap(am.number), properties = am.properties*remap)
    }
  }
}

/** a sequence argument corresponding to an Arg */
sealed abstract class SeqArg extends ArgumentMarker {
  val sep: Delim
  def makeCorrespondingArg(n: Int, remap: Int => Int): Arg
  override def toString = properties.asStringPrefix + number.toString + sep + "…"
  override def isSequenceVia = Some(sep)
}

/** normal arguments */
case class SimpArg(number : Int, properties: CommonMarkerProperties = noProps) extends Arg {
  override def toString = properties.asStringPrefix + number
  def by(s:String): SimpSeqArg = SimpSeqArg(number,Delim(s), properties)
}

/**
 * an implicit argument
 *
 * usually these are not mentioned in the notation, but occasionally they have to be, e.g., when an implicit argument is the last argument
 */
case class ImplicitArg(number: Int, properties: CommonMarkerProperties = noProps) extends ArgumentMarker {
   override def toString = properties.asStringPrefix + "%I" + number
   def *(remap: Int => Int): ImplicitArg = copy(number = remap(number), properties = properties * remap)
}

/**
 * sequence of [[SimpArg]]
 * @param sep the separator
 */

case class SimpSeqArg(number: Int, sep: Delim, properties: CommonMarkerProperties) extends SeqArg {
  override def toString = properties.asStringPrefix + number.toString + sep + "…"
  def makeCorrespondingArg(n: Int, remap: Int => Int) = SimpArg(n, properties * remap)
}


/** bundles flags for properties of [[LabelArg]] and [[LabelSeqArg]]
 * @param typed types are required
 * @param defined definitions are required
 */
case class LabelInfo(typed: Boolean, defined: Boolean) {
  override def toString = (if (typed) "T" else "") + (if (defined) "D" else "")
}

object LabelInfo {
  def none = LabelInfo(false,false)
}

/** OML arguments, possibly with required type/definiens
 */
case class LabelArg(number : Int, info: LabelInfo, properties: CommonMarkerProperties = noProps) extends Arg {
  override def toString = properties.asStringPrefix + "L" + number.toString + (if (info.typed) "T" else "") + (if (info.defined) "D" else "")
  def by(s:String): LabelSeqArg = LabelSeqArg(number, Delim(s), info, properties)
}

/**
 * sequence of [[LabelArg]]
 * @param sep the separator
 */
case class LabelSeqArg(number: Int, sep: Delim, info: LabelInfo, properties: CommonMarkerProperties) extends SeqArg {
  override def toString = properties.asStringPrefix + "L" + number.toString + info.toString + sep + "…"
  def makeCorrespondingArg(n: Int, remap: Int => Int) = LabelArg(n, info, properties*remap)
  override def isSequence = true
}

/** a variable binding
 *
 * @param number the number of the variable
 * @param typed true if the variable carries a type (to be inferred if omitted)
 * @param sep if given, this is a variable sequence with this separator;
 *   for typed variables with the same type, only the last one needs a type
 */
case class Var(number: Int, typed: Boolean, sep: Option[Delim], properties: CommonMarkerProperties = noProps) extends ChildMarker with VariableComponent {
   override def toString = properties.asStringPrefix + "V" + number.toString + (if (typed) "T" else "") + (sep.map(_.toString + "…").getOrElse(""))
   override def isSequenceVia = sep
   def makeCorrespondingSingleVar(n: Int, remap: Int => Int) = Var(n, typed, None, properties*remap)
   def *(remap: Int => Int): Var = copy(number = remap(number), properties = properties * remap)
   def info = LabelInfo(typed, false)
}

/** Verbalization markers occur in verbalization notations
 */
sealed abstract class VerbalizationMarker extends Marker {
  def toParsing : List[Marker]
}

case class WordMarker(word : String) extends VerbalizationMarker {
  override def toString = word
  def toParsing = Delim(word) :: Nil
}

// TODO add toString method in presentation markers

/** PresentationMarker's occur in two-dimensional notations
 *
 *  They typically take other lists of markers as arguments, thus building a tree of markers.
 */
sealed abstract class PresentationMarker extends Marker {
   /** @return the same marker with all nested markers replaced according to a function */
   def flatMap(f: Marker => List[Marker]) : PresentationMarker
   /** the list of all markers that occur as children of this one (excluding delimiters) */
   def content: List[Marker]
   /** visits every marker once */
   override def atomicDescendants: List[Marker] = {
     val c = content
     if (c.isEmpty) List(this)
     else c.flatMap(_.atomicDescendants)
   }
}

sealed abstract class AtomicPresentationMarker extends PresentationMarker {
  def content = Nil
}

/** groups a list of markers into a single marker */
case class GroupMarker(content: List[Marker]) extends PresentationMarker {
   def flatMap(f: Marker => List[Marker]) = GroupMarker(content flatMap f)
}
/** decorates a marker with various scripts */
case class ScriptMarker(main: Marker, sup: Option[Marker], sub: Option[Marker],
                        over: Option[Marker], under: Option[Marker]) extends PresentationMarker {
   private def gMap(f: Marker => List[Marker])(m: Marker) = {
      val mF = f(m)
      if (mF.length == 1) mF.head else GroupMarker(mF)
   }
   def flatMap(f: Marker => List[Marker]) =
      ScriptMarker(gMap(f)(main), sup map gMap(f), sub map gMap(f), over map gMap(f), under map gMap(f))
   def content = main :: List(sup,sub,over,under).flatMap(_.toList)
}
/** a marker for fractions */
case class FractionMarker(above: List[Marker], below: List[Marker], line: Boolean) extends PresentationMarker {
   def flatMap(f: Marker => List[Marker]) = {
      FractionMarker(above.flatMap(f), below.flatMap(f), line)
   }
   def content = above:::below
}
trait PresentationMarkerWrappingMarkerList {
  def content: List[Marker]
  def foreach(f: Marker => Unit): Unit = {content foreach f}
}
/** a marker based on mathml mtd elements, representing table cells */
case class TdMarker(content : List[Marker]) extends PresentationMarker {
   def flatMap(f : Marker => List[Marker]) = {
     TdMarker(content.flatMap(f))
   }
}
/** a marker based on mathml mtd elements, representing table rows */
case class TrMarker(content : List[Marker]) extends PresentationMarker {
   def flatMap(f : Marker => List[Marker]) = {
     TdMarker(content.flatMap(f))
   }
}
/** a marker based on mathml mtd elements, representing tables */
case class TableMarker(content : List[Marker]) extends PresentationMarker {
   def flatMap(f : Marker => List[Marker]) = {
     TdMarker(content.flatMap(f))
   }
}

/**a marker representing the nth(index) root in mathml*/
case class RootMarker(argument : List[Marker], index : List[Marker] = Nil) extends PresentationMarker {
  def flatMap(f : Marker => List[Marker]) = {
    RootMarker(content.flatMap(f),index)
  }
  def content = argument ::: index
}

/**a maker based on mathml label*/
case class LabelMarker(content: List[Marker], label : String) extends PresentationMarker {
  def flatMap(f : Marker => List[Marker]) = {
    LabelMarker(content.flatMap(f),label)
  }
}

/** a marker for a fixed numeric value */
case class NumberMarker(value : Delim) extends AtomicPresentationMarker {
  def flatMap(f : Marker => List[Marker]) = {
    NumberMarker(value)
  }
}

/**Marker for Identifier in MathML -*/
case class IdenMarker(value: Delim) extends AtomicPresentationMarker {
  def flatMap(f : Marker => List[Marker]) = {
    IdenMarker(value)
  }
}

/**Marker for error elements in MathML*/
case class ErrorMarker(content: List[Marker]) extends PresentationMarker{
  def flatMap(f: Marker => List[Marker]) = {
    ErrorMarker(content)
  }
}

/**Marker for phantom elements in MathML*/
case class PhantomMarker(content : List[Marker]) extends PresentationMarker{
  def flatMap(f: Marker => List[Marker]) = {
    PhantomMarker(content.flatMap(f))
  }
}
/** a marker for mglyph, to load non-standard symbols
  *
  *  @param src the source (link) of the symbol
 *  @param alt the text to show in case of failure
 *  */
case class GlyphMarker( src : Delim, alt: String = "Failed Loading") extends AtomicPresentationMarker {
  def flatMap(f: Marker => List[Marker]) = {
    GlyphMarker(src)
  }
}

case class TextMarker(text : Delim) extends AtomicPresentationMarker {
  def flatMap(f:Marker => List[Marker]) = {
    TextMarker(text)
  }
}

/** a marker for type of the presented object */
case object InferenceMarker extends AtomicPresentationMarker {
  def flatMap(f: Marker => List[Marker]) = InferenceMarker
}

object PresentationMarker {
   private def makeOne(ms : List[Marker]) : Marker = ms match {
     case hd :: Nil => hd
     case l => GroupMarker(l)
   }

   private def splitOffOne(ms: List[Marker]) : (Marker,List[Marker]) = ms match {
      case Nil => (Delim(" "), Nil)
      case Delim("(") :: rest =>
         var i = 0
         var level = 1
         while (level > 0) {
           //TODO handles this better, see if still actually needed
           if (i >= rest.length) { //could not find closing bracket => assuming bracket is just a delim
             return (Delim("("),rest)
           }
            rest(i) match {
               case Delim("(") => level +=1
               case Delim(")") => level -=1
               case m =>
            }
            i += 1
         }
         val rem = rest.drop(i)
         if (i == 2) (rest(1), rem)
         else {
            val group = introducePresentationMarkers(rest.take(i-1))
            (GroupMarker(group), rem)
         }
      case m :: rest => (m, rest)
   }
   def introducePresentationMarkers(ms: List[Marker]) : List[Marker] = {
      var sofar: List[Marker] = Nil
      var left : List[Marker] = ms
      var isRootProcessed = true
      while (left != Nil) {
         val (first, others) = splitOffOne(left)
         first match {
            case Delim(w) if List("^", "_", "^^", "__") contains w =>
               if (sofar.isEmpty) sofar ::= Delim(" ")
               val scripted = sofar.head match {
                  case m: ScriptMarker => m
                  case m => ScriptMarker(m, None, None, None, None)
               }
               val (script, rest) = splitOffOne(others)
               val pscript = makeOne(introducePresentationMarkers(List(script)))
               left = rest
               val newHead = w match {
                  case "^" => scripted.copy(sup = Some(pscript))
                  case "_" => scripted.copy(sub = Some(pscript))
                  case "^^" => scripted.copy(over = Some(pscript))
                  case "__" => scripted.copy(under = Some(pscript))
               }
               sofar = newHead :: sofar.tail
            case Delim("/") =>
               if (sofar.isEmpty) sofar ::= Delim(" ")
               val `enum` = sofar.head
               val (denom, rest) = splitOffOne(others)
               left = rest
               val newHead = FractionMarker(List(`enum`), List(denom), true)
               sofar = newHead :: sofar.tail
            case Delim("[&") =>
              get_until(List("&]"), others, true) match {
               case None => //end not found, ignoring
                 sofar ::= first
                 left = others
               case Some((taken, end)) =>
                 val processed = introducePresentationMarkers(taken)
                 sofar ::= TdMarker(processed)
                   left = end
             }
            case Delim("[\\") =>
               get_until(List("\\]"), others, true) match {
               case None => //end not found, ignoring
                 sofar ::= first
                 left = others
               case Some((taken, end)) =>
                 val processed = introducePresentationMarkers(taken)
                 sofar ::= TrMarker(processed)
                 left = end
             }
            case Delim("[[") =>
             get_until(List("]]"), others, true) match {
               case None => //end not found, ignoring
                 sofar ::= first
                 left = others
               case Some((taken, end)) =>
                 val processed = introducePresentationMarkers(taken)
                 sofar ::= TableMarker(processed)
                 left = end
             }
            case Delim("√") =>
              val (arg, rest) = splitOffOne(others)
              if(isRootProcessed){
                left = rest
                sofar ::= RootMarker(introducePresentationMarkers(List(arg)))
              }
              else{
                left = rest
                sofar = RootMarker(introducePresentationMarkers(List(arg)),List(sofar.head)) :: sofar.tail
                isRootProcessed = true
              }
            case Delim("//*") =>
              get_until(List("*//"),others,true) match {
                case None =>
                  sofar ::= first
                  left = others
                case Some((taken,end))=>
                  val processed = introducePresentationMarkers(taken)
                  sofar ::= PhantomMarker(processed)
                  left = end
              }

            /*FR: I'm removing this case because ' is too important as a delimiter.
             In general, special markers must not clash with reasonable delimiters.
             The weirder ones should all start with some escape character.
            case Delim("'") => // root will be '(root)√(base)
              val (arg,rest) = splitOffOne(others)
              left = rest
              sofar ::= makeOne(introducePresentationMarkers(List(arg)))
              isRootProcessed = false
              */

            case Delim(w) if w.startsWith("#num_") => //mathml number
              left = others
              sofar ::= NumberMarker(Delim(w.substring(5)))

            case Delim(w) if w.startsWith("#id_") => //mathml identifier
              left = others
              sofar ::= IdenMarker(Delim(w.substring(4)))
              // [tdMarker].label(LABEL)
            case Delim(w) if w.startsWith(".label(") => //TODO: support markers as labels
              if(w.endsWith(")"))
                sofar.head match {
                case td : TdMarker =>
                  sofar = LabelMarker(List(sofar.head),w.substring(7).dropRight(1)) :: sofar.tail
                case notEnding =>
                  sofar ::= first
                }
              else
                sofar ::= Delim(" ")
              left = others

            case Delim(w) if w.startsWith("#glyph_") =>
              left = others
              sofar ::= GlyphMarker(Delim(w.substring(7)))

            case Delim("err_") =>
              val (content,lrest) = splitOffOne(others)
              left = lrest
              sofar ::= ErrorMarker(List(content))

            case Delim(w) if w.startsWith("/*") && w.endsWith("*/") =>{
              left = others
              val second = w.substring(2).dropRight(2)
              sofar ::= TextMarker(Delim(second))
            }
            case m =>
               sofar ::= m
               left = others
         }
      }
      sofar.reverse
   }

   def get_until(delims : List[String], markers : List[Marker], dropLast : Boolean = false) : Option[(List[Marker], List[Marker])] = {
     var left = markers
     var sofar : List[Marker] = Nil
     var found = false
     while (left != Nil && !found) {
       left.head match {
         case Delim(s) if delims.contains(s) =>
           found = true
           if (dropLast)
             left = left.tail
         case _ =>
           sofar ::= left.head
           left = left.tail
       }
     }
     if (found) {
       Some(sofar.reverse, left)
     } else {
       None
     }
   }

   /** recursively replaces all presentation markers with their list of children and flattens the whole list
     *
     *  @return all non-presentation markers in m in traversal order
    */
   def flatten(m: Marker): List[Marker] = m match {
      case p: PresentationMarker =>
         var res: List[Marker] = Nil
         p flatMap {m =>
            val ms = flatten(m)
            res = res ::: ms
            Nil
         }
         res
      case m => List(m)
   }
}
/**
 * see [[Arity]]
 *
 * Almost all ArityComponents arise as the non-Delimiter NotationMarkers
 */
sealed trait ArityComponent {
   val number: Int
   def isSequenceVia: Option[Delim] = None
   def isSequence: Boolean = isSequenceVia.isDefined
   def precedence: Option[Precedence] //=None
}
sealed trait ArgumentComponent extends ArityComponent
sealed trait VariableComponent extends ArityComponent
//sealed trait ScopeComponent extends ArityComponent

object Marker {
   /** checks if s(i) exists and satisfies p; negative i counts from end */
   private def charAtIs(s: String, i: Int, p: Char => Boolean) = {
     charAt(s,i).map(p).getOrElse(false)
   }

   import StringSplit.At2
   def parse(s: String) : Marker = s match {
         case At2("%L", _) =>
           // %L`i[d|c][!]_m resulting in LocalNotationInfo(i,[d|c],[!]) added to marker m, i.e., previous argument i determines notations to use for argument m
           var i = 2
           while (charAtIs(s, i, _.isDigit)) {i+=1}
           val n = s.substring(2,i).toInt
           val role = charAt(s,i) match {
             case Some('d') =>
               i += 1
               LocalNotationInfo.Domain
             case Some('c') =>
               i += 1               
               LocalNotationInfo.Codomain
             case _ =>
               LocalNotationInfo.Theory
           }
           val replace = if (charAt(s,i) contains '!') {i += 1; true} else false
           if (! (charAt(s,i) contains '_')) throw ParseError(s"'_' expected at position $i: $s")
           val lni = LocalNotationInfo(n, role, replace)
           parse(s.substring(i+1)) match {
             case am: ArgumentMarker =>
               if (am.number < n)
                 throw ParseError(s"local notations for argument ${am.number} must be obtained from previous argument, not argument $n: $s")
               am.addLocalNotationInfo(lni)
             case _ => throw ParseError("only argument markers can use local notations: " + s)
           }
         case At2("%i",rest) =>
           parse(rest) match {
             case d: Delim => InstanceName(d)
             case _ => throw ParseError("%i must be followed by plain delimiter: " + s)
           }
         case "%n" => SymbolName()
         case s if s.startsWith("%D") || s.startsWith("%R") =>
            // Ds ---> delimiter s (used to escape special delimiters)
            // Rs ---> same as Ds but associates to the right
            if (s.length == 2)
               throw ParseError("not a valid marker " + s)
            else {
               val assocLeft = s(1) == 'D' 
               Delim(s.substring(2), assocLeft)
            }
         case At2("%I", rest) =>
            // In or Gn ---> implicit argument or implicit guard
            val n = stringToInt(rest).getOrElse(throw ParseError("not a valid marker " + s))
            ImplicitArg(n,noProps)
         case s if s.startsWith("V") && charAtIs(s, 1, _.isDigit) =>
            //Vn ---> variable
            var i = 1
            while (charAtIs(s, i, _.isDigit)) {i+=1}
            val n = s.substring(1,i).toInt
            val d = s.substring(i)
            if (d == "")
               //Vn ---> untyped, no sequence
               Var(n, false, None, noProps)
            else if (d == "T")
               //VnT ---> typed, no sequence
               Var(n, true, None, noProps)
            else if (d.startsWith("T") && d.endsWith("…")) {
               //VnTsep… ---> typed, sequence
               val sep = d.substring(1,d.length-1)
               Var(n, true, Some(Delim(sep)),noProps)
            } else if (d.endsWith("…")) {
               //Vnsep… ---> untyped, sequence
               val sep = d.substring(0,d.length-1)
               Var(n, false, Some(Delim(sep)),noProps)
            } else
               throw ParseError("not a valid marker " + s)
         case s if s.startsWith("L") && charAtIs(s,1,_.isDigit) =>
           //Ln ---> OML
           var i = 1
           while (i < s.length && s(i).isDigit) {i+=1}
           val n = s.substring(1,i).toInt
           var d = s.substring(i)
           var typed = false
           var defined = false
           if (d.startsWith("T")) {
             d = d.substring(1)
             typed = true
           }
           if (d.startsWith("D")) {
             d = d.substring(1)
             defined = true
           }
           val li = LabelInfo(typed,defined)
           if (d.endsWith("…")) {
             if (d.startsWith("d")) {
               throw ParseError("'d' not supported anymore, use %B at the beginning of the marker instead")
             }
             val sep = d.dropRight(1)
             LabelSeqArg(n,Delim(sep),li,noProps)
           } else {
             LabelArg(n,li,noProps)
           }
         case s if s.endsWith("…") && charAtIs(s,0,_.isDigit) =>
            //nsep… ---> sequence argument/scope
            var i = 0
            while (charAtIs(s,i,_.isDigit)) {i+=1}
            val n = s.substring(0, i).toInt
            val rem = s.substring(i,s.length-1)
            SimpSeqArg(n, Delim(rem),noProps)
         case s if charAtIs(s,0,_.isDigit) =>
            // number --> argument
            try {
               val n = s.toInt
               if (n <= 0) {
                 throw ParseError("not a valid marker: " + s.toString)
               }
               //n ---> arguments, no sequence
               SimpArg(n.abs,noProps)
            } catch {case e: Exception =>
               //other string ---> delimiter
              throw ParseError("not a valid marker")
            }
         case "" => throw ParseError("not a valid marker")
         case m => Delim(s)  //other string ---> delimiter
   }
}
