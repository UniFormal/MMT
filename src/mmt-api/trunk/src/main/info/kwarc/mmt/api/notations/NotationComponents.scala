package info.kwarc.mmt.api.notations

import info.kwarc.mmt.api._
import utils._

/** Objects of type Marker make up the pattern of a Notation */
sealed abstract class Marker {
   override def toString : String
}

/** Markers that are delimiters */
sealed abstract class Delimiter extends Marker {
   def text : String
   /** the value of a delimiter may depend on the name of the operator that owns the notation
    *  expand eliminates such dependencies
    *  
    *  expansion is identity by default
    */
   def expand(name: ContentPath, alias: List[LocalName]) : Delimiter = this
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
case class Delim(s: String) extends Delimiter {
   override def toString = {
      if (s == "%w") s
      else if (List('%', 'V').contains(s(0)) || s.endsWith("…"))
         "%D" + s
      else {
         if (stringToInt(s).isDefined) "%D" + s else s
      }
   }
   def text = if (s == "%w") " " else s
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
 * expands to the name of the instance
 * 
 * only legal for notations within patterns
 */
case class InstanceName() extends PlaceholderDelimiter {
   override def toString = "%i"
   override def expand(path: ContentPath, alias: List[LocalName]) = Delim(if (path.name.length <= 1) "" else path.name.init.toPath)
}

/**
 * expands to the name of the symbol or an alias (whichever is the shortest) 
 * 
 * useful for repetitive notations that differ only in the name
 */
case class SymbolName() extends PlaceholderDelimiter {
   override def toString = "%n"
   override def expand(path: ContentPath, alias: List[LocalName]) = {
     val shortest = (path.name :: alias).sortBy(_.length).find(_.nonEmpty)
     Delim(shortest.map(_.toPath).getOrElse(""))
   }
}

sealed abstract class ArgumentMarker extends Marker with ArgumentComponent

/** an argument
  *
  * @param n absolute value is the argument position, negative iff it is in the binding scope
 */
sealed abstract class Arg extends ArgumentMarker {
  val number: Int
  val precedence : Option[Precedence]
   override def toString = number.toString
   def by(s:String) : SeqArg = SimpSeqArg(number,Delim(s))

}

case class SimpArg(number : Int, precedence : Option[Precedence] = None) extends Arg

case class LabelArg(number : Int, typed: Boolean, defined : Boolean, precedence : Option[Precedence] = None) extends Arg {
  override def toString = "L" + number.toString + (if (typed) "T" else "") + (if (defined) "D" else "")
  override def by(s:String) = LabelSeqArg(number,Delim(s),typed,defined)
}

/**
 * an implicit argument
 * 
 * usually these are not mentioned in the notation, but occasionally they have to be, e.g., when an implicit argument is the last argument
 */
case class ImplicitArg(number: Int, precedence : Option[Precedence] = None) extends ArgumentMarker {
   override def toString = "%I" + number
}

/** a sequence argument
  *
  * @param n absolute value is the argument position, negative iff it is in the binding scope
 * @param sep the delimiter between elements of the sequence 
 */
sealed abstract class SeqArg extends ArgumentMarker {
  val number: Int
  val sep: Delim
  def makeCorrespondingArg(n: Int): Arg
  val precedence : Option[Precedence]
  override def toString = number.toString + sep + "…"
   override def isSequence = true
}


case class SimpSeqArg(number : Int, sep : Delim, precedence : Option[Precedence] = None) extends SeqArg {
  def makeCorrespondingArg(n: Int) = SimpArg(n, precedence)
}

case class LabelSeqArg(number: Int, sep: Delim, typed : Boolean, defined : Boolean, precedence : Option[Precedence] = None) extends SeqArg {
  override def toString = "L" + number.toString + (if (typed) "T" else "") + (if (defined) "D" else "") + sep + "…"
  def makeCorrespondingArg(n: Int) = LabelArg(n, typed, defined, precedence)
}

/** a variable binding
  *
  * @param n the number of the variable
 * @param typed true if the variable carries a type (to be inferred if omitted)
 * @param sep if given, this is a variable sequence with this separator;
 *   for typed variables with the same type, only the last one needs a type 
 */
case class Var(number: Int, typed: Boolean, sep: Option[Delim], precedence : Option[Precedence] = None) extends Marker with VariableComponent {
   override def toString = "V" + number.toString + (if (typed) "T" else "") + (sep.map(_.toString + "…").getOrElse(""))
   override def isSequence = sep.isDefined
}

case object AttributedObject extends Marker {
   override def toString = "%a" 
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
}
/** groups a list of markers into a single marker */
case class GroupMarker(elements: List[Marker]) extends PresentationMarker {
   def flatMap(f: Marker => List[Marker]) = GroupMarker(elements flatMap f)
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
}
/** a marker for fractions */
case class FractionMarker(above: List[Marker], below: List[Marker], line: Boolean) extends PresentationMarker {
   def flatMap(f: Marker => List[Marker]) = {
      FractionMarker(above.flatMap(f), below.flatMap(f), line)
   }
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
case class RootMarker(content : List[Marker], index : List[Marker] = Nil) extends PresentationMarker {
  def flatMap(f : Marker => List[Marker]) = {
    RootMarker(content.flatMap(f),index)
  } 
}

/**a maker based on mathml label*/
case class LabelMarker(content: List[Marker], label : String) extends PresentationMarker {
  def flatMap(f : Marker => List[Marker]) = {
    LabelMarker(content.flatMap(f),label)
  }
}

/** a marker for a fixed numeric value */
case class NumberMarker(value : Delim) extends PresentationMarker {
  def flatMap(f : Marker => List[Marker]) = {
    NumberMarker(value)
  } 
}

/**Marker for Identifier in MathML -*/
case class IdenMarker(value: Delim) extends PresentationMarker {
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
case class GlyphMarker( src : Delim, alt: String = "Failed Loading") extends PresentationMarker{
  def flatMap(f: Marker => List[Marker]) = {
    GlyphMarker(src)
  }
}

case class TextMarker(text : Delim) extends PresentationMarker {
  def flatMap(f:Marker => List[Marker]) = {
    TextMarker(text)
  }
}

/** a marker for type of the presented object */
case object InferenceMarker extends PresentationMarker {
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
               val enum = sofar.head
               val (denom, rest) = splitOffOne(others)
               left = rest
               val newHead = FractionMarker(List(enum), List(denom), true)
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
   def isSequence: Boolean = false
   val precedence: Option[Precedence] //=None
}
sealed trait ArgumentComponent extends ArityComponent
sealed trait VariableComponent extends ArityComponent
//sealed trait ScopeComponent extends ArityComponent

object Marker {
   def parse(s: String) = s match {
         case "%i" => InstanceName()
         case "%n" => SymbolName()
         case "%a" => AttributedObject
         case s: String if s.startsWith("%D") =>
            // Ds ---> delimiter s (used to escape special delimiters)
            if (s.length == 2)
               throw ParseError("not a valid marker " + s) 
            else
               Delim(s.substring(2))
         case s: String if s.startsWith("%I") =>
            // In or Gn ---> implicit argument or implicit guard
            try {
               val n = s.substring(2).toInt
               ImplicitArg(n)
            } catch {
               case e: Throwable => throw ParseError("not a valid marker " + s) 
            }
         case s: String if s.startsWith("V") && s(1).isDigit =>
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
         case s: String if s.startsWith("L") && s(1).isDigit =>
           //On ---> OML
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
           if (d.endsWith("…")) {
             val sep = d.dropRight(1)
             LabelSeqArg(n,Delim(sep),typed,defined)
           } else LabelArg(n,typed,defined)
         case s: String if s.endsWith("…") && s(0).isDigit =>
            //nsep… ---> sequence argument/scope
            var i = 0
            while (s(i).isDigit) {i+=1}
            val n = s.substring(0, i).toInt
            val rem = s.substring(i,s.length-1)
            SimpSeqArg(n, Delim(rem))
         case "" => throw ParseError("not a valid marker")
         case s:String =>
            try {
               val n = s.toInt
               //n ---> arguments, no sequence
               SimpArg(n.abs) //TODO deprecate negative positions
            } catch {case e: Throwable =>
               //other string ---> delimiter
               Delim(s)
            }
         case m => throw ParseError("not a valid marker " + m.toString)
   }
}