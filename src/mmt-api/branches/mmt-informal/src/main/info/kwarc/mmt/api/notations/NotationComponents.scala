package info.kwarc.mmt.api.notations

import info.kwarc.mmt.api._
import presentation.Precedence

/** Objects of type Marker make up the pattern of a Notation */
sealed abstract class Marker {
   override def toString : String
}

/** Markers that are delimiters */
sealed abstract class Delimiter extends Marker {
   def text : String
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
 * @param s the delimiting String, %w for whitespace
 */
case class Delim(s: String) extends Delimiter {
   override def toString = {
      if (s == "%w") s
      else if (List('%', 'V').contains(s(0)) || s.endsWith("…"))
         "%D" + s
      else {
         try {s.toInt; "%D" + s}
         catch {case e:Throwable => s}
      }
   }
   def text = if (s == "%w") " " else s
}

/** special delimiters that are expanded to a string based on the declaration that the notation occurs in
 * 
 * These should never occur in notations that are used by the Scanner. They should be expanded first.
 */
abstract class PlaceholderDelimiter extends Delimiter

/**
 * expands to the name of the instance
 * 
 * only legal for notations within patterns
 */
case class InstanceName(path: GlobalName) extends PlaceholderDelimiter {
   override def toString = "%i"
   def text = if (path.name.length <= 1) "" else path.name.init.toPath
}

/**
 * expands to the name of the symbol
 * 
 * useful for repetitive notations that differ only in the name
 */
case class SymbolName(path: GlobalName) extends PlaceholderDelimiter {
   override def toString = "%n"
   def text = if (path.name.length < 1) "" else path.name.last.toPath
}

sealed abstract class ArgumentMarker extends Marker with ArgumentComponent

/** an argument
 * @param n absolute value is the argument position, negative iff it is in the binding scope
 */
case class Arg(number: Int, precedence : Option[Precedence] = None) extends ArgumentMarker {
   override def toString = number.toString
   def by(s:String) = SeqArg(number,Delim(s))
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
 * @param n absolute value is the argument position, negative iff it is in the binding scope
 * @param sep the delimiter between elements of the sequence 
 */
case class SeqArg(number: Int, sep: Delim, precedence : Option[Precedence] = None) extends ArgumentMarker {
   override def toString = number.toString + sep + "…"
   override def isSequence = true
}

/** a variable binding 
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
  def toParsing = Delim(word) :: Nil
}


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

/** a marker for type of the presented object */
case object InferenceMarker extends PresentationMarker {
   def flatMap(f: Marker => List[Marker]) = InferenceMarker
}

object PresentationMarker {
   private def splitOffOne(ms: List[Marker]) : (Marker,List[Marker]) = ms match {
      case Nil => (Delim(" "), Nil)
      case Delim("(") :: rest =>
         var i = 0
         var level = 1
         while (level > 0) {
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
      while (left != Nil) {
         left.head match {
            case Delim(w) if List("^", "_", "^^", "__") contains w =>
               if (sofar.isEmpty) sofar ::= Delim(" ")
               val scripted = sofar.head match {
                  case m: ScriptMarker => m
                  case m => ScriptMarker(m, None, None, None, None)
               }
               val (script, rest) = splitOffOne(left.tail)
               left = rest
               val newHead = w match {
                  case "^" => scripted.copy(sup = Some(script))
                  case "_" => scripted.copy(sub = Some(script))
                  case "^^" => scripted.copy(over = Some(script))
                  case "__" => scripted.copy(under = Some(script))
               } 
               sofar = newHead :: sofar.tail
            case Delim("/") =>
               if (sofar.isEmpty) sofar ::= Delim(" ")
               val enum = sofar.head
               val (denom, rest) = splitOffOne(left.tail)
               left = rest
               val newHead = FractionMarker(List(enum), List(denom), true)
               sofar = newHead :: sofar.tail
            case Delim("[&") => 
              get_until(List("&]"), left, true) match {
               case None => //end not found, ignoring
                 sofar ::= left.head
                 left = left.tail
               case Some((taken, end)) => 
                 val processed = introducePresentationMarkers(taken)
                 sofar ::= TdMarker(processed)
                   left = end
             }
            case Delim("[\\") => 
               get_until(List("\\]"), left, true) match {
               case None => //end not found, ignoring
                 sofar ::= left.head
                 left = left.tail
               case Some((taken, end)) => 
                 val processed = introducePresentationMarkers(taken)
                 sofar ::= TrMarker(processed)
                   left = end
             }
            case Delim("[[") =>
             get_until(List("]]"), left, true) match {
               case None => //end not found, ignoring
                 sofar ::= left.head
                 left = left.tail
               case Some((taken, end)) => 
                 val processed = introducePresentationMarkers(taken)
                 sofar ::= TableMarker(processed)
                   left = end
             }
            case m =>
               sofar ::= m
               left = left.tail
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
       Some(sofar, left)
     } else {
       None
     }
     
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
   def parse(name: GlobalName, s: String) = s match {
         case "%i" => InstanceName(name)
         case "%n" => SymbolName(name)
         case "%a" => AttributedObject
         case s: String if s.startsWith("%D") =>
            // Ds ---> delimiter s (used to escape special delimiters)
            if (s.length == 2)
               throw ParseError("not a valid marker " + s) 
            else
               Delim(s.substring(2))
         case s: String if s.startsWith("%I") =>
            // In ---> implicit argument
            try {
               val n = s.substring(2).toInt
               ImplicitArg(n)
            } catch {
               case e: Throwable => throw ParseError("not a valid marker " + s) 
            }
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
               Arg(n.abs) //TODO deprecate negative positions
            } catch {case e: Throwable =>
               //other string ---> delimiter
               Delim(s)
            }
         case m => throw ParseError("not a valid marker " + m.toString)
   }
}