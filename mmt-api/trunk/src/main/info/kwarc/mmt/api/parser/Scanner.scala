package info.kwarc.mmt.api.parser.test

case class Ambiguous() extends java.lang.Throwable
case class Invalid() extends java.lang.Throwable

class Token

class TokenList(tokens: List[Token]) {
   def apply(n: Int) = tokens(n)
   val length = tokens.length
   def top: Token = tokens.head
}

case class TokenSlice(tokens: TokenList, start: Int, next: Int) { 
}

class Scanner(tl: TokenList, notations: List[Notation]) {
   private var seen = TokenSlice(tl, 0, 0)
   def top: Token = tl(seen.next)
   def current = TokenSlice(tl, seen.next, seen.next + 1)
   def pick: TokenSlice = {
      val s = seen
      seen = TokenSlice(tl, seen.next, seen.next)
      s
   }
   // the currently open notations, inner-most first
   private var active: List[ActiveNotation] = Nil
   /*
    * precondition: closable.reverse ::: ans == active
    * return condition:
    *   None no active notation can be applied
    *   Some(l) the prefix l.reverse of active can be closed, and then the next notation can be applied   
    */
   private def checkActive(ans: List[ActiveNotation], closable: List[ActiveNotation]) : Option[List[ActiveNotation]] = ans match {
      case Nil => None
      case an::rest => 
         if (an.applicable) {
            Some(closable)
         } else {
            if (an.closable) {
               checkActive(rest, an :: closable)
            } else {
               None
            }
         }
   }
   private def applyFirst {
      val toClose = active.head.apply
      if (toClose)
         active = active.tail
   }
   private def next {
      val activeApplicable = checkActive(active, Nil)
      activeApplicable match {
         case Some(closable) =>
            closable.reverseMap {an => 
                an.close
                active = active.tail
            }
            applyFirst
         case None =>
            val applicable = notations filter {a => a.applicable(this)}
            applicable match {
               case hd :: Nil =>
                  val an = hd.activate(this)
                  active ::= an
                  applyFirst
               case Nil =>
                  seen = seen.copy(next = seen.next + 1)
               case _ => throw Ambiguous() //some kind of ambiguity-handling here (maybe look for next delimiter)
            }
      }
      if (seen.next < tl.length)
        next
      else {
         //active notations must eat seen tokens and close
      }
   }
}


abstract class Marker
case class Delim(t: Token) extends Marker
case class Arg(n: Int) extends Marker
case class SqArg(n: Int, sep: Delim) extends Marker

abstract class Found
case class FoundDelim(delim: TokenSlice) extends Found
case class FoundArg(arg: TokenSlice, n: Int) extends Found
case class FoundSeqArg(seqarg: TokenSlice, delims: List[FoundDelim], args: List[FoundArg]) extends Found

class Notation {
   val markers: List[Marker]
   def applicable(scanner: Scanner): Boolean
   def activate(scanner: Scanner): ActiveNotation = {
      val an = new ActiveNotation(scanner, markers)
      an
   }
}

class ActiveNotation(scanner: Scanner, markers: List[Marker]) {
   private var found : List[Found] = Nil
   private var left : List[Marker] = markers
   private var remember : Int = 0

   def applicable: Boolean = {
      left match {
         case Arg(n) :: Delim(t) :: _ =>
            remember = n
            scanner.top == t
         case _ => false
      } 
   }
   def apply: Boolean = {
      found ::= FoundArg(scanner.pick, remember)
      left = left.tail
      found ::= FoundDelim(scanner.current)
      left = left.tail
      if (markers.isEmpty) {
         true
      } else {
         false
      }
   }
   def closable : Boolean = {
      left match {
         case Arg(n) :: Nil =>
            remember = n
            true
         case _ => false
      }
   }
   def close {
      found ::= FoundArg(scanner.pick, remember)
      left = left.tail
   }
}
