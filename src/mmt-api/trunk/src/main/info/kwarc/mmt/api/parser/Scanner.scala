package info.kwarc.mmt.api.parser.test

case class Ambiguous() extends java.lang.Throwable
case class Invalid() extends java.lang.Throwable

class Token

class TokenList(tokens: List[Token]) {
   def apply(n: Int) = tokens(n)
   val length = tokens.length
   def top: Token = tokens.head
}

case class TokenSlice(tokens: TokenList, start: Int, end: Int) { 
}

class Scanner(tl: TokenList, notations: Unit => List[ActiveNotation]) {
   private var seen = TokenSlice(tl, 0, 0)
   def top: Token = tl(seen.end)
   def pick: TokenSlice = {
      val s = seen
      seen = TokenSlice(tl, seen.end, seen.end)
      s
   }
   private var active: List[List[ActiveNotation]] = List(notations())
   def pushActive(n: ActiveNotation) {
      active ::= n :: notations()
   }
   def popActive {
      active = active.tail
   }
   def next {
      val t = tl(seen.end)
      val applicable = active.head filter {a => a.applicable(this)}
      applicable match {
         case hd :: Nil =>
            applicable.head.apply(this)
            next
         case Nil =>
            seen = seen.copy(end = seen.end + 1)
            next
         case _ => throw Ambiguous() //some kind of ambiguity-handling here (maybe look for next delimiter)
      }
   }
}


abstract class Marker
case class Delim(t: Token) extends Marker
case class Arg(n: Int) extends Marker
case class SqArg(n: Int, sep: Delim) extends Marker

abstract class Found
case class FoundDelim(delim: TokenSlice) extends Found
case class FoundArg(arg: TokenSlice) extends Found
case class FoundSeqArg(seqarg: TokenSlice, delims: List[FoundDelim], args: List[FoundArg]) extends Found

class ActiveNotation(markers: List[Marker]) {
   private var found : List[Found] = Nil
   private var left : List[Marker] = markers
   private var first = true

   def applicable(scanner: Scanner): Boolean = {
      scanner.top == left.head 
   }
   def apply(scanner: Scanner) {
      if (first) {
         scanner.pushActive(this)
         first = false
      }
      //found = FoundDelim(scanner) 
      left = left.tail
      found ::= FoundArg(scanner.pick)
      if (markers.isEmpty) {
         //apply notation here
         scanner.popActive
      }
   }
}

