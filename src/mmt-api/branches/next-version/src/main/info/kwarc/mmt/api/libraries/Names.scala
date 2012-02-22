package info.kwarc.mmt.api.libraries
import info.kwarc.mmt.api._
import symbols._
import modules._
import objects._

case class Completion(parent: Path, completions: scala.collection.Set[LocalName])

/** Auxiliary methods for name lookup */
object Names {
   def get(t: Term)(implicit lib: Lookup) : DeclaredTheory = t match {
      case OMMOD(p) => lib.get(p) match {
         case th: DeclaredTheory => th
         case _ => throw GetError("name resolution only implemented for declared theories")
      }
      case _ => throw GetError("name resolution only implemented for atomic theories")
   }                                 // TODO home was TheoryObj
   def lookIn(home: Term, n: List[String])(implicit lib: Lookup) : List[Completion] = {
      val t = get(home)
      if (n.isEmpty)
         return List(Completion(t.path, t.domain))
      val name = LocalName(n map NamedStep)
      lib.getO(home % name) match {
         case None => Nil
         case Some(c : Constant) => List(Completion(c.path, Set.empty))
         case Some(a : Alias) => List(Completion(a.path, Set.empty))
         case Some(l : DefinitionalLink) => List(Completion(l.path, get(l.from).domain))
      }
   }                                  // TODO home was TheoryObj
   def resolve(home: Term, n: List[String])(implicit lib: Lookup) : List[Completion] = {
      val r = lookIn(home, n)
      if (! r.isEmpty) return r
      val hd :: tl = n
      val incls = lib.importsTo(home).toList
      val inclsP = incls filter {
         case OMMOD(p) => p.last == hd
         case _ => false
      }
      if (! inclsP.isEmpty) {
         val rs = inclsP.flatMap(lookIn(_, tl))
         if (! rs.isEmpty) return rs
      }
      val rs = incls.flatMap(lookIn(_, n))
      rs
   }
}