package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.utils._

/*
sealed abstract class SynRole
case object MathObject extends SynRole
case class Binder(bvars : ArgList[BVar], keys : List[Obj], ret : SynRole) extends SynRole
case object Key extends SynRole
case class FuncSynRole(args : ArgList[SynRole], ret : SynRole)

sealed case class BVar(keys : List[Obj])

sealed abstract class ArgList[A]
case class One[A](r: SynRole) extends ArgList[A]
/** greedy star operator */
case class Repetition[A](r: SynRole) extends ArgList[A]

*/
/*
 FR: Universes are deprecated

sealed abstract class Universe(override val toString : String)
case class Individual(kind : Option[String]) extends Universe(kind.getOrElse(""))
case object Proof extends Universe("proof")
case object Judgment extends Universe("judgment")
case object Error extends Universe("error")
case object Binder extends Universe("binder")
case object Key extends Universe("key")

abstract class UnivCheck {
   def apply(that : Universe) : Option[String]
}
case class IsEqualTo(it : Universe) extends UnivCheck {
   def apply(that : Universe) =
      if (it == that) None else Some("expected " + it + "; found " + that)
}
case object IsSemantic extends UnivCheck {
   def apply(that : Universe) =
      if (List(Error,Binder,Key).exists(_ == that))
         Some("expected semantic role; found " + that)
      else None
}

/** helper object for universes */
object Universe {
   /** parses a universe from a string */
   def parse(s : String) : Universe = s match {
      case "proof" | "axiom" | "theorem" => Proof
      case "judgment" => Judgment
      case "error" => Error
      case "binder" => Binder
      case "key" | "attribution" | "semantic-attribution" => Key
      case "" => Individual(None)
      case s => Individual(Some(s))
   }
}

*/