package info.kwarc.mmt.api.libraries
import info.kwarc.mmt.api._
import symbols._
import modules._
import objects._
import utils.MyList._

/** a possible completion
 * @parent theory from which the resolved symbol is imported
 * @completion name of the completed symbol 
 */
case class Completion(parent: Path, completion: LocalName)

/** Auxiliary methods for name lookup */
object Names {
   private def get(t: Term)(implicit lib: Lookup) : Option[DeclaredTheory] = t match {
      case OMMOD(p) => lib.get(p) match {
         case th: DeclaredTheory => Some(th)
         case _ => None
      }
      case _ => None
   }
   private def lookIn(home: Term, partialName: String)(implicit lib: Lookup) : List[Completion] = {
      get(home) match {
         case None => Nil
         case Some(t) =>
            val names = t.valueList flatMap {
               case i: Include => Nil
               case d => List(d.name)
            }
            names.filter(_.flat.startsWith(partialName)).map(n => Completion(t.path, n))
      }
   }
   /** returns the list of possible completions of partialName imported from the theory/via the structure given by qualifiers */
   def resolve(home: Term, qualifiers: List[String], partialName: String)(implicit lib: Lookup) : List[Completion] = {
      val incls = lib.importsTo(home).toList
      if (qualifiers.isEmpty) {
         incls flatMap {i => lookIn(i, partialName)}
      } else {
         val name = LocalName(qualifiers map NamedStep)
         lib.getO(home % name) match {
            case Some(l : Structure) => resolve(l.from, Nil, partialName)
            case Some(_) => Nil
            case None =>
               val hd :: tl = qualifiers
               val inclsP = incls filter {
                  case OMMOD(p) => p.last == hd
                  case _ => false
               }
               inclsP flatMap {i => resolve(i, tl, partialName)}
         }
      }
   }
}