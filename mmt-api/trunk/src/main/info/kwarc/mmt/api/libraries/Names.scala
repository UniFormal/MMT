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
// we assume includes have been flattened
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
            val names = t.valueList mapPartial {d => if (d.name.isAnonymous) None else Some(d.name)}
            names.filter(_.toString.startsWith(partialName)).map(n => Completion(t.path, n))
      }
   }
   /** returns the list of possible completions of partialName imported from the theory/via the structure given by qualifiers */
   def resolve(home: Term, qualifiers: List[String], partialName: String)(implicit lib: Lookup) : List[Completion] = {
      val incls = lib.visible(home).toList
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
   /** resolves the unqualified identifier name in the theory home */
   def resolve(home: Term, name: String)(implicit lib: Lookup) : Option[ContentElement] = {
      { 
         lib.getO(home % name)  // symbol in the current theory
      } orElse {
         home match {
            case OMMOD(p) => lib.getO(p.parent ? name) // module in the namespace of the current theory
            case _ => None
         }
      } orElse {
         val incls = lib.visible(home).toList
         val es = incls mapPartial {i => lib.getO(i % name)}
         if (es.length == 1) Some(es(0)) else None  // uniquely resolvable symbol in an included theory
      }
   }
}