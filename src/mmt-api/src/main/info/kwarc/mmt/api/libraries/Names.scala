package info.kwarc.mmt.api.libraries
import info.kwarc.mmt.api._
import symbols._
import modules._
import objects._
import utils.MyList._

/** a possible completion
 * @param parent theory from which the resolved symbol is imported
 * @param completion name of the completed symbol 
 */
case class Completion(parent: MPath, completion: LocalName) {
   def path = parent ? completion
}

case class IncludeOption(from : MPath, to: MPath, name : LocalName) {
  def spath = from ? name  
}

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
            val names = t.domain.toList
            names.filter(_.toString.startsWith(partialName)).map(n => Completion(t.path, n))
      }
   }
   /** returns the list of possible completions of partialName imported from the theory/via the structure given by qualifiers */
   def resolve(home: Term, qualifiers: List[String], partialName: String)(implicit lib: Lookup) : List[Completion] = {
      val incls = lib.visible(home).toList
      if (qualifiers.isEmpty) {
         incls flatMap {i => lookIn(i, partialName)}
      } else {
         val name = LocalName(qualifiers map SimpleStep)
         lib.getO(home, name) match {
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

   def resolveIncludes(home : Term, name : String)(implicit lib : Library) : Option[List[IncludeOption]] = {
      val incls = lib.visible(home).toList
      val current = incls flatMap {i => 
        get(i) match {
         case None => Nil
         case Some(t) =>
            val names = t.domain.toList
            names.filter(_.toString == name)
        }
      }
      current match { 
        case Nil => 
          val allPaths = lib.getAllPaths.toList
          val options = allPaths flatMap {i =>
            get(OMMOD(i)) match {
              case None => Nil
              case Some(t) => 
                val names = t.domain.toList
                names.filter(_.toString == name).map(n => IncludeOption(i, home.toMPath, n))
            }
          }
          Some(options)
        case _ => None
      }
   }
}