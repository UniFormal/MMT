package info.kwarc.mmt.api.moc

import info.kwarc.mmt.api._

import scala.collection._
import scala.collection.immutable.{HashMap}


class PragmaticRefiner(pragTypes : Set[PragmaticChangeType]) {

  def detectPossibleRefinements(diff : StrictDiff) : Set[(PragmaticChange, Set[Int])] = {
    val matches = new mutable.HashSet[(PragmaticChange,Set[Int])]()

    val changes = diff.changes.zipWithIndex.toSet
    changes.subsets() foreach {indexedChSet =>
      // have to explicitly state more generic type due to
      // Set being invariant in its type parameter
      val chSet : Set[ContentChange] = indexedChSet.map(_._1)
      val chIndexes = indexedChSet.map(_._2)
      pragTypes map {pType =>
        if (pType.matches(chSet)) {
          val usedChanges = chIndexes.map(diff.changes(_)).toList
          pType.make(new StrictDiff(usedChanges)) match {
            case Some(ch) => matches += (ch -> chIndexes)
            case _ => throw ImplementationError("given refinement doesn't conform to pragmatic change type. " + pType.name + " with changes " + usedChanges)
          }
        }
      }
    }
    matches
  }

  private var enabledMatches : Set[(PragmaticChange, Set[Int])] = new immutable.HashSet[(PragmaticChange, Set[Int])]()

  private def setEnabledMatches(matches : Set[(PragmaticChange, Set[Int])]) = {
    enabledMatches = matches
  }

  def enrich(diff :  StrictDiff, withChanges : Option[List[String]] = None) : Diff = {
    val possibleMatches = detectPossibleRefinements(diff)
    val enabledMatches = withChanges match {
      case None => possibleMatches //default is to apply all possible refinements
      case Some(l) => possibleMatches.filter(p => l.contains(p._1.description))
    }
    val changes = diff.changes

    val tmp = enabledMatches.flatMap(_._2) //list of used indexes
    val usedIndexes = tmp.toSet //set of used indexes
    //ensure that used indexes are unique
    require(usedIndexes.size == tmp.size, "Error: each change must be used in at most one pragmatic refinement but duplicates found in " + tmp)
    val unusedChanges = changes.zipWithIndex filter {p => !usedIndexes.contains(p._2)}

    var newChanges : List[ContentChange] = unusedChanges.map(_._1) ++ enabledMatches.map(_._1)

    new Diff(newChanges)
  }

  def apply(diff : StrictDiff, withChanges : Option[List[String]] = None) : Diff = enrich(diff, withChanges)

}




