package info.kwarc.mmt.api.moc

import info.kwarc.mmt.api._

import scala.collection._
import scala.collection.immutable.{HashMap}


class PragmaticRefiner(pragTypes : Set[PragmaticChangeType]) {
  
  def detectPossibleRefinements(diff : StrictDiff) : Map[PragmaticChangeType, Set[Set[Int]]] = {
    val matches = new mutable.HashMap[PragmaticChangeType,mutable.HashSet[Set[Int]]]()
    
    val changes = diff.changes.zipWithIndex.toSet
    changes.subsets foreach {indexedChSet =>       
      // have to explicitly state more generic type due to 
      // Set being invariant in its type parameter 
      val chSet : Set[ContentChange] = indexedChSet.map(_._1)
      val chIndexes = indexedChSet.map(_._2)
      pragTypes map {pType =>
        if (pType.matches(chSet)) {
          if (!matches.isDefinedAt(pType)) {
            matches(pType) = new mutable.HashSet[Set[Int]]
          }
          matches(pType) += chIndexes
        } 
      }
    }
    matches
  }
  
  private var enabledMatches : Map[PragmaticChangeType, Set[Set[Int]]] = new HashMap[PragmaticChangeType, Set[Set[Int]]]()
  
  def setEnabledMatches(matches : Map[PragmaticChangeType, Set[Set[Int]]]) = {
    enabledMatches = matches
  }
  
  def enrich(diff :  StrictDiff, forceAllApplicable : Boolean = false) : Diff = {
    if (forceAllApplicable) {
      setEnabledMatches(detectPossibleRefinements(diff))
    }
    
    val changes = diff.changes
    
    val tmp = enabledMatches.flatMap(_._2).flatten
    
    val usedIndexes = tmp.toSet
    require(usedIndexes.size == tmp.size, "Error: each change must be used in at most one pragmatic refinement but duplicates found in " + tmp)
    
    val unusedChanges = changes.zipWithIndex filter {p => !usedIndexes.contains(p._2)}
    
    var newChanges : List[ContentChange] = unusedChanges.map(_._1) 
    
    enabledMatches foreach {p =>
      val ptype = p._1
      p._2 map {indexes =>
        val usedChanges = indexes.map(changes(_))
        ptype.make(new StrictDiff(usedChanges.toList)) match {
          case Some(ch) => newChanges ::= ch
          case None => throw ImplementationError("given refinement doesn't conform to pragmatic change type. " + ptype.name + " with changes " + usedChanges)
        }
      }
      
    }
   
    new Diff(newChanges)
  }
  
  def apply(diff : StrictDiff, forceAllApplicable : Boolean = false) : Diff = enrich(diff, forceAllApplicable)

}




