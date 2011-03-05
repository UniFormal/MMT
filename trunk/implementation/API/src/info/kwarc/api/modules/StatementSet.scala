package jomdoc.modules
import jomdoc._
import scala.xml.Node
import jomdoc.libraries._
import jomdoc.symbols._
import jomdoc.utils._

/**
 * StatementSet represents the content of modules, i.e., a set of statements.
 * 
 * It provides (i) a name-indexed hash map of symbol level Declarations, and a list of Imports.
 * It is mixed into theories and links to hold the symbols/imports and assignments/imports, respectively.
 * 
 * @param S symbols.Symbol for theories and symbols.Assignment for links
 * @param I modules.TheoImport for theories and modules.LinkImport for links
 */
trait StatementSet[S <: Declaration, I <: Import] {
   //invariant: order contains the keys for which statements is defined in the order of addition
   protected val statements = new scala.collection.mutable.HashMap[LocalPath,S]
   protected var imports : List[I] = Nil
   protected var order : List[Statement] = Nil
   //invariant: "prefixes" stores all prefixes for all LocalPaths in the domain of "statements", together with a counter how often they occur
   protected val prefixes = new scala.collection.mutable.HashMap[LocalPath,Int]
   def get(name : LocalPath) : S = statements(name)
   def getO(name : LocalPath) : Option[S] =
      try {Some(get(name))}
      catch {case _ => None}
   def get(name : String) : S = get(new LocalPath(name))
   def getImports : List[I] = imports
   def add(s : S) {
	      val name = s.name
	      val pr = name.prefixes
	      if (prefixes.isDefinedAt(name))
	         throw AddError("a statement name with prefix " + name + " already exists")
	      if (pr.exists(statements.isDefinedAt(_)))
	         throw AddError("a statement name for a prefix of " + name + " already exists")
	      //increase counters for all prefixes of the added statement
	      pr.foreach(p => prefixes(p) = prefixes.getOrElse(p,0) + 1)
	      statements(name) = s
	      order = order ::: List(s)
   }
   def add(i : I) {
      imports ::= i
      order = order ::: List(i)
   }
   def delete(name : LocalPath) {
      if (statements.isDefinedAt(name)) {
         //decrease counters for all prefixes of the deleted statement
         name.prefixes.foreach(p => {
            val i = prefixes(p)
            if (i > 1) prefixes(p) = i - 1
            else prefixes -= p
         })
         statements -= name
         order = order.filter(_ != get(name))
      }
   }
   def update(s : S) = {
	   delete(s.name)
	   add(s)
   }
   def isEmpty = statements.isEmpty
   def valueList = order
}