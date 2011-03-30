package info.kwarc.mmt.api.modules
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.utils._
import scala.xml.Node

/**
 * StatementSet represents the content of modules, i.e., a set of declarations.
 * 
 * It provides (i) a name-indexed hash map of symbol level Declarations, and a list of Imports.
 * It is mixed into theories and links to hold the symbols/imports and assignments/imports, respectively.
 * 
 * @param S symbols.Symbol for theories and symbols.Assignment for links
 */
trait Body[S <: Declaration] {
   //invariant: order contains the keys for which statements is defined in the order of addition
   protected val statements = new scala.collection.mutable.HashMap[LocalName,S]
   protected var order : List[S] = Nil
   //invariant: "prefixes" stores all prefixes for all LocalPaths in the domain of "statements", together with a counter how often they occur
   //protected val prefixes = new scala.collection.mutable.HashMap[LocalName,Int]
   def get(name : LocalName) : S = statements(name)
   def getFirst(name: LocalName, error: String => Nothing) : (S, Option[LocalName]) =
      getFirst(name, None, error(" no prefix of " + name + " is declared"))
   private def getFirst(name: LocalName, rest : Option[LocalName], error: => Nothing) : (S, Option[LocalName]) =
      statements.get(name) match {
         case Some(d) => (d, rest)
         case None => name match {
            case !(n) => error
            case ln \ n =>
              val r = rest match {case None => !(n) case Some(l) => n / l}
              getFirst(ln, Some(r), error)
         }
      }
   def getO(name : LocalName) : Option[S] =
      try {Some(get(name))}
      catch {case _ => None}
   def get(name : String) : S = get(LocalName(name))
   def add(s : S) {
	      val name = s.name
	 /*     val pr = name.prefixes
	      if (prefixes.isDefinedAt(name))
	         throw AddError("a statement name with prefix " + name + " already exists")
	      if (pr.exists(statements.isDefinedAt(_)))
	         throw AddError("a statement name for a prefix of " + name + " already exists")
	      //increase counters for all prefixes of the added statement
	      pr.foreach(p => prefixes(p) = prefixes.getOrElse(p,0) + 1) */
	      if (statements.isDefinedAt(name))
	         throw AddError("a declaration for the name " + name + " already exists")
	      statements(name) = s
	      order = order ::: List(s)
   }
   def delete(name : LocalName) {
      if (statements.isDefinedAt(name)) {
         //decrease counters for all prefixes of the deleted statement
         /*name.prefixes.foreach(p => {
            val i = prefixes(p)
            if (i > 1) prefixes(p) = i - 1
            else prefixes -= p
         })*/
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
   protected def innerNodes = valueList.map(_.toNode)
   protected def innerString = valueList.map("\t" + _.toString).mkString("\n{", "\n", "\n}")
   protected def innerComponents = valueList
}