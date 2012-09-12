package info.kwarc.mmt.api.modules
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.utils._

import scala.xml.Node

/**
 * Body represents the content of modules, i.e., a set of declarations.
 * 
 * It is mixed into theories and links to hold the symbols and assignments, respectively.
 * It provides a name-indexed hash map named declarations, and a set of unnamed declarations.
 * For named declarations, the name must be unique; for unnamed declarations, the field implictKey must be non-empty and unique. 
 * 
 * @param S Symbol for theories and Assignment for links
 */
trait Body[S <: Declaration] {
   /** the set of named statements, indexed by name */
   protected val statements = new scala.collection.mutable.HashMap[LocalName,S]
   /** the set of unnamed statement */
   protected val implicits  = new scala.collection.mutable.HashSet[S]
   /** all declarations in reverse order of addition */
   protected var order : List[S] = Nil
   //invariant: "prefixes" stores all prefixes for all LocalPaths in the domain of "statements", together with a counter how often they occur
   //protected val prefixes = new scala.collection.mutable.HashMap[LocalName,Int]
   /** true iff a declaration for a name is present */ 
   def declares(name: LocalName) = statements.isDefinedAt(name)
   def domain = statements.keySet
   /** retrieve a named declaration, may throw exception if not present */ 
   def get(name : LocalName) : S = statements(name)
   /** retrieve a declaration, None if not present */ 
   def getO(name : LocalName) : Option[S] =
      try {Some(get(name))}
      catch {case _ => None}
   /** same as get(LocalName(name)) */ 
   def get(name : String) : S = get(LocalName(name))
   /** retrieves the most specific applicable declaration
    * @param name the name of the declaration
    * @param rest the suffix that has been split off so far; this argument should be omitted in calls from outside this class 
    * @return the most specific (longest prefix of name) known declaration (if any) and the remaining suffix
    */
   def getMostSpecific(name: LocalName, rest : LocalName = LocalName(Nil)) : Option[(S, LocalName)] =
      statements.get(name) match {
         case Some(d) => Some((d, rest))
         case None => name match {
            case LocalName.Anon => None
            case !(n) => None
            case ln \ n => getMostSpecific(ln, n / rest)
         }
      }
   /** adds a named or unnamed declaration, throws exception if name already declared */ 
   def add(s : S) {
	      val name = s.name
	 /*     val pr = name.prefixes
	      if (prefixes.isDefinedAt(name))
	         throw AddError("a statement name with prefix " + name + " already exists")
	      if (pr.exists(statements.isDefinedAt(_)))
	         throw AddError("a statement name for a prefix of " + name + " already exists")
	      //increase counters for all prefixes of the added statement
	      pr.foreach(p => prefixes(p) = prefixes.getOrElse(p,0) + 1) */
	      if (name.isAnonymous) {
	         s.implicitKey match {
	           case None => throw AddError("an unnamed declaration must have a key")
	           case Some(key) => get(key) match {
	              case Some(_) => throw AddError("an unnamed declaration with the same key already exists: " +  s.toString)
	              case None => implicits += s
	           }
	         }
	      } else {
            if (statements.isDefinedAt(name)) {
              //statements.toList.map(x => println(x._2))
               throw AddError("a declaration for the name " + name + " already exists")
            }
	         statements(name) = s
	      }
	      order = s :: order
   }
   /** delete a named declaration (does not have to exist) */
   def delete(name : LocalName) {
      if (statements.isDefinedAt(name)) {
         //decrease counters for all prefixes of the deleted statement
         /*name.prefixes.foreach(p => {
            val i = prefixes(p)
            if (i > 1) prefixes(p) = i - 1
            else prefixes -= p
         })*/
         statements -= name
         order = order.filter(_.name != name)
      }
   }
   /** replace an named declaration with another one (preserving the order) */
   /* this is deprecated, call replace below instead
     def replace(old : S, nw : S) {
     statements -= old.name
     statements(nw.name) = nw
     order = order.foldRight[List[S]](Nil)((x,o) => if (x.path == old.path) {nw :: o} else {x :: o})
   }*/ 
   /** updates a named declaration (preserving the order) */
   def update(s : S) = {
	   replace(s.name, s)
   }
   /** replaces a named declaration with others (preserving the order) */
   def replace(name: LocalName, decs: S*) {
      if (name.isAnonymous) return
      var seen : List[S] = Nil
      var tosee : List[S] = order
      var continue = true
      while (continue && ! tosee.isEmpty) {
         val hd = tosee.head
         if (hd.name == name) {
            order = seen.reverse ::: decs.reverse.toList ::: tosee.tail
            continue = false
         } else {
            seen = hd :: seen
            tosee = tosee.tail
         }
      }
      if (continue) throw AddError("no declaration " + name + " found")
      statements -= name
      decs foreach {d =>
        if (statements.isDefinedAt(d.name))
           throw AddError("a declaration for the name " + d.name + " already exists")
        statements(d.name) = d
      }
   }
   /** gets the unnamed declaration with a certain key */
   def get(key: Term) : Option[S] = {
      implicits find {d => d.implicitKey == Some(key)}
   }
   /** deletes the unnamed declaration with a certain key */
   def delete(key: Term) {
      get(key) match {
         case Some(d) =>  
            implicits -= d
            order = order filterNot(_ == d)
         case None =>
      }
   }
   /** true iff no declarations present */
   def isEmpty = statements.isEmpty && implicits.isEmpty
   /** the list of declarations in the order of addition, includes declarations generated by MMT */
   def valueList = order.reverse.toList
   /** the list of declarations in the order of addition, excludes declarations generated by MMT */
   def valueListNG = valueList.filter(! _.isGenerated)
   /** the list of declarations using elaborated declarations where possible */
   def valueListElab = valueList.filter(_.inElaborated)
   /** the list of declarations in the order of addition as an iterator */
   def iterator = valueList.iterator
   protected def innerNodes = valueListNG.map(_.toNode)
   protected def innerNodesElab = valueListElab.map(_.toNode)
   protected def innerString =
      if (valueListNG.isEmpty) ""
      else valueListNG.map("\t" + _.toString).mkString(" = {\n", "\n", "\n}")
   def innerComponents = valueListNG.filter(! _.isGenerated)
}