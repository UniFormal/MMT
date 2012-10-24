package info.kwarc.mmt.api.objects
//import info.kwarc.mmt.api.patterns._
import info.kwarc.mmt.api._
/** Stores all implicit conversions related to MMT objects
 * This is imported in all source files that use conversions */

object Conversions {
   /** implicit conversion between a context and a list of variable declarations */
   implicit def list2context(l : List[VarDecl]) : Context = Context(l : _*)
   implicit def context2list(c: Context) : List[VarDecl] = c.variables.toList
   implicit def vardec2context(d: VarDecl) : Context = Context(d)
   /** implicit conversion between a substitution and a list of maps */
   implicit def list2substitution(l : List[Sub]) : Substitution = Substitution(l:_*)
   implicit def substitution2list(s: Substitution) : List[Sub] = s.subs.toList
   implicit def varsub2substitution(s: Sub) : Substitution = Substitution(s)   
   /** wraps OMV around a variable name, works well with the methods % and / of the OMV */ 
   implicit def string2OMV(s: String) : OMV = OMV(s)
   implicit def localName2OMV(s: LocalName) : OMV = OMV(s) 
   implicit def string2LocalName(s: String) : LocalName = LocalName(s)

   /*implicit def sequence2list(s : Sequence) : List[Term] = s.items.toList.map    
   */
//   implicit def list2sequence(l : List[Term]): Sequence = Sequence(l.map(x => SeqTerm(x)) : _*)
   /*implicit def sequence2list(s : Sequence) : List[Term] = s.items.toList.map    
   */
//   implicit def list2sequence(l : List[Term]): Sequence = Sequence(l.map(x => SeqTerm(x)) : _*)
}