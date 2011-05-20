package info.kwarc.mmt.api.objects

object Conversions {
   /** implicit conversion between a context and a list of variable declarations */
   implicit def list2context(l : List[VarDecl]) : Context = Context(l : _*)
   implicit def context2list(c: Context) : List[VarDecl] = c.variables.toList
   /** implicit conversion between a substitution and a list of maps */
   implicit def list2substitution(l : List[Sub]) : Substitution = Substitution(l:_*)
   implicit def substitution2list(s: Substitution) : List[Sub] = s.subs.toList
   /** wraps OMV around a variable name, works well with the methods % and / of the OMV */ 
   implicit def string2OMV(s: String) : OMV = OMV(s)
}