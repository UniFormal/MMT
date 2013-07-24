package info.kwarc.mmt.api.pragmatics
import info.kwarc.mmt.api._

//TODO this should look into includes as well; but this is subsumed by the next TODO
//TODO this should be abolished in favor of a RoleHandler
class PragmaticStore {
   /** stores the features, hashed by defining theory */
   private val features = new utils.HashMapToSet[MPath, Feature]
   /** caches all strict application symbols */
   private var apps : List[GlobalName] = Nil

   def add(fs: Feature*) = fs foreach {f =>
      features(f.theory) += f
      f match {
         case a: Application => apps ::= a.apply
         case _ =>
      }
   }
   def getStrictApps = apps
   def getApplication(m: MPath) : Option[Application] = {
      features.getOrElse(m, Nil) map {
         case a: Application => return Some(a)
         case _ =>
      }
      None
   } 
   def getHOAS(m: MPath) : Option[HOAS] = {
      features.getOrElse(m, Nil) map {
         case h: HOAS => return Some(h)
         case _ =>
      }
      None
   }
   def getTyping(m: MPath) : Option[Typing] = {
      features.getOrElse(m, Nil) map {
         case t: Typing => return Some(t)
         case _ =>
      }
      None
   }
}
