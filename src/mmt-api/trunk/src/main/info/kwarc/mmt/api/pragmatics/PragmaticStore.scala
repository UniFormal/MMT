package info.kwarc.mmt.api.pragmatics
import info.kwarc.mmt.api._


//experimental

class PragmaticStore {
   private val features = new utils.HashMapToSet[MPath, Feature]
   def add(fs: Feature*) = fs foreach {f => features(f.theory) += f}
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
}

