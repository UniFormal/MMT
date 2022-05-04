package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api.Level.Level
import info.kwarc.mmt.api._
import utils._

/** builds a folder by concatenating the build results of its children
  *
  * not used yet!
  * */
trait AggregatingBuildTarget extends TraversingBuildTarget {
   override def buildDir(bd: BuildTask, builtChildren: List[BuildTask]): BuildResult = {
     var res = ""
     builtChildren.foreach {bt =>
       val f = File.read(bt.outFile)
       res += f
     }
     File.write(bd.outFile, res)
     BuildSuccess(Nil, Nil)
   }
}
