package info.kwarc.mmt.api.archives
import info.kwarc.mmt.api._
import libraries._
import frontend._
import backend._
import modules._
import ontology._
import patterns._
import symbols._
import objects._
import utils._
import utils.FileConversion._

import scala.collection.mutable._

/** This trait adds validation operations to Archive's */
trait ValidatedArchive extends WritableArchive {
   /** checks modules in content structurally and generates term-level dependency relation in .occ files */
   def check(in: List[String] = Nil, controller: Controller) {
      traverse("content", in, extensionIs("omdoc")) {case Current(_, inPath) =>
         val mpath = Archive.ContentPathToMMTPath(inPath)
         val rels = new HashSet[RelationalElement]
         def reCont(r : RelationalElement) = {
           rels += r
           controller.memory.ontology += r
         }
         controller.checker.check(mpath)(reCont, _ => ())
         val relFile = (relDir / inPath).setExtension("occ")
         relFile.getParentFile.mkdirs
         val relFileHandle = File.Writer(relFile)
         rels foreach {r => relFileHandle.write(r.toPath + "\n")}
         relFileHandle.close
      }
    }
    
   /** checks modules in content structurally and then validates all ValidationUnits */
    def validate(in: List[String] = Nil, controller: Controller) {

      def validateUnit(v: ValidationUnit) {
         val solver = new Solver(controller, v.judgement.stack.theory, Context())
         val result = solver(v.judgement)
         if (result && solver.getSolution.isDefined)
            log("validated " + v.component)
         else
            log("errors while validating " + v.component)
      }
       
      traverse("content", in, extensionIs("omdoc")) {case Current(_, inPath) =>
         val mpath = Archive.ContentPathToMMTPath(inPath)
         controller.checker.check(mpath)(_ => (), validateUnit)
      }
    }
}

      //controller.checker.printStatistics()

      /*
      controller.compChecker.printStatistics()
      //println(controller.memory.ontology.getObjects(DependsOn))
      val ont = controller.memory.ontology

      val objects = ont.getObjects(DependsOn)
      val subjects = ont.getSubjects(DependsOn)
      val tpObj = objects.filter(x => x.last == "type")
      val dfObj = objects.filter(x => x.last == "definition")

      val transImps = objects.toList.map(x => ont.queryList(x, Transitive(ToSubject(DependsOn))).size).sortWith((x,y) => x < y)
      val imps = objects.toList.map(x => ont.queryList(x, ToSubject(DependsOn)).size).sortWith((x,y) => x < y)
      val impsTp = tpObj.toList.map(x => ont.queryList(x, ToSubject(DependsOn)).size).sortWith((x,y) => x < y)
      val impsDf = dfObj.toList.map(x => ont.queryList(x, ToSubject(DependsOn)).size).sortWith((x,y) => x < y)

      val deps = subjects.toList.map(x => ont.queryList(x, ToObject(DependsOn)).size).sortWith((x,y) => x < y)
      println(tpObj.size)
      println(dfObj.size)
      println("impsTp" + impsTp)
      println("impsDf" + impsDf)
      println("imps" + imps)
      println("transImps" + transImps)
      println("deps" + deps)
      println("obj" + objects.size)
      println("subj" + subjects.size)
      */
