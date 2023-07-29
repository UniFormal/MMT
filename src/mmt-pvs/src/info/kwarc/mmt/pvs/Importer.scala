package info.kwarc.mmt.pvs

import syntax._
import info.kwarc.mmt.api._
import documents._
import archives._
import info.kwarc.mmt.api.ontology.{RelationalElement, ULOStatement}

class PVSImporter extends Importer {
   val key = "pvs-omdoc"


   override def start(args: List[String]): Unit = {
      val em = controller.extman
      // content enhancers
      super.start(args)
      em.addExtension(new LambdaPiInclude)
      // em.addExtension(new PVSImporter)
      em.addExtension(new PVSServer)
      em.addExtension(PVSTheory.preproc)
   }

   def inExts = List("xml")

   //override def inDim = RedirectableDimension("pvsxml", Some(Dim("src","pvsxml")))

   private val parseXML = syntax.makeParser

   //private var startAt = "/home/raupi/lmh/localmh/MathHub/PVS/NASA/source/vect_analysis/pvsxml/cont_real_vect2.xml"
   // private var startAt = "/home/raupi/lmh/localmh/MathHub/PVS/Prelude/src/pvsxml/K_props.xml"
   def importDocument(bf: BuildTask, index: Document => Unit,rel:ULOStatement => Unit): BuildResult = {
      //   if (bf.inFile.toFilePath.toString < startAt) return BuildResult.empty
      log("Reading " + bf.inFile)
      val e = try {
         parseXML(bf.inFile)
      } catch {
         case e: utils.ExtractError =>
            log(e.getMessage)
            sys.exit()
      }

      val conv = new PVSImportTask(controller, bf, index)
      val ret = e match {
         case d: pvs_file =>
            conv.doDocument(d)
         case m: syntax.Module =>
            conv.doDocument(pvs_file(List(m)))
      }

      /*
      try {
         ret match {
            case BuildSuccess(used, pr) => Sorter.add(pr.head.asInstanceOf[LogicalDependency].mpath, used.map(
               _.asInstanceOf[LogicalDependency].mpath))
            case MissingDependency(used, pr) => Sorter.add(pr.head.asInstanceOf[LogicalDependency].mpath, used.map(
               _.asInstanceOf[LogicalDependency].mpath))
         }
      } catch {
         case e : Throwable =>
          e.printStackTrace()
          throw e
      }
      BuildResult.empty
      */

      ret

   }
}

object Sorter {
   // private val initstr = "http://shemesh.larc.nasa.gov/fm/ftp/larc/PVS-library/"
   private val initstr = "http://pvs.csl.sri.com/"
   private var ls : List[(String,Set[String])] = Nil

   private def mkString(mp : MPath) : String = {
      //println(mp.toString.drop(initstr.length))
      val ar = mp.toString.drop(initstr.length).split('?').toList
      //println(ar)
      ar.head + "/pvsxml/" + ar.tail.head + ".xml"
   }

   def add(n : MPath, deps : List[MPath]) = {
      val fname = mkString(n)
      if (!ls.exists(p => p._1 == fname)) {
         ls ::= ((fname,(deps.filter(_.toString.startsWith(initstr)) map mkString).toSet))
         // sort()
         // ls foreach {p => println("controller.handleLine(\"build PVS/NASA mmt-omdoc " + p._1 + "\")")}
      }
   }
   private def sort(i : Int = 0): Unit = {
      if (i == ls.length -1) return
      else if (ls.drop(i + 1).exists(p => ls(i)._2 contains p._1)) {
         ls = ls.take(i) ::: ls.drop(i+1) ::: ls(i) :: Nil
         sort()
      } else sort(i+1)
   }
   def printsorted = {
      sort()
      ls foreach {p => println("controller.handleLine(\"build PVS/Prelude pvs-omdoc " + p._1 + "\")")}
   }
}
