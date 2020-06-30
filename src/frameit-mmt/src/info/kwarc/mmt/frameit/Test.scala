package info.kwarc.mmt.frameit

import info.kwarc.mmt.api.{DPath, MPath, Path}
import info.kwarc.mmt.api.frontend.{ConsoleHandler, Controller}
import info.kwarc.mmt.api.objects.{OMA, OMLIT}
import info.kwarc.mmt.api.ontology.{IsConstant, IsTheory, IsView}
import info.kwarc.mmt.api.symbols.FinalConstant
import info.kwarc.mmt.api.uom.RealizedType
import info.kwarc.mmt.api.utils.{File, FilePath}

object Test {
  def main(args: Array[String]): Unit = {
    val ctrl = new Controller()
    // All logging goes to console
    ctrl.report.addHandler(ConsoleHandler)

    val PathToMathhub = File("/home/benjamin/IdeaProjects/mathhub")
    val mmtArchiveHome = PathToMathhub //ctrl.getHome / "archives" / "MathHub"
    ctrl.addArchive(mmtArchiveHome / "UFrameIt_MMT")
    ctrl.addArchive(mmtArchiveHome / "MitM")
    ctrl.addArchive(mmtArchiveHome / "smglom")
    ctrl.addArchive(mmtArchiveHome / "smglom" / "arithmetics")
    ctrl.addArchive(mmtArchiveHome / "MMT/urtheories")
    ctrl.addArchive(mmtArchiveHome / "MMT/LFX")

    // The identifier "MMT/urtheories" is specified in "MMT/urtheories/META-INF/MANIFEST.MF"
    // In general every archive specified its ID there.
    //val urtheoriesArchive = ctrl.backend.getArchive("FrameIt/Prototype").get
    // The next two lines trigger processing of the whole archive and make the data
    // available in ctrl.depstore, the dependency store - among others.
    //urtheoriesArchive.allContent
    //urtheoriesArchive.readRelational(FilePath("/"), ctrl, "rel")

    val frameitArchive = {
      // Try quering the archive, possibly building once, then giving up
      //ctrl.build(mmtArchiveHome / "UFrameIt_MMT" / "source" / "Scrolls" / "FactTypeScroll.mmt")(info.kwarc.mmt.api.ErrorThrower)
      ctrl.handleLine(s"build"+ "FrameIt/Prototype"+" mmt-omdoc")
      ctrl.backend.getArchive("FrameIt/Prototype").getOrElse {
        ctrl.handleLine(s"build"+ "FrameIt/Prototype"+" mmt-omdoc")
        ctrl.backend.getArchive("FrameIt/Prototype").getOrElse(
          throw info.kwarc.mmt.api.GetError(s"Archive "+"FrameIt/Prototype" +" not found")
        )
      }
    }
    frameitArchive.allContent
    frameitArchive.readRelational(FilePath("/"), ctrl, "rel")
    // Get and print all individual ("inds") objects which are a theory
    val theoriespaths = ctrl.depstore.getInds(IsTheory)
    val theoryParser = new PushoutHandler(ctrl)
    //theories foreach println
    val theos = theoriespaths.filter(_.toString().contains("Situation")).map( t => {
      //println(t)
      ctrl.getTheory(t.asInstanceOf[MPath])
    })
    theos.foreach( t => {
      println(t)
      println(theoryParser.readPushout(t))
    })
  }
}

