package info.kwarc.mmt.api.moc

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.utils._ 
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.presentation._


object Main {

  def main(args: Array[String]): Unit = {
    //val p = new DPath(mmt.baseURI / "set_theories" / "mizar" / "HIDDEN.omdoc")
     val mp = new DPath(mmt.baseURI / "owl" / "families" / "identifiers.omdoc") //? "_"
    //TODO configure checking
    val cold = new Controller
    val cnew = new Controller

    
    cold.handle(ExecFile(File("moc1-startup.mmt")))
    cnew.handle(ExecFile(File("moc2-startup.mmt")))

    /*
    cold.backend.getArchives map {archive =>
      archive.generateSVNArchive(cnew.backend)
    }
    */

    println("--- Computing Diff ---")
    
    val diff = Differ.diff(cold, cnew, mp, mp)
        
    val pp = new scala.xml.PrettyPrinter(100,2)
    println("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" + pp.format(diff.toNode))
    
    /*
    println("--- Applying Diff ---")
    
    val c = new Controller( new libraries.FoundChecker(new libraries.DefaultFoundation, NullReport), NullReport)
    c.handle(ExecFile(new java.io.File("moc3-startup.mmt")))
    val doc = Patcher.patch(diff, c)
    
    /*// Presenter stuff 
    val nset = new DPath(mmt.baseURI / "foundations"/ "lf" / "mathml.omdoc") ? "mathml"
    
    val m = doc.getModulesResolved(c.library)(0)
    val builder = new presentation.FileWriter(new java.io.File("/home/mihnea/hidden.xhtml"))
    c.presenter.apply(m, GlobalParams(builder,nset))
    builder.file.close()

    val om = cold.getDocument(p).getModulesResolved(cold.library)(0)
    val obuilder = new presentation.FileWriter(new java.io.File("/home/mihnea/hidden-old.xhtml"))
    cold.presenter.apply(om, GlobalParams(obuilder,nset))
    obuilder.file.close()
    
    val nm = cnew.getDocument(p).getModulesResolved(cnew.library)(0)
    val nbuilder = new presentation.FileWriter(new java.io.File("/home/mihnea/hidden-new.xhtml"))
    cnew.presenter.apply(nm, GlobalParams(nbuilder,nset))
    nbuilder.file.close()
    */
    
    val docPath = "/home/mihnea/kwarc/moc-test/r3/set_theories/mizar/HIDDEN.omdoc"
	val out = new java.io.FileWriter(docPath)
    val docNode =  pp.format(doc.toNodeResolved(c.library))
    
    out.write("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" + docNode.toString)
    out.close
    
    
    //val ctest = new Controller( new libraries.FoundChecker(libraries.DefaultFoundation), NullReport)
    //ctest.handle(ExecFile(new java.io.File("moc3-startup.mmt")))

    println("--- Checking new Diff (should be empty) ---")

    val diff2 = Differ.diff(cnew, c, p, p)
    println(pp.format(diff2.toNodeFlat))
   */
  }

}