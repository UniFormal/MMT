package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import modules._
import symbols._
import notations._
import frontend._
import backend._
import objects._
import utils._
import documents._

class MWSHarvestExporter extends Exporter {
  val outDim = Dim("export", "mws")
  val key = "mws-harvest"
  override val outExt = "harvest"
    
  def exportTheory(t: DeclaredTheory, bf: BuildFile) { 
    rh("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
    rh("<mws:harvest xmlns:mws=\"http://search.mathweb.org/ns\" xmlns:m=\"http://www.w3.org/1998/Math/MathML\">\n")
    t.getDeclarations foreach {d =>
      d.getComponents.foreach {
         case (comp, tc: AbstractTermContainer) =>
            tc.get.foreach {t =>
               val node = <mws:expr url={CPath(d.path,comp).toPath}>{t.toCML}</mws:expr>
               rh(node.toString + "\n")
            }
         case _ => 
      }
    }
    rh("</mws:harvest>\n")
  }
  
  def exportView(v: DeclaredView, bf: BuildFile) { 
    //excluding expressions from views for now
  }
  
  
  def exportNamespace(dpath: DPath, bd: BuildDir, namespaces: List[(BuildDir,DPath)], modules: List[(BuildFile,MPath)]) {
    //Nothing to do - MathML in namespaces
  }
  
  def exportDocument(doc : Document, bt: BuildTask) {
       //Nothing to do - no MathML at document level
  }
}