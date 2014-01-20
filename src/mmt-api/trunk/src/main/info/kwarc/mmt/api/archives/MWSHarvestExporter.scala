package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import modules._
import symbols._
import presentation._
import frontend._
import backend._
import objects._
import utils._
import documents._

class MWSHarvestExporter extends Exporter {
  val outDim = Dim("export", "mws")
  val key = "mws-harvest"
  override val outExt = "mws"

  val custom : ArchiveCustomization = new DefaultCustomization    
    
  def exportTheory(t: DeclaredTheory, bf: BuildFile) { 
    rh("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
    rh("<mws:harvest xmlns:mws=\"http://search.mathweb.org/ns\" xmlns:m=\"http://www.w3.org/1998/Math/MathML\">\n")
    t.getDeclarations foreach {  
      case c: Constant =>
        List(c.tp,c.df).map(tO => tO map { 
          t =>
            val url = custom.mwsurl(c.path)
            val cml = custom.prepareQuery(t)
            val node = <mws:expr url={url}>{cml}</mws:expr>
            rh(node.toString + "\n")
        })
      /*case i : Instance => {
          val url = mwsbase + "/" + thy.name + ".html" + "#" + i.name.flat
          val node = <mws:expr url={url}>{i.matches.toCML}</mws:expr>
          outStream.write(node.toString + "\n")
        }*/
      case _ => 
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