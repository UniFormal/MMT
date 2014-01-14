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

class MWSHarvestContentExporter extends ContentExporter {
  val outDim = Dim("export", "mws", "content")
  val key = "mws-content"
  override val outExt = "mws"
  val custom : ArchiveCustomization = new DefaultCustomization    
    
  def doTheory(t: DeclaredTheory, bf: BuildFile) { 
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
  
  def doView(v: DeclaredView, bf: BuildFile) { 
    //excluding expressions from views for now
  }
  
  
  def doNamespace(dpath: DPath, bd: BuildDir, namespaces: List[(BuildDir,DPath)], modules: List[(BuildFile,MPath)]) {
    //Nothing to do - MathML in namespaces
  }
}

class MWSHarvestNarrationExporter extends NarrationExporter {
  val outDim = Dim("mws-narration")
  val key = "mws-narration"
  override val outExt = "mws"
    
  val custom : ArchiveCustomization = new DefaultCustomization    
   
  def doDocument(doc: Document, bt : BuildTask) {
    rh("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
    rh("<mws:harvest xmlns:mws=\"http://search.mathweb.org/ns\" xmlns:m=\"http://www.w3.org/1998/Math/MathML\">\n")
    try {
      def narrToCML(n : NarrativeObject) : List[scala.xml.Node] = n match {
        case nt : NarrativeTerm => List(custom.prepareQuery(nt.term))
        case nn : NarrativeNode => nn.child.flatMap(narrToCML)
        case _ => Nil
      }
      doc.components collect {
        case n : Narration =>  
          val exprs = narrToCML(n.content)
          val url = custom.mwsurl(doc.path)
          exprs foreach {cml =>
            val out = <mws:expr url={url}>{cml}</mws:expr>
            rh(out.toString + "\n")
          }        
      }
    } catch {
      case e : GetError => //doc not found, can safely ignore 
    }   
    rh("</mws:harvest>\n")
  } 
}