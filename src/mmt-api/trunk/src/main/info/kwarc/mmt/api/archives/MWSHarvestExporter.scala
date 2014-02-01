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
  override val outExt = "harvest"
    
  def exportTheory(t: DeclaredTheory, bf: BuildFile) { 
    rh("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
    rh("<mws:harvest xmlns:mws=\"http://search.mathweb.org/ns\" xmlns:m=\"http://www.w3.org/1998/Math/MathML\">\n")
    t.getDeclarations foreach {d =>
      d.getComponents.foreach {
         case (comp, tc: AbstractTermContainer) =>
            tc.get.foreach {t =>
               val node = <mws:expr url={CPath(d.path,comp).toPath}><content>{t.toCML}</content></mws:expr>
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


import scala.xml.Node
import parser._
/**
 * a wrapper around a remote MathWebSearch server
 * @param url the URL of the server
 */
class MathWebSearch(url: java.net.URL) {
   val qvarBinder = utils.mmt.mmtcd ? "qvar"
   private val qvarMarkers = List(Delim("$"), Var(1, false, Some(Delim(","))), Delim(":"), Arg(2))
   val qvarNot = new TextNotation(qvarBinder, Mixfix(qvarMarkers), presentation.Precedence.infinite, utils.mmt.mmtcd)

   private def queryToXML(query: MathWebSearchQuery) = {
      val queryCML = query.term match {
         case OMBIND(OMS(this.qvarBinder), qvars, qBody) => qBody.toCMLQVars(qvars)
         case t => t.toCML
      }
      <mws:query xmlns:mws="http://www.mathweb.org/mws/ns" xmlns:m="http://www.w3.org/1998/Math/MathML"
             limitmin={query.limitmin.toString} answsize={query.answsize.toString} totalreq="yes" output="xml">
            <mws:expr>{queryCML}</mws:expr>
      </mws:query>
   }
   
   /** sends a query
    *  @param query the query, using this.qvarBinder at the toplevel to bind the query variables
    *  @return MathWebSearch's reply
    */
   def apply(query: MathWebSearchQuery): List[MathWebSearchAnswer] = {
      val responseXML = utils.xml.post(url, queryToXML(query))
      val response = responseXML match {
         case <mws:answset>{answs @_*}</mws:answset> =>
            answs.toList.map {
               case n @ <mws:answ>{_*}</mws:answ> =>
                  val p = Path.parseC(utils.xml.attr(n, "uri"), utils.mmt.mmtbase)
                  val xpS = utils.xml.attr(n, "xpath")
                  // xpath has format "/*[Int].../*[Int]
                  val xp = xpS.substring(3, xpS.length-1).split("\\]/\\*\\[").toList.map(_.toInt - 1).tail
                  MathWebSearchAnswer(p, Position(xp))
            }
      }
      response
   }
}

/** an answer in an MathWebSearch answer set */
case class MathWebSearchAnswer(cpath: CPath, pos: Position)

case class MathWebSearchQuery(term: Term, answsize: Int = 1000, limitmin: Int = 0)