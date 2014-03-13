package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api._
import parser._
import objects._
import Conversions._
import notations._

import scala.xml.Node

case class MathWebSearchQuery(pattern: TermPattern, answsize: Int = 1000, limitmin: Int = 0) {
   def toXML = {
      val queryCML = if (pattern.qvars.isEmpty)
         pattern.query.toCML
      else
         pattern.query.toCMLQVars(pattern.qvars)
      <mws:query xmlns:mws="http://www.mathweb.org/mws/ns" xmlns:m="http://www.w3.org/1998/Math/MathML"
             limitmin={limitmin.toString} answsize={answsize.toString} totalreq="yes" output="xml">
            <mws:expr>{queryCML}</mws:expr>
      </mws:query>
   }
}

/**
 * a wrapper around a remote MathWebSearch server
 * @param url the URL of the server
 */
class MathWebSearch(val url: java.net.URL) {
   /** sends a query
    *  @param query the query, using this.qvarBinder at the toplevel to bind the query variables
    *  @return MathWebSearch's reply
    */
   def apply(query: MathWebSearchQuery): List[SearchResult] = {
      val responseXML = utils.xml.post(url, query.toXML)
      val response = responseXML match {
         case <mws:answset>{answs @_*}</mws:answset> =>
            answs.toList.map {
               case n @ <mws:answ>{_*}</mws:answ> =>
                  val p = Path.parseC(utils.xml.attr(n, "uri"), utils.mmt.mmtbase)
                  val xpS = utils.xml.attr(n, "xpath")
                  // xpath has format "/*[Int].../*[Int]
                  val xp = xpS.substring(3, xpS.length-1).split("\\]/\\*\\[").toList.map(_.toInt - 1).tail
                  SearchResult(p, Position(xp), None)
            }
      }
      response
   }
}
