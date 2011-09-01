package info.kwarc.mmt.mizar.test

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.mizar.mizar.reader._
import info.kwarc.mmt.mizar.mizar.translator._
import info.kwarc.mmt.mizar.mmt.objects._

import java.net.HttpURLConnection;
import java.net._
import java.io._
/*
class M2OWebServer {
  def main() {
    val port = 6649
    val threads = 5
    val w = new M2OThread()
  }
}

class M2OThread extends M2OWebServer with Runnable {
  
}
*/
object MwsService {

  def makeQVars(n : scala.xml.Node) : scala.xml.Node = n match {
    case <m:apply><csymbol>{c}</csymbol><m:ci>{name}</m:ci></m:apply> => 
      if (c.toString == "http://oaff.mathweb.org/mml?qvar?qvar") {
        <mws:qvar>{name}</mws:qvar>
      } else { 
        n
      }
    case _ =>    
      if (n.child.length == 0)
          n
      else
    	  new scala.xml.Elem(n.prefix, n.label, n.attributes, n.scope, n.child.map(makeQVars(_)) : _*)

  }
  
  def parseQuery(n : scala.xml.Node, aid : String, mmlversion : String) : scala.xml.Node = {
    TranslationController.currentAid = aid
    TranslationController.currentTheory = DPath(Mizar.baseURI) ? TranslationController.currentAid
	
    var nr = n.child.length
    val qvars = n.child.slice(0, nr - 1)
    qvars.map(x => TranslationController.addQVarBinder())
    val p = n.child(nr - 1)
    val expr = p.label match {
    	case "Not" | "And" | "For" | "Pred" | "PrivPred" | "Is" | "Verum"  => 
    	  val mizq = PropositionParser.parseFormula(p)
    	  val mmtq = PropositionTranslator.translateFormula(mizq)
    	  <mws:query output="xml" limitmin="0" answsize="30"><mws:expr>{mmtq.toCML}</mws:expr></mws:query>
    	case "Typ" => 
    	  val mizq = TypeParser.parseTyp(p)
    	  val mmtq = TypeTranslator.translateTyp(mizq)
    	  <mws:query output="xml" limitmin="0" answsize="30"><mws:expr>{mmtq.toCML}</mws:expr></mws:query>

    	case _ => 
    	  val mizq = TypeParser.parseTerm(p)
    	  val mmtq = TypeTranslator.translateTerm(mizq)
    	  <mws:query output="xml" limitmin="0" answsize="30"><mws:expr>{mmtq.toCML}</mws:expr></mws:query>
    }
    makeQVars(expr)
  }
  
  
  
  def main(args: Array[String]): Unit = {
    val n = MizarCompiler.getNode("test.xml")
	val q = parseQuery(n,"ORDINAL2","")
	println(q)
	val urlStr = "http://localhost:6284"
    val url = new URL(urlStr);
    val conn =  url.openConnection.asInstanceOf[HttpURLConnection]
    conn.setRequestMethod("POST")
    conn.setDoOutput(true);
	val out = conn.getOutputStream()
	out.write(q.toString.getBytes())
	out.close()
	val in = new BufferedReader(new InputStreamReader(conn.getInputStream()));
	var v = ""
	while(v != null) {
	  println(v)
	  v = in.readLine()
	}
	in.close()
	
  }

}