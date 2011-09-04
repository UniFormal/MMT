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
  
  def applyInferences(f : scala.xml.Node, args : List[scala.xml.Node]) : List[scala.xml.Node] = (f.toString, args) match {
    case ("http://oaff.mathweb.org/mml?HIDDEN?R1", a :: b :: Nil) => 
      if (!(a.label == "qvar" && b.label == "qvar")) {
    	  <m:apply><csymbol>{f}</csymbol>{a}{b}</m:apply> ::
    	  <m:apply><csymbol>{f}</csymbol>{b}{a}</m:apply> :: Nil
      } else {
    	  <m:apply><csymbol>{f}</csymbol>{a}{b}</m:apply> :: Nil    	  
      }
      
    case _ => List(<m:apply><csymbol>{f}</csymbol>{args}</m:apply>)
  }

  def product[S](l : List[List[S]]) : List[List[S]] = l match {
    case Nil => List(Nil)
    case hd :: tl => hd.flatMap(x => product[S](tl).map(y => x :: y))
  }
  
  def applyImplicitInferences(q : scala.xml.Node) : List[scala.xml.Node]= q match {
    case <m:apply><csymbol>{f}</csymbol>{args @ _*}</m:apply> => 
      product[scala.xml.Node](args.map(applyImplicitInferences(_)).toList).flatMap(x => applyInferences(f,x))
    case _ => q.child.length match {
      case 0 => List(q)
      case _ => product[scala.xml.Node](q.child.map(applyImplicitInferences(_)).toList).map(l => 
      	new scala.xml.Elem(q.prefix, q.label, q.attributes, q.scope, l : _*))
    }
  }
  
  
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
  
  def parseQuery(n : scala.xml.Node, aid : String, mmlversion : String, limitmin : String) : scala.xml.Node = {
    TranslationController.currentAid = aid
    TranslationController.currentTheory = DPath(Mizar.baseURI) ? TranslationController.currentAid
	
    var nr = n.child.length
    val qvars = n.child.slice(0, nr - 1)
    println(qvars.length)
    qvars.map(x => TranslationController.addQVarBinder())
    val p = n.child(nr - 1)
    val cml = p.label match {
    	case "Not" | "And" | "For" | "Pred" | "PrivPred" | "Is" | "Verum"  => 
    	  val mizq = PropositionParser.parseFormula(p)
    	  val mmtq = PropositionTranslator.translateFormula(mizq)
    	  mmtq.toCML
    	case "Typ" => 
    	  val mizq = TypeParser.parseTyp(p)
    	  val mmtq = TypeTranslator.translateTyp(mizq)
    	  mmtq.toCML

    	case _ => 
    	  val mizq = TypeParser.parseTerm(p)
    	  val mmtq = TypeTranslator.translateTerm(mizq)
    	  mmtq.toCML
    }
    TranslationController.clearVarContext()
    val q = makeQVars(cml)
    println(applyImplicitInferences(q).length)
    applyImplicitInferences(q).map(println)
    <mws:query output="xml" limitmin={limitmin} answsize="30"><mws:expr>{q}</mws:expr></mws:query>

  }
  
  
  
  def main(args: Array[String]): Unit = {
    val n = MizarCompiler.getNode("test.xml")
	val q = parseQuery(n,"ORDINAL2","","0")
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