package info.kwarc.mmt.mizar.test

import info.kwarc.mmt.mizar.mizar.reader._
import info.kwarc.mmt.mizar.mizar.translator._
import info.kwarc.mmt.mizar.mmt.objects._

import info.kwarc.mmt.api._
import objects._
import backend._

import java.net.HttpURLConnection;
import java.net._
import java.io._

class MwsService() extends QueryTransformer {
  def isApplicable(src : String) : Boolean = src == "mizar"

  def transformSearchQuery(n : scala.xml.Node, params: List[String]) : List[scala.xml.Node] = {
     val aid = params(0)
     val mmlversion = params(1)
     val q = parseQuery(scala.xml.Utility.trim(n), aid, mmlversion)
     applyImplicitInferences(q)
  }

  private def removeLFApp(n : scala.xml.Node) : scala.xml.Node = n match {
    case <m:apply><csymbol>{x}</csymbol>{s @ _*}</m:apply> => 
      if (x.toString == "http://cds.omdoc.org/foundational?LF?@")
        <m:apply>{s.map(removeLFApp)}</m:apply>
      else {
        new scala.xml.Elem(n.prefix, n.label, n.attributes, n.scope, true, n.child.map(removeLFApp) : _*)
      }
    case _ =>  
      if (n.child.length == 0)
        n
      else
        new scala.xml.Elem(n.prefix, n.label, n.attributes, n.scope, true, n.child.map(removeLFApp(_)) : _*)
  }
  
  def applyInferences(f : scala.xml.Node, args : List[scala.xml.Node]) : List[scala.xml.Node] = (f.toString, args) match {
    case ("HIDDEN?R1", a :: b :: Nil) => 
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
      	new scala.xml.Elem(q.prefix, q.label, q.attributes, q.scope, true, l : _*))
    }
  }
  
  def makeQVars(n : scala.xml.Node, evars : List[String], uvars : List[String]) : scala.xml.Node = n match {
    case <m:apply><m:apply><csymbol>{s}</csymbol>{a1}</m:apply><m:bvar><m:apply>{zz}<m:ci>{v}</m:ci>{a}{b}</m:apply></m:bvar>{body}</m:apply> =>
      if (s.toString == "http://latin.omdoc.org/foundations/mizar?mizar-curry?ex") {
    	  makeQVars(body,  v.toString() :: evars, uvars)
      } else if (s.toString == "http://latin.omdoc.org/foundations/mizar?mizar-curry?for") {
    	  makeQVars(body, evars, v.toString() :: uvars) 
      } else {
    	  new scala.xml.Elem(n.prefix, n.label, n.attributes, n.scope, true, n.child.map(makeQVars(_, evars, uvars)) : _*)

      }
    case <m:apply><csymbol>{c}</csymbol><m:ci>{name}</m:ci></m:apply> => 
      if (c.toString == "http://oaff.mathweb.org/mml?qvar?qvar") {
        <mws:uvar><mws:qvar>{name}</mws:qvar></mws:uvar>
      } else if (c.toString == "http://oaff.mathweb.org/mml?qvar?const") {
        <mws:uvar><mws:qvar>{name}</mws:qvar></mws:uvar>
      } else if (c.toString == "http://oaff.mathweb.org/mml?qvar?constFunc") {
        <mws:qvar>{name}</mws:qvar>
      } else { 
        n
      }	
    
    case <m:ci>{s}</m:ci> => {
      if (evars.contains(s.toString))
        <mws:qvar>{s}</mws:qvar>
      else if (uvars.contains(s.toString))
        <mws:uvar><mws:qvar>{s}</mws:qvar></mws:uvar>
      else
        n
    }
    
    case <csymbol>{p}</csymbol> => 
        val firstq = p.toString.indexOf('?')
        <csymbol>{p.toString.substring(firstq + 1)}</csymbol>
    case _ => 
      if (n.child.length == 0)
          n
      else
    	  new scala.xml.Elem(n.prefix, n.label, n.attributes, n.scope, true, n.child.map(makeQVars(_, evars, uvars)) : _*)
  }
  
  def parseQuery(n : scala.xml.Node, aid : String, mmlversion : String) : scala.xml.Node = {
    TranslationController.query = true
    TranslationController.currentAid = aid
    TranslationController.currentVersion = mmlversion.toInt
    
    var nr = n.child.length
    val qvars = n.child.slice(0, nr - 1)
    qvars.map(x => TranslationController.addQVarBinder())
    val p = n.child(nr - 1)
    val cml = p.label match {
    	case "Not" | "And" | "For" | "Pred" | "PrivPred" | "Is" | "Verum" | "Exists"  => 
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
    val q = makeQVars(removeLFApp(cml), Nil, Nil)
    <mws:expr>{q}</mws:expr>
  }
  
  
  def main(args: Array[String]): Unit = {
    val mizar = new MizarCompiler
    val n = mizar.getNode("test.xml")
	val q = transformSearchQuery(n, List("ORDINAL2",""))
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
