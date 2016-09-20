package info.kwarc.mmt.api.web

import info.kwarc.mmt.api.frontend.Logger
import info.kwarc.mmt.api.{GeneralError, utils}
import info.kwarc.mmt.api.utils.{File, HTML, URI}

import scala.collection.immutable.List
import scala.util.Try
import scala.util.matching.Regex.Groups
import scala.xml.{Elem, Node}

abstract class WebExtractor {
  val scheme : String
  val key : String
  val dontpull : Boolean = false
  def pull(urlp: String) : Elem = {
    val url = URI(scheme + "://" + urlp).toJava.toURL
    val input = File.getFromURL(url)
    var output = new java.io.ByteArrayOutputStream()
    try {
      val byteArray = Stream.continually(input.read).takeWhile(_ != -1).map(_.toByte).toArray
      output.write(byteArray)
      scala.xml.XML.loadString(output.toString)
    } catch {
      case e : Exception =>
        <html></html>
    } finally {
      input.close
      output.close
    }
  }

  def getChild(n : Elem, child : String, prop : (String,String) = ("","")) : Elem = {
    if (prop == ("",""))
      (n \ child).toList.collect({case a : Elem => a}).headOption.getOrElse(throw new Exception("Child not found: " + child + " in " + n))
    else (n \ child).toList.collect({case a : Elem => a}).find(p => (p \ ("@" + prop._1)).text == prop._2).getOrElse(throw new Exception("Child not found: " + child + " in " + n))
  }
  private def retrieveInt(n : Elem, child : Either[String,(String,String,String)]*) : Elem = {
    if (child.isEmpty) n
    else {
      val nnode = child.head match {
        case Left(s) => getChild(n,s)
        case Right((a,b,c)) => getChild(n,a,(b,c))
      }
      retrieveInt(nnode,child.tail:_*)
    }
  }
  def retrieve(n: Elem, child : Any*) : Elem = retrieveInt(n,child map {
    case s : String => Left(s)
    case (a : String,b : String, c : String) => Right((a,b,c))
    case _ => throw new Exception("Only strings or string triples allowed!")
  }:_*)

  def apply(uri : String, h: HTML): Unit = {
    import h._
    table(cls="external-content",attributes = List(("width","100%"))) {
      tr(cls="external-header") {
        th(cls="external-header") { a(scheme + "://" + uri) { text { key } } }
      }
      tr {
        td {
          if (!dontpull) Try(content(pull(uri),uri,h)).getOrElse({text {"Error: " };a(scheme + "://" + uri) { text { scheme + "://" + uri } } })
          else content(<html><body></body></html>,uri,h)
        }
      }
    }
  }

  protected def content(node: Elem, uri : String, h : HTML)
}

object WikiExtractor extends WebExtractor {
  val key = "Wikipedia"
  val scheme = "http"
  def content(wiki : Elem, uri : String, h : HTML) = {
    wiki match {
      case ht @ <html>{hbd @ _*}</html> =>
        val content = retrieve(ht,"body",("div","id","content"),("div","id","bodyContent"),("div","id","mw-content-text"))
        var ret : List[Node] = Nil
        var done = false
        content.child.foreach(n => if(!done) n match {
          case d @ <div>{s @ _*}</div> if (d \ "@class").text == "hatnote" =>
          case d @ <div>{s @ _*}</div> if (d \ "@class").text == "thumb tright" =>
          case d @ <div>{s @ _*}</div> if (d \ "@class").text == "NavContent" =>
          case d @ <h2>{s @ _*}</h2> => done = true
          case d @ <h3>{s @ _*}</h3> => done = true
          case d @ <table>{s @ _*}</table> if (d \ "@class").text contains "navbox" =>
          case d @ <table>{s @ _*}</table> if (d \ "@class").text contains "metadata" =>
          case d @ <table>{s @ _*}</table> if (d \ "@class").text contains "ambox-Refimprove" =>
          case d @ <div>{s @ _*}</div> if (d \ "@id").text == "toc" => done = true
          case nod => ret ::= nod
        })
        val nret = ret.reverse.map(_.toString.replaceAll(
          "href=\"/","href=\"http://en.wikipedia.org/").replaceAll("href=\"#","href=\"" + uri + "#"))
        h.literal(nret.mkString(""))
      }
    }
}

object PlanetMathExtractor extends WebExtractor {
  val key = "planetmath.org"
  val scheme = "http"
  override val dontpull: Boolean = true
  def content(pm : Elem, uri : String, h : HTML) = {
    pm match {
      case ht @ <html>{hbd @ _*}</html> =>
        //val content = retrieve(ht,"body",("div","id","content"),("div","id","bodyContent"),("div","id","mw-content-text"))
        h.a(uri) { h.text { uri } }
        // TODO: I get malformed html back -.-
    }
  }
}

object WolframExtractor extends WebExtractor {
  val key = "Wolfram MathWorld"
  val scheme = "http"
  override val dontpull = true
  def content(pm : Elem, uri : String, h : HTML) = {
    pm match {
      case ht @ <html>{hbd @ _*}</html> =>
        h.a(uri) { h.text { uri } }
        // Waaay to slow!
        /*
        val c1 = retrieve(ht,"body",("table","id","pageTable"),"tr")
        val content = retrieve(c1.child(1).asInstanceOf[Elem],("div","id","mainContent"))
        var ret : List[Node] = Nil
        var done = false
        content.child.foreach(n => if(!done) n match {
          case d @ <div>{s @ _*}</div> if (d \ "@class").text == "linktrail" =>
          case d @ <h1>{s @ _*}</h1> =>
          case d @ <br/> =>
          case d @ <div>{s @ _*}</div> if (d \ "@id").text == "related" => done = true
          case nod => ret ::= nod
        })
        val nret = ret.reverse.map(_.toString.replaceAll(
          "href=\"/","href=\"http://mathworld.wolfram.com/").replaceAll(
          "src=\"/","src=\"http://mathworld.wolfram.com/").replaceAll("href=\"#","href=\"" + uri + "#"))
        h.literal(nret.mkString(""))
        */
    }
  }
}

object EncyclopediaOfMathExtractor extends WebExtractor {
  val key = "Encyclopedia of Math"
  val scheme = "https"
  override val dontpull = true
  def content(pm : Elem, uri : String, h : HTML) = {
    pm match {
      case ht @ <html>{hbd @ _*}</html> =>
        //val content = retrieve(ht,"body",("div","id","content"),("div","id","bodyContent"),("div","id","mw-content-text"))
        h.a(uri) { h.text { uri } }
      // TODO: I get malformed html back -.-
    }
  }
}

object NLabExtractor extends WebExtractor {
  val key = "nLab"
  val scheme = "https"
  def content(pm : Elem, uri : String, h : HTML) = {
    pm match {
      case ht @ <html>{hbd @ _*}</html> =>
        val content = retrieve(ht,"body",("div","id","Container"),("div","id","Content"),("div","id","revision"))
        var ret : List[Node] = Nil
        var done = false
        content.child.foreach(n => if(!done) n match {
          case d @ <div>{s @ _*}</div> if (d \ "@class").text == "rightHandSide" =>
          case d @ <div>{s @ _*}</div> if (d \ "@class").text contains "toc" =>
          case d @ <h1>{s @ _*}</h1> =>
          case d @ <div>{s @ _*}</div> if (d \ "@id").text == "contents" =>
          case d @ <h2>{s @ _*}</h2> if ret.length>1 => done = true
          case d @ <h2>{s @ _*}</h2> if (d \ "@id").text contains "related" => done = true
          case nod if nod.toString.trim != "" => ret ::= nod
          case _ =>
        })
        val nret = ret.reverse.map(_.toString.replaceAll(
          "href=\"/","href=\"https://ncatlab.org/").replaceAll(
          "src=\"/","src=\"https://ncatlab.org/").replaceAll("href=\"#","href=\"" + uri + "#"))
        h.literal(nret.mkString(""))
    }
  }
}