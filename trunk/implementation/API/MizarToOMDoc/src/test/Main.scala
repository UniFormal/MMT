package test

import mizar.translator._
import mizar.objects._
import mizar.reader._
import mmt.objects._
import jomdoc._
import jomdoc.documents._
import jomdoc.utils._
import jomdoc.frontend._
import jomdoc.symbols._
import jomdoc.libraries._
import jomdoc.modules._
import jomdoc.objects._

object Main {

  def main(args: Array[String]): Unit = {
	  var source = "test-data/tarski.xmlabs";
		try {
			source = args(0)
		} catch {
			case e: Exception => println("<!--input file not given, using default: " + source +  " -->")
		}
		val src = scala.io.Source.fromFile(source)
		val cp = scala.xml.parsing.ConstructingParser.fromSource(src, false)
		val input : scala.xml.Node = cp.document()(0)
		src.close
		ArticleParser.parseArticle(input);
		val art : Option[MizArticle] = ParsingController.currentArticle
		art match {
			case None => println("Error Parsing article")
			case Some(article) => {
				ArticleTranslator.translateArticle(article)
				val pp = new scala.xml.PrettyPrinter(100,2)
				val out = new java.io.FileWriter("output.xml")
		        val art_th = TranslationController.controller.get(new DPath(new xml.URI(article.title)) ? article.title)
				
		        println(art_th.toString)
		        out.write(pp.format(art_th.toNode))
				out.close
			}
		}

  }

}