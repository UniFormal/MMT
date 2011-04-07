package test

import mizar.translator._
import mizar.objects._
import mizar.reader._
import mmt.objects._
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.presentation._
import info.kwarc.mmt.api.lf._
import scala.xml._

object Main {
  		
  def main(args: Array[String]): Unit = {
		var name = "tarski"  
	    var base = "mml/"
	    
	    try {
			name = args(0)
	    } catch {
	 		case e: Exception => println("<!--input file not given, using default: "  + name +  " -->")
	    }
		
	    Translator.translationInit()
	    Translator.translateArticle(base, name.toUpperCase())

  }

}
