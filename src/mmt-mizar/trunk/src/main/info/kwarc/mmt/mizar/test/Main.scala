package info.kwarc.mmt.mizar.test

import info.kwarc.mmt.mizar.mizar.translator._
import info.kwarc.mmt.mizar.mizar.objects._
import info.kwarc.mmt.mizar.mizar.reader._
import info.kwarc.mmt.mizar.mmt.objects._
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.patterns._
import info.kwarc.mmt.api.presentation._
import info.kwarc.mmt.api.lf._
import scala.xml._

object Main {
  		
  def dumpPatterns() : Unit = {
    var p : scala.collection.mutable.LinkedList[Node] = new scala.collection.mutable.LinkedList()
    p = p :+ DefPatterns.MizAttrIsCompleteDef.toNode
    p = p :+ DefPatterns.MizAttrIsPartialDef.toNode
    p = p :+ DefPatterns.MizAttrMeansCompleteDef.toNode
    p = p :+ DefPatterns.MizAttrMeansPartialDef.toNode
    p = p :+ DefPatterns.MizFuncIsCompleteDef.toNode
    p = p :+ DefPatterns.MizFuncMeansCompleteDef.toNode
    p = p :+ DefPatterns.MizFuncIsPartialDef.toNode
    p = p :+ DefPatterns.MizFuncMeansPartialDef.toNode
    p = p :+ DefPatterns.MizModeIsCompleteDef.toNode
    p = p :+ DefPatterns.MizModeIsPartialDef.toNode
    p = p :+ DefPatterns.MizModeMeansCompleteDef.toNode
    p = p :+ DefPatterns.MizModeMeansPartialDef.toNode
    p = p :+ DefPatterns.MizPredIsCompleteDef.toNode
    p = p :+ DefPatterns.MizPredIsPartialDef.toNode
    p = p :+ DefPatterns.MizPredMeansCompleteDef.toNode
    p = p :+ DefPatterns.MizPredMeansPartialDef.toNode
    p = p :+ DefPatterns.MizStructDef.toNode
    
    p = p :+ SchemePatterns.MizSchemeDef.toNode
    
    p = p :+ artPatterns.AntonymicNotation.toNode
    p = p :+ artPatterns.Lemma.toNode
    p = p :+ artPatterns.SynonymicNotation.toNode

    val out = new java.io.FileWriter(docPath)
    val docPath = "/home/mihnea/kwarc/omdoc/" + "content/http..latin.omdoc.org/foundations/mizar/mizar-patterns.omdoc" 
    val base = mmt.baseURI / "foundations" / "mizar" 
    val pp = new scala.xml.PrettyPrinter(100,2)
	
    val nd : scala.xml.Node = 
    	<omdoc xmlns="http://omdoc.org/ns" xmlns:om="http://www.openmath.org/OpenMath">
    		<theory name="mizar-patterns" base={base.toString}>
    		  <include from="?HIDDEN"/>
    		{p}
    		</theory>
    	</omdoc>

    val docNode = pp.format(nd)
    out.write("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" + docNode.toString)
    out.close
  }
  
  def main(args: Array[String]): Unit = {
		MizarCompiler.init()
	    
	    //dumpPatterns()
	    
	    val f = File("/home/mihnea/kwarc/oaff/mml/source/")
	    val files = f.toJava.listFiles().map(f => File(f)).filter(x => MizarCompiler.isApplicable(x.toJava.getName())).toList
	    MizarCompiler.compileLibrary(files)
	    
	    
	    //val tf = File("/home/mihnea/kwarc/oaff/mml/source/yellow_9.miz")
	    //MizarCompiler.compile(tf,tf)
	    
	    
  }

}
