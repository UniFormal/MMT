package info.kwarc.mmt.oeis

import java.io.File
import java.net.URL
import java.util.Calendar

import scala.io.Source
import scala.util.Random
import scala.xml.{Elem, XML}
import parser.{DocumentParser}


object Library {
  //store everything, check before crawling
  //hardcoded for now
  val dictionary = Source.fromFile("../../mmt-oeis/resources/dictionary").getLines().map(_.trim).toSet
  val docParser = new DocumentParser(dictionary)

  private def getURL(entryID : String) : URL = new URL("""http://oeis.org/search?q=id:"""+entryID+"""&fmt=text""")

  //will just give id of number-th OEIS entry
  private def createID( number : String) : String = "A"+"000000".substring(0,6-number.length) + number

  def crawlDocuments(from : Int, to :Int) = {
    if(from < 1){
      throw new Error("There is no entry "+from+" in OEIS!")
    }

    from to to foreach(i =>{
      val theory = createID(i.toString)
      val file = Source.fromURL(getURL(theory))

      printToFile(new File("resources/"+theory)){
        p => file.getLines().foreach(p.println)
      }

      if(i % 10 == 0){
        println("Fetching entry "+ theory)
      }

      file.close()
    })
  }

  def crawlXML(from : Int, to : Int)= {
    if(from < 1){
      throw new Error("There is no entry "+from+" in OEIS!")
    }

    from to to foreach(i =>{
      val theory = createID(i.toString)
      val file = Source.fromURL(getURL(theory))

      val xml = docParser.fromReaderToXML(file)

      if(i % 10 == 0){
        println("Fetching entry "+ theory)
      }

      file.close()
      writeXML(xml, theory)
    })
  }

  def crawlXMLLocal(from : Int, to : Int)= {
    if(from < 1){
      throw new Error("There is no entry "+from+" in OEIS!")
    }

    from to to foreach(i =>{
      val theory = createID(i.toString)
      val fileLoc = "/home/enxhi/github/OEISLAB/oeis/source/oeis_source/"+theory+".txt"
      val ioFile = new java.io.File(fileLoc)
      if(ioFile.exists) {
        val file = Source.fromFile(ioFile)
        val xml = docParser.fromReaderToXML(file)

        if (i % 1000 == 0) {
          println("Fetching entry " + theory)
        }

        file.close()
        writeXML(xml, theory)
      }else{
        println("File doesn't exists: " + theory)
      }
    })
  }

  def crawlText(from : Int, to : Int) = {
    if(from < 1){
      throw new Error("There is no entry "+from+" in OEIS!")
    }

    from to to foreach(i =>{
      val theory = createID(i.toString)
      val doc: List[String] = docParser.getFormulas(Source.fromURL(getURL(theory)))

      if(i % 10 == 0){
        println("Fetching entry "+ theory)
      }

      writeFormula(doc, theory)
    })
  }
  def getXML(entry : Int) : Elem = {
    val id = createID(entry.toString)
//    if(storage.get(id).isEmpty){
      docParser.fromReaderToXML(Source.fromURL(getURL(id)))
//    }else{
//      storage.get(id).get.toNode
//    }
  }

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit): Unit = {
    val print = new java.io.PrintWriter(f)
    try {
      op(print)
    } finally {
      print.close()
    }
  }

  def writeXML(xml : Elem, theory : String) = {
    XML.save("xml_out/" + theory +".omdoc", xml, "UTF-8", true, null)
  }

  def writeFormula(formulas : List[String], theory : String) : Unit = {
    printToFile(new File("xml_out/"+theory)) { p =>
      formulas.foreach(p.println)
    }
  }

  def main(args : Array[String]) = {
//    crawlXMLLocal(1, 3000)
    println(Calendar.getInstance().getTime())
    val documents = 5000
    val max = 255000
    val scriptPath = "/home/enxhi/github/logs/oeis_sc"
    println(1 to 1)
    val rndm = new Random()
    1 to documents foreach { x =>
      val n = rndm.nextInt(max) + 1
      val theory = createID(n.toString)
      printToFile(new File(scriptPath))( p => "cp ../../oeis/source/oeis_omdoc/" + theory + " ../source/" )
      crawlXMLLocal(n,n)
    }

    println(docParser.textParser.succeded)
    println(docParser.textParser.calls)
    println(docParser.textParser.exceptions)
    println(Calendar.getInstance().getTime())
  }
}
