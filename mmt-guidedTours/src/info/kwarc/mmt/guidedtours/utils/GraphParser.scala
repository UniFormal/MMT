package info.kwarc.mmt.guidedtours.utils

import scala.collection.mutable._

object Utilities {
  
  def parseGraph(location: String) : (List[String], List[(String, String)]) = {
    
    var buffer : ListBuffer[(String, String)] = new ListBuffer()
    var theories : ListBuffer[String] = new ListBuffer()
    
    for(line <- scala.io.Source.fromFile(location).getLines()) {
      val reg = """(.*) ->(.*)""".r
      
      line match {
        case reg(first, second) => {
           
           val after = {
             if (second.length > 0) 
               second.substring(1) 
             else second
           }
           
           if(theories.indexOf(first) < 0) {
             theories.append(first)
           }
           
           val deps = after.split(",")
           
           deps.foreach { 
             x => {
               if(theories.indexOf(x) < 0) {
                 theories.append(x)
               }
               buffer.append((first, x)) 
             } 
           }
         }
         
         case default => "NONE\n"
      }
    }
    //println(theories)
    (theories.toList, buffer.toList)
  }
  
  def getMatrixFromGraph(theories: List[String], buffer: List[(String, String)]) : Array[Array[Double]] = {
    var result = Array.fill(theories.length)(Array.fill(theories.length)(0.0))
    //println(Array(1,3,4).deep.mkString)
    //new Array[Array[Double]](theories.length)
    buffer.foreach {
      case (first, second) => {
        val indOne = theories.indexOf(first)
        val indTwo = theories.indexOf(second)
        
        result(indOne)(indTwo) = 1.0
        result(indTwo)(indOne) = 1.0
      }
    }
   
    result
  }
}