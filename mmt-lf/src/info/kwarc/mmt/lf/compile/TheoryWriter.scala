package info.kwarc.mmt.lf.compile

import info.kwarc.mmt.lf.compile._


class TheoryWriter {

  /*
   * sorts out where each declaration should go
   * 
   */
  def sorter(dec : DECL) : Unit = {
    
    
    dec match {
      
      case ADT(a,l) => println("data")
        
      case _ => println("don't know")
    }
    
  }
  
  
  
  /*
   * takes a list of DECLarations and an output file name, writes declarations to a file
   */
  def write(deL : List[DECL], out : String) : Unit = {
    
    
  }
  
}