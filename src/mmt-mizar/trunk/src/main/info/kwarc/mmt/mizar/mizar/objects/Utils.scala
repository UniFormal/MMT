package info.kwarc.mmt.mizar.mizar.objects

object MizXML {
  def set : MizTyp = new MizTyp("HIDDEN", "M", 1,"", new MizCluster(Nil) :: new MizCluster(Nil) :: Nil, Nil, None)
}

/**
 * Represents a reference to the .miz source file
 * @param line source line
 * @param col source column 
 */
case class SourceRef(line : Int, col : Int)

case class SourceRegion(start : SourceRef, end : SourceRef)

class Symbol(val kind : String, val symbolnr : Int, val name : String) {
  var aid : Option[String] = None
  var formatnr : Int = -1
  var absnr : Int = -1
  
  def setAid(s : String) {
	  aid = Some(s)
  }
  
  def setFormatnr(nr : Int) = {
    formatnr = nr
  }
  
  def setAbsnr(i : Int) = {
	  absnr = i
  }
  	
}

class Dictionary(var symbols : List[Symbol]) {

	
  def addSymbols(s : List[Symbol]) = {
    symbols = s ::: symbols
  }

  
  def addAbsnr(kind : String, formatnr : Int, absnr : Int) {
	 symbols.find(s => s.kind == kind  && s.formatnr == formatnr) match {
      case None => None
      case Some(s) => s.setAbsnr(absnr)
    } 
  }
  	
  def addFormatnr(kind : String, symbolnr : Int, formatnr : Int) = {
    symbols.find(s => s.kind == kind && s.symbolnr == symbolnr) match {
      case None => None
      case Some(s) => s.setFormatnr(formatnr)
    }
  }

  def addPattern(kind : String, formatnr : Int, aid : String) {
	symbols.find(s => s.kind == kind && s.formatnr == formatnr) match {
      case None => None
      case Some(s) => s.setAid(aid)
    }  
  }
  
  def getNameBySymbolnr(kind : String, symbolnr : Int) : Option[String] = {
    symbols.find(s => s.kind == kind && s.symbolnr == symbolnr) match {
      case None => None
      case Some(s) => Some(s.name)
    }
  }
  
  def getNameByFormatnr(kind : String, formatnr : Int) : Option[String] = {
	  symbols.find(s => s.kind == kind && s.formatnr == formatnr) match {
	 	  case None => None
	 	  case Some(s) => Some(s.name)
	  }
  }
  
  def getAidBySymbolnr(kind : String, symbolnr : Int) : Option[String] = {
	  symbols.find(s => s.kind == kind && s.symbolnr == symbolnr) match {
	 	  case None => None
	 	  case Some(s) => s.aid
	  }
  }
  	
  def getNameByAbsnr(aid : String, kind : String, absnr : Int) : Option[String] = {
	  symbols.find(s => s.kind == kind && s.absnr == absnr && s.aid == Some(aid)) match {
	 	  case None => None
	 	  case Some(s) => Some(s.name)
	  }
  }
  
  /**
   * Returns a variable name that is free in any mizar formula by excluding all dictionary names
   * Is used when the translation adds new bindings to avoid clashes with mizar variable names.
   */
  def getFreeVar() : String = {
	val baseName = "x"
	var nr = 0	
	while (symbols.map(x=> x.name).contains(baseName + nr)) {
		nr = nr + 1
	}
	baseName + nr
  }
  
  def clear() = {
    symbols = Nil
  }
  
}
