package mizar.objects


class Symbol(val kind : String, val symbolnr : Int, val name : String) {
  var formatnr : Int = -1
  def setFormatnr(nr : Int) = {
    formatnr = nr
  }

}

class Dictionary(var symbols : List[Symbol]) {
  def addSymbols(s : List[Symbol]) = {
    symbols = s ::: symbols
  }

  def addFormatnr(kind : String, symbolnr : Int, formatnr : Int) = {
    symbols.find(s => s.kind == kind && s.symbolnr == symbolnr) match {
      case None => None
      case Some(s) => s.setFormatnr(formatnr)
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
  
}
