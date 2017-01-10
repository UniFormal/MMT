package info.kwarc.mmt.mizar.objects

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

class Format(val kind : String, val nr : Int, val symbolnr : Int, val argnr : Int, val leftargnr : Int, val rightsymbolnr : Option[Int]) {
  private var _aid : Option[String] = None
  private var _absnr : Int = -1
  private var _symbol : Symbol = null
  private var _rightsymbol : Option[Symbol] = None
  
  def aid : Option[String] = _aid
  def absnr : Int = _absnr
  def symbol  : Symbol = _symbol
  def rightsymbol  = _rightsymbol
  
  def setAid(aid : String) = _aid = Some(aid)
  def setAbsnr(i : Int) = _absnr = i
  def setSymbol(s : Symbol) = _symbol = s
  def setRightsymbol(sO : Symbol) = _rightsymbol = Some(sO)
  override def toString = s"F = {kind: $kind, nr: $nr, symnr: $symbolnr, argnr: $argnr, largnr: $leftargnr, rsymbnr: $rightsymbolnr, aid: $aid, absnr: $absnr, sym: $symbol}"
}

class Symbol(val kind : String, val nr : Int, val name : String) {
  override def toString = s"S = {kind: $kind, nr: $nr, name: $name}"
}

class Dictionary(var formats : List[Format]) {
  var symbols : List[Symbol] = Nil
  
  def addFormat(f : Format) = {
    formats = f :: formats
  }
  
  def getFormat(kind : String, formatnr : Int) : Option[Format] = {
    formats.find(f => f.kind == kind && f.nr == formatnr)
  }
  
  def addAbsnr(kind : String, formatnr : Int, absnr : Int) {
    getFormat(kind, formatnr).map(_.setAbsnr(absnr)) 
  }
  
  def addSymbol(s : Symbol) {
    def matches(f : Format) : Boolean = {
      //numbers must match
      if (f.symbolnr != s.nr) return false
      //exceptions for primitives
      if (f.kind == "K" && (s.kind == "{" || s.kind == "[")) return true
      //general rule
      f.kind == s.kind
    }
    def matchesRight(f : Format) : Boolean = f.rightsymbolnr match {
      case None => false 
      case Some(rightsymbolnr) =>
        //numbers must match
        if (rightsymbolnr != s.nr) return false
        //exceptions for primitives
        if (f.kind == "K" && (s.kind == "}" || s.kind == "]")) return true
        //general rule
        f.kind == s.kind
    }
    
    symbols ::= s
    formats.find(matches).map(_.setSymbol(s))
    formats.find(matchesRight).map(_.setRightsymbol(s))     
  }

  def addPattern(kind : String, formatnr : Int, aid : String, absnr : Int) {
    getFormat(kind, formatnr) foreach {f =>
      f.setAid(aid)
      f.setAbsnr(absnr)
    }
  }
  

  /*
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
  */
  
  
  def getFormatByAbsnr(aid : String, kind : String, absnr : Int) : Option[Format] = {
    formats.find(f => f.kind == kind && f.absnr == absnr && f.aid == Some(aid))
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
    formats = Nil
  }
  
}
