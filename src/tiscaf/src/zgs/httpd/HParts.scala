package zgs.httpd


trait HPartDescriptor {
  def header(key : String) : Option[String]
  def headerKeys : scala.collection.Set[String]
  override def toString = (for(k <- headerKeys) yield { k + " -> " + header(k).get }).mkString(", ")
}


// any 'false' disposes the connection, declineAll will not be 
// called - i.e. acceptor must do all cleanup work itself at 'false'

abstract class HPartsAcceptor(reqInfo : HReqHeaderData) {
  
  // to implement
  
  def open(desc : HPartDescriptor) : Boolean // new part starts with it...
  def accept(bytes : Array[Byte]) : Boolean  // ... takes bytes (multiple calls!)...
  def close :Unit                            // ... and ends with this ...
  def declineAll : Unit                      // ... or this one apeals to abort all parts
  
  // to override
  
  def headerEncoding : String = "UTF8" 
}

final private class HPart(acceptor : HPartsAcceptor) {
  
  def takeHeader(strings : List[String]) : Boolean = 
    if (strings.size == 0) false else {
      val header = strings.map(s => splitString(s)).filter(_.isDefined).map(_.get)
      if (header.size == 0) false else { // 'content-disposition' is expected at least
        desc.he ++= header
        acceptor.open(desc)
      }
    }
  
  def takeBytes(bytes : Array[Byte]) : Boolean = acceptor.accept(bytes)
  def close : Unit                             = acceptor.close
  def declineAll : Unit                        = acceptor.declineAll
  
  
  private def splitString(s : String) : Option[(String,String)] = {
    val idx = s.indexOf(':')
    if (idx < 1 && s.length > idx + 1) None else {
      val name = s.substring(0, idx).trim.toLowerCase
      val value = s.substring(idx + 1, s.length).trim
      Some((name, value))
    }
  }

  // the only implementation
  private val desc = new HPartDescriptor {
    val he = new scala.collection.mutable.HashMap[String, String] 
    def header(key : String) = he.get(key.toLowerCase)
    def headerKeys = he.keySet
  }
}

/* ==== stream structure after request header's CLRF CLRF:
part0
...
partN
CLRF -- bdr -- CLRF // stream end marker
==== partX structure:
partHeader
bytes
==== partHeader structure
-- bdr CLRF 
header0 CLRF
... 
headerN CLRF 
CLRF
=== */

private object HPartState extends Enumeration {
  val AtPrefix, AtHeader, WaitsHeaderBytes, InData, WaitsDataBytes, IsReady, IsInvalid = Value
}

// this object is created  for each multipart request

final private class HParts(acceptor : HPartsAcceptor, reqData : HReqHeaderData) {
  
  //------------------ init ------------------------
  
  // boundary aware
  
  private val bdrString = reqData.boundary.get
  private val prefix    = HParts.toBytes("\r\n--" + bdrString)
  private val partStart = HParts.toBytes("\r\n--" + bdrString + "\r\n")
  private val endMark   = HParts.toBytes("\r\n--" + bdrString + "--\r\n")  
  
  // request stream state
  
  private var state : HPartState.Value = HPartState.AtPrefix
  private var tail : Array[Byte]       = HParts.toBytes("\r\n") 
  private var part : Option[HPart]     = None
  
  private def resetPart : Unit = for(p<- part) { p.close; part = None }
  
  //------------------- SPI ------------------------------
  
  def processNextBytes(appendTail : Array[Byte]) : HReqState.Value = {
    tail ++= appendTail // eat it completely
    
    //println(HParts.asString(tail))
    
    // main loop: eat bytes as long as possible
    @scala.annotation.tailrec
    def doProcess : HPartState.Value = state match {
      case HPartState.AtPrefix => state = atPrefix; doProcess
      case HPartState.AtHeader => state = atHead ;  doProcess
      case HPartState.InData   => state = inData;   doProcess
      case x => x
    }
    
    state = doProcess // till WaitsXyzBytes or IsInvalid or IsReady is got
    
    // WHERE to declineAll?
    
    state match {
      case HPartState.WaitsHeaderBytes => state = HPartState.AtHeader; HReqState.WaitsForPart
      case HPartState.WaitsDataBytes   => state = HPartState.InData; HReqState.WaitsForPart
      case HPartState.IsInvalid        => for(p <- part) p.declineAll; part = None; HReqState.IsInvalid
      case HPartState.IsReady          => resetPart; HReqState.IsReady
    }
  }
  
  //------------------ internals --------------------
  
  //private def onPrefix = HParts.startsWith(tail, prefix)
  
  // results to IsInvalid, isReady or AtHeader
  private def atPrefix : HPartState.Value = {
    resetPart
    if (HParts.startsWith(tail, endMark))        HPartState.IsReady
    else if (HParts.startsWith(tail, partStart)) HPartState.AtHeader
    else                                        HPartState.IsInvalid
  }
  
  //----- in header
    
  private def atHead : HPartState.Value = {
    
    val till = tail.size - 4

    def findEol(idx : Int) : Option[Int] = (0 to till).find { idx =>
      tail(idx)     == 13 &&
      tail(idx + 1) == 10 &&
      tail(idx + 2) == 13 && 
      tail(idx + 3) == 10
    }
    
    if (till < 0) HPartState.WaitsHeaderBytes else findEol(12) match {
      case None        => HPartState.WaitsHeaderBytes
      case Some(shift) => acceptHeader(shift)
    }
    
  }
  
  private def acceptHeader(shift : Int) : HPartState.Value =
    if (shift >= HParts.maxHeaderLength) HPartState.IsInvalid else {
      val ar = tail.take(shift)
      tail = tail.slice(shift + 4, tail.size)
      val strings = HParts.asString(ar, acceptor.headerEncoding)
                          .split("\r\n").map(_.trim).filter(_.length != 0).toList.tail
      part = Some(new HPart(acceptor))
      if (part.get.takeHeader(strings)) HPartState.InData
      else HPartState.IsInvalid
    }
 
  //-------- in data
  
  private def inData : HPartState.Value = {
    
    val eatTill = HParts.keepTailPosition(tail, prefix)
    
    if (eatTill == 0) {
      if (tail.size >= prefix.size) HPartState.AtPrefix
      else HPartState.WaitsDataBytes
    }
    else { // in data
      if (part.get.takeBytes(tail.take(eatTill))) {
        tail = tail.slice(eatTill, tail.size)
        if (tail.size >= prefix.size) HPartState.AtPrefix else HPartState.WaitsDataBytes
      }
      else HPartState.IsInvalid
    }
  }

}


private object HParts {
  
  val maxHeaderLength = 1024
  
  def toBytes(s : String) = s.getBytes("ISO-8859-1")
  def asString(a : Array[Byte], enc : String) : String = new String(a, enc)
  
  def keepTailPosition(in : Array[Byte], pattern : Array[Byte]) : Int = {
    
    val inLength = in.size
    val patLength = pattern.size
    val delta = inLength - patLength
    
    // tail length > pattern length
    def findFullMatch : Option[Int] = {
      
      val patRange = 0 until patLength

      @scala.annotation.tailrec
      def tryOffset(idx : Int) : Option[Int] = if (idx > delta) None else {
        patRange.find { i => pattern(i) != in(idx + i) } match {
          case None => Some(idx)
          case _    => tryOffset(idx + 1)
        }
      }
      tryOffset(0)
    }
    

    // tail length <= pattern length
    def findPartialMatch(tail : Array[Byte]) : Int = {
      val tailLength = tail.length

      @scala.annotation.tailrec
      def tryOffset(idx : Int) : Int = if (idx == tailLength) idx else {
        (0 until (tailLength - idx)).find { i => tail(idx + i) != pattern(i) } match {
          case None => idx
          case _    => tryOffset(idx + 1)
        }
      }
      tryOffset(0)
    }
    
    
    if (delta > 0) findFullMatch match {
      case Some(idx) => idx
      case None =>
        val tail = in.slice(delta + 1, inLength)
        findPartialMatch(tail) + delta + 1
    }
    else findPartialMatch(in)
  }
  
  def hasFragment(in : Array[Byte], offset : Int, pattern : Array[Byte]) : Boolean = {
    val patLength = pattern.size
    if (offset + patLength > in.size) false
    else (0 until patLength).find { i => pattern(i) != in(offset + i) }.isEmpty
  }
  
  def startsWith(in : Array[Byte], pattern : Array[Byte]) : Boolean = hasFragment(in, 0, pattern)
  
} 

