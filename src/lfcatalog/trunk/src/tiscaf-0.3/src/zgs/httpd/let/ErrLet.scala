package zgs.httpd.let

import zgs.httpd._


class ErrLet(status : HStatus.Value, msg : String = "") extends HLet {
  
  def act(tk : HTalk) : Unit = {
    val add = if (msg.length == 0) "" else ", " + msg
    val toWrite = (HStatus.asString(status) + add + "\n").getBytes("ISO-8859-1")
    
    tk.setStatus(status)
      .setContentType("text/plain")
      .setContentLength(toWrite.size) // if not chunked
      .write(toWrite)
      .close
    
    tk.ses.invalidate
  }
}
