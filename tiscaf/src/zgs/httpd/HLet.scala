package zgs.httpd


trait HLet {
  
  //------------------- to implement ------------------------
  
  def act(talk : HTalk) : Unit
  
  //-------------------- to override ------------------------
  
  def before : Seq[HLet] = Nil
  def partsAcceptor(reqInfo : HReqHeaderData) : Option[HPartsAcceptor] = None // for multipart requests
  
  //------------------------ few helpers --------------------
    
  protected def err(status : HStatus.Value, msg : String, tk : HTalk) = new let.ErrLet(status, msg) act(tk)
  protected def err(status : HStatus.Value, tk : HTalk)               = new let.ErrLet(status) act(tk)
  protected def e404(tk : HTalk)                                      = err(HStatus.NotFound, tk)
  
  protected def redirect(uriPath : String, tk : HTalk) = new let.RedirectLet(uriPath) act(tk)
  
  protected def sessRedirect(uriPath : String, tk : HTalk) : Unit =  {
    val parts = uriPath.split("\\?", 2)
    val url = parts(0) + ";" + tk.ses.idKey + "=" + tk.ses.id + {
      if (parts.size == 2) "?" + parts(1)
      else ""
    }
    new let.RedirectLet(url) act(tk)
  }
  
  protected def delegate(toLet : HLet, tk : HTalk) : Unit =
    if (toLet.before.forall(be => { be.act(tk); !tk.isClosed})) toLet.act(tk)
  
}
