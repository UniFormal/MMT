package zgs.httpd


protected object HResolver {

  private object errApp extends HApp {
    override def tracking  = HTracking.NotAllowed
    override def keepAlive = false
    override def chunked   = false
    override def buffered  = false 
    override def gzip      = false
    def resolve(req : HReqHeaderData) = sys.error("not used")
    val hLet = new let.ErrLet(HStatus.NotFound)
  }
  
  def resolve(apps : Seq[HApp], req : HReqHeaderData) : (HApp, HLet) = {
    @scala.annotation.tailrec
    def doFind(rest : Seq[HApp]) : (HApp,HLet) = rest match {
      case Nil     => (errApp, errApp.hLet)
      case a :: as => a.resolve(req) match {
        case Some(let) => (a, let)
        case None      => doFind(as)
      }
    }
    doFind(apps)
  }
}
