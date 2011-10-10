package zgs.httpd

object HTree {
  implicit def string2lay(aDir : String) = new HTree { override def dir = aDir }
  
  def stub(text : String) : HLet = new HLet {
    def act(tk : HTalk) {
      val out = text.getBytes("UTF-8")
      tk.setContentLength(out.size) // if not buffered
        .setContentType("text/plain; charset=utf8")
        .write(out)
        .close
    }
  }
}

trait HTree { self =>

  def dir : String       = ""
  def let : Option[HLet] = None
  def lays : Seq[HTree]   = Nil

  final def !(addLet : => HLet) = new HTree {
    override def dir  = self.dir
    override def let  = Some(addLet)
    override def lays = self.lays
  }

  final def +=(addLays : HTree*) = new HTree {
    override def dir  = self.dir
    override def let  = self.let
    override def lays = addLays.toList
  }

  final def resolve(dirs : Seq[String]) : Option[HLet] = dirs.filter(_.length != 0).toList match {
    case Nil  => if (self.dir.length == 0) self.let else None // uri == ""
    case list =>
      // not-tail recursion
      def nextDir(rest : Seq[String], lay : HTree) : Option[HTree] = lay.dir match {
        case s if s == rest.head =>
          if (rest.size > 1) lay.lays.find(_.dir == rest.tail.head).flatMap(nextDir(rest.tail, _))
          else Some(lay) // it's a leaf in dirs - the only place of possible success
        case "" => lay.lays.find(_.dir == rest.head).flatMap(nextDir(rest, _))
        case _  => None
      }
      nextDir(list, self).flatMap(_.let)
  }

  final def resolve(uriPath : String) : Option[HLet] = resolve(uriPath.split("/"))
}
