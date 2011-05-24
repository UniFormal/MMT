package info.kwarc.mmt.lf

case class URI(var uri : java.net.URI) {
      def this(s : String) = this(new java.net.URI(s))
      def this(scheme : String, authority : String, path : String, query : String) = this(new java.net.URI(scheme, authority, path, query, null))
      private def up(s : String) : String = {
      	 val p = s.lastIndexOf("/")
         if (p == -1) ""
         else if (p == 0) "/"
         else s.substring(0,p)
      }
      def /(n : String) = new URI(uri.toString + "/" + n)
      def ?(n : String) = new URI(uri.toString + "?" + n)
      def ^ : URI = new URI(this.getScheme, this.getAuthority, up(this.getPath), null)      
      def resolve(s : String) : URI = resolve(new java.net.URI(s))
      def resolve(u : java.net.URI) : URI = {
         //resolve implements old URI RFC, therefore special case for query-only URI needed
         if (u.getScheme == null && u.getAuthority == null && u.getPath == "")
            new URI(new java.net.URI(uri.getScheme, uri.getAuthority, uri.getPath, u.getQuery, u.getFragment))
         else
            new URI(uri.resolve(u))
      }
      def getBase : info.kwarc.mmt.lf.URI = new java.net.URI(this.getScheme, this.getAuthority, this.getPath, null, null)
      def normalize() : java.net.URI = uri.normalize()
      override def toString = uri.toString
}

object URI {
      implicit def toMyURI(u : info.kwarc.mmt.lf.URI) : java.net.URI = u.uri
      implicit def javaURItoMyURI(u : java.net.URI) = new URI(u)
}

object testme extends Application {
  val u = new URI("file:/C:/courses/Twelf/logics/a?m")
  println(URI(u) / "s")
  println(URI(u) ? "s")
}