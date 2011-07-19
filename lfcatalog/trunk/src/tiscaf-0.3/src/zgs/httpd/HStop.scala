package zgs.httpd


import java.net.Socket

// embryonic way to stop

final class HStop(port : Int) {

  def stop {
    val s = new Socket("localhost", port)
    s.getOutputStream.write("stop".getBytes("ISO-8859-1"))
    s.close
  }
}
