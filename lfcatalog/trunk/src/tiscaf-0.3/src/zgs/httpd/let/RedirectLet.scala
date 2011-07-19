package zgs.httpd.let

import zgs.httpd._


class RedirectLet(toUri : String) extends HLet {
  
  def act(tk : HTalk) {
    tk.setContentLength(0)
      .setContentType("text/html")
      .setHeader("Location", toUri)
      .setStatus(HStatus.MovedPermanently)
      .close
  }

}
