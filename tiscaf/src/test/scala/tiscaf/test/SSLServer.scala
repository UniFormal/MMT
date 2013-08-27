package tiscaf
package test

import java.security._

object SSLServer extends App with HServer {

  def ports = Set(8080)
  def apps = List(simpleApp)

  override def ssl = List(new HSslContext {
    def keystore = {
      val ks = KeyStore.getInstance("JKS")
      val stream = getClass.getResourceAsStream("/test-keystore.jks")
      try {
        ks.load(stream, null)
      } finally {
        if (stream != null)
          stream.close
      }
      ks
    }
    def passphrase = "123456"
    def port = 8443
  })

  object simpleApp extends HApp {
    def resolve(req: HReqData) = Some(new HSimpleLet {
      def act(talk: HTalk) {
        talk.setContentLength(6 + talk.req.uriPath.length)
          .write("path: " + talk.req.uriPath)
      }
    })
  }

  start

}
