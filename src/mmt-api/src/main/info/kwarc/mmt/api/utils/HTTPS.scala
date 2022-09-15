package info.kwarc.mmt.api.utils

import java.security.cert._
import javax.net.ssl._
import java.security._

/** a trust manager that does nothing */
class TrustAllX509TrustManager extends X509TrustManager {
    def getAcceptedIssuers = Array()
    def checkClientTrusted(certs: Array[X509Certificate], authType: String): Unit = {}
    def checkServerTrusted(certs: Array[X509Certificate], authType: String): Unit = {}
}

/** Some sites use certificates that are not trusted by the JRE by default.
 *  The clean solution would be to add the appropriate certificates either to the local Java keystore directory or at runtime.
 *  As a quick hack, this code simply disables all default certificate checking.
 */
object TrustAllX509TrustManager {
  private var active = false
  private object hv extends HostnameVerifier {
     def verify(string: String, ssls: SSLSession) = true
  }
  /** configures Java so that it trusts all HTTPS connections (idempotent) */
  def trustAll: Unit = {
     if (!active) {
       val sc = SSLContext.getInstance("TLS")
       sc.init(null, Array(new TrustAllX509TrustManager), new SecureRandom)
       HttpsURLConnection.setDefaultSSLSocketFactory(sc.getSocketFactory)
       HttpsURLConnection.setDefaultHostnameVerifier(hv)
     }
     active = true
  }
}

