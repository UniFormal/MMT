package info.kwarc.mmt.odk.SCSCP

import java.net.{HttpURLConnection, URL}
import java.util.Scanner
import java.util.{List => jList}

import scala.collection.JavaConversions.{asScalaBuffer, mapAsScalaMap}

/**
  * Created by twiesing on 31/05/16.
  */
object utils {
  def post_url(url : URL, data: String, cookies : List[String] = Nil, charset : String = "UTF-8", headers : Map[String, List[String]] = Map()) : (Int, Map[String, List[String]], String, List[String]) = {

    // create a connection
    val conn = url.openConnection.asInstanceOf[HttpURLConnection]

    // enable post and set encoding
    conn.setDoOutput(true)

    // set encoding headers
    conn.setRequestProperty("Accept-Charset", charset)
    conn.setRequestProperty("Content-Type", "application/x-www-form-urlencoded;charset=" + charset)


    // add the cookies and headers
    cookies.foreach(cookie => conn.addRequestProperty("Cookie", cookie.split(";", 2)(0)))
    headers.foreach(header => header._2.foreach(conn.setRequestProperty(header._1, _)))

    // create an output stream and write to it
    val output = conn.getOutputStream
    output.write(data.getBytes(charset))

    // read response state and cookies
    val state = conn.getResponseCode

    // response headers
    val response_fields : Map[String, List[String]] = mapAsScalaMap(conn.getHeaderFields).mapValues(list2List).toMap
    val response_cookies : List[String] = response_fields("Set-Cookie")

    // and turn it into a string
    val response = conn.getInputStream
    val scanner = new Scanner(response)
    val response_body = scanner.useDelimiter("\\A").next()

    // return body, code and Cookies
    (state, response_fields, response_body, response_cookies)
  }

  private def list2List[A](l : jList[A] ) : List[A] = asScalaBuffer(l).toList
}
