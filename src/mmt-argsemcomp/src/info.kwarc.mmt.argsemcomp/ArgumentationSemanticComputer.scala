package info.kwarc.mmt.argsemcomp
import info.kwarc.mmt.api.utils._
import java.io._

import com.spotify.docker.client
import com.spotify.docker.client.DefaultDockerClient
import com.spotify.docker.client.messages.ContainerConfig

class ArgumentationSemanticComputer (val f: JSON, val sem: String, val computer: String = "default") {
    def JsonToTgf (f: JSONObject) : List[String] = {
      val edgelist: JSON = f("edges").getOrElse(return List())
      val includes: List[String] = {
        for (edge:JSONObject <- edgelist if edge("style").getOrElse(return List()) == JSONString("include")) yield edge("from").toString +" "+ edge("to").toString
      }
      println(includes)
      includes
    }
    def TgfToJson (tgf: List[String]) : JSONObject = null
    def CallComputer (tgf: List[String], semantic: String, computer: String) : List[String] = {
      val inputfile = new File("input.tgf")
      val writer = new PrintWriter (inputfile)
      for (item:String <- tgf ) writer.write(item +"\n")

      val docker = DefaultDockerClient.fromEnv().build()
      val config = ContainerConfig.builder.image(computer).cmd("-v inputfile:"+inputfile.getName +" DOCKER_ID/YOUR_SOLVER_REPOSITORY	600	-f	/test_frameworks/admbuster_1000.apx	-fo	apx	-p	DC-PR	-a	c408 --rm").build
      val container = docker.createContainer(ContainerConfig.builder.build)
      docker.startContainer(computer)
    }

  }

