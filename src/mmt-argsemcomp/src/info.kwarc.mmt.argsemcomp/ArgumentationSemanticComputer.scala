package info.kwarc.mmt.argsemcomp
import info.kwarc.mmt.api.utils._
import java.io._

import com.spotify.docker.client.DefaultDockerClient
import com.spotify.docker.client.messages.ContainerConfig
import com.spotify.docker.client.DockerClient.LogsParam
import info.kwarc.mmt.api.web.GraphSolverExtension

class ArgumentationSemanticComputer (val f: JSON, val sem: String, val computer: String = "default") extends GraphSolverExtension {
  val key = "default"
  def apply (f: JSON, sem: String, comp: String = "default") :JSON = {TgfToJson(CallComputer(JsonToTgf(f), sem, comp))}

  def JsonToTgf (f: JSON) : List[String] = {
    val edgelist: JSON = f("edges").getOrElse(return List())
    val includes: List[String] = {
      for (edge:JSONObject <- edgelist if edge("style").getOrElse(return List()) == JSONString("include")) yield edge("from").toString +" "+ edge("to").toString
    }
    println(includes)
    includes
  }

  def CallComputer (tgf: List[String], semantic: String, computer: String) : List[String] = {
    val inputfile = new File("input.tgf")
    val writer = new PrintWriter (inputfile)
    for (item:String <- tgf ) writer.write(item +"\n")
    writer.close()
    for (line<-inputfile) println(line)
    val docker = DefaultDockerClient.fromEnv().build()
    val config = ContainerConfig.builder.image(computer).cmd("-v inputfile.tgf:"+inputfile.getName + " " + computer + "	600	-f inputfile.tgf	-fo	tgf	-p "+ semantic).build
    val container = docker.createContainer(ContainerConfig.builder.build)
    docker.startContainer(computer)
    inputfile.delete()

    var logs : String = null
    try {
      val stream  = docker.logs(computer, LogsParam.stdout, LogsParam.stderr)
      try
        logs = stream.readFully
      finally if (stream != null) stream.close()
    }
    docker.killContainer(computer)
    val accepted: List[String] = logs.split(" ").toList
    println(accepted)
    accepted
  }

  def TgfToJson (tgf: List[String]) : JSON = null

}

