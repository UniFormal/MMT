package info.kwarc.mmt.argsemcomp
import info.kwarc.mmt.api.utils._

import com.spotify.docker.client.DefaultDockerClient
import com.spotify.docker.client.messages.ContainerConfig
import com.spotify.docker.client.DockerClient.LogsParam
import info.kwarc.mmt.api.web.GraphSolverExtension

class ArgumentationSemanticComputer extends GraphSolverExtension {
  val key = "default"
  def apply (f: JSON, sem: String, comp: String = "default") :JSON = {
    println("Starting Argumentation Semantic Computer")
    val ret = TgfToJson(CallComputer(JsonToTgf(f), sem, comp))
    println("return" + ret)
    ret}

  def JsonToTgf (f: JSON) : List[String] = {
    val edgelist: JSON = f(Left("edges")).getOrElse(return List())
    val includes: List[String] = edgelist match {
      case a: JSONArray => for (edge: JSON <- a if edge(Left("style")).getOrElse(return List()) == JSONString("include")) yield edge(Left("from")).toString + " " + edge(Left("to")).toString
      case _ => List()
    }
    println("includes" + includes)
    includes
  }

  def CallComputer (tgf: List[String], semantic: String, computer: String) : List[String] = {
    println("Calling computer")
    val inputfile = File("mmt-argsemcomp\\src\\info.kwarc.mmt.argsemcomp\\input.tgf")
    File.WriteLineWise(inputfile, tgf)
    println(inputfile)
    //val writer = new StandardPrintWriter (inputfile, false)
    //for (item:String <- tgf ) writer.write(item +"\n")
    //writer.close()
    File.ReadLineWise(inputfile)(println)
    println("Should have printed lines")
    //for(line <- Source.fromFile("input.tgf").getLines())
      //println(line)

    val docker = DefaultDockerClient.fromEnv().build()
    //val config = ContainerConfig.builder().image(computer).cmd("docker run -v "+ inputfile.getAbsolutePath + ":inputfile.tgf " + computer + "	600	-f inputfile.tgf	-fo	tgf	-p "+ semantic).build()
    val config = ContainerConfig.builder().image(computer).cmd("docker run -v //c\\mmt2\\MMT\\src\\mmt-argsemcomp\\src\\info.kwarc.mmt.argsemcomp\\input.tgf:inputfile.tgf " + computer + "	600	-f inputfile.tgf	-fo	tgf	-p "+ semantic).build()
    val container = docker.createContainer(config)
    val id : String = container.id()
    print(id)
    docker.startContainer(id)
    println("Docker container started and input file deleted")
    var logs : String = null
    try {
      val stream  = docker.logs(id, LogsParam.stdout, LogsParam.stderr)
      try
        logs = stream.readFully
      finally if (stream != null) stream.close()
    }
    println("docker logs " + logs)
    //docker.killContainer(id)
    docker.removeContainer(id)
    docker.close
    //inputfile.delete()
    println("Removed docker container closed the docker client and deleted input file")
    val accepted: List[String] = logs.split(" ").toList
    println("accepted first attempt" + accepted)
    accepted
  }

  def TgfToJson (tgf: List[String]) : JSON = null

}

