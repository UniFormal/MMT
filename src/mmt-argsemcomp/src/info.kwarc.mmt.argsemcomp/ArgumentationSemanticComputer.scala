package info.kwarc.mmt.argsemcomp
import info.kwarc.mmt.api.utils._
import com.spotify.docker.client.DefaultDockerClient
import com.spotify.docker.client.messages.ContainerConfig
import com.spotify.docker.client.DockerClient.LogsParam
import info.kwarc.mmt.api.web.GraphSolverExtension
import net.sf.jargsemsat.jargsemsat.alg.GroundedSemantics
import net.sf.jargsemsat.jargsemsat.datastructures.DungAF

/** TO DO: Implement proper error catching
  *
  */


class ArgumentationSemanticComputer extends GraphSolverExtension {
  val key = "default"
  def apply (f: JSON, sem: String, comp: String = "default") :JSON = {
    println("Starting Argumentation Semantic Computer")
    val ret = TgfToJson(f, CallComputer(JsonToTgf(f), sem, comp))
    println("return " + ret)
    ret}

  def JsonToTgf (f: JSON) : JSON = {
    val nodelist: JSON = f(Left("nodes")).getOrElse(return JSONNull)
    val edgelist: JSON = f(Left("edges")).getOrElse(return JSONNull)
    println("nodelist" + nodelist)
    val theories: JSON = nodelist match {
      case a: JSONArray => for (node: JSON <- a) println("Nodes" + node(Left("id")).getOrElse(JSONNull))
        println("case JSONArray")
        JSONConversions.fromList(for (node: JSON <- a) yield node(Left("id")).getOrElse(JSONNull))
      case _ => JSONArray()
    }
    /* val includes: List[(String,String)] = edgelist match {
      case a: JSONArray => for (edge: JSON <- a if edge(Left("style")).getOrElse(return (List(),List())) == JSONString("include")) yield (edge(Left("from")).toString, edge(Left("to")).toString)
      case _ => List()
    } */

    val includes: JSON = edgelist match {
      case a: JSONArray => JSONConversions.fromList(for (edge: JSON <- a if edge(Left("style")).getOrElse(return JSONNull) == JSONString("include")) yield JSONArray(edge(Left("from")).getOrElse(JSONNull), edge(Left("to")).getOrElse(JSONNull)))
      case _ => JSONArray()
    }
    println("theories " + theories)
    println("includes " + includes)
    JSONArray(theories, includes)
  }

  def CallComputer (tgf: JSON, semantic: String, computer: String) : JSONArray = {

    if (computer == "default solver") {
      var args:java.util.Vector[String]= new java.util.Vector[String]()
      for (node <- tgf(Right(0))) args.add(node.toString)
      println("Args " + args)
      var atts:java.util.Vector[net.sf.jargsemsat.jargsemsat.datastructures.Pair[String,String]]= new java.util.Vector[net.sf.jargsemsat.jargsemsat.datastructures.Pair[String,String]]()
      for (edge <- tgf(Right(1))) atts.add(new net.sf.jargsemsat.jargsemsat.datastructures.Pair[String,String](edge(Right(0)).toString, edge(Right(1)).toString))
      println("Atts " + atts)
      if (semantic == "grounded") {
        val solver = new DungAF(args, atts)
        println("solver output" + solver.getGroundedExt.getClass)
        for (item <- solver.getGroundedExt.toArray) println("item " + item)
        val output = solver.getGroundedExt.toArray
        val retstring : String = output(0).toString
        println("retstring " + retstring.getClass)
        println ("retstringarray " + retstring.split(","))
        for (string <- retstring.split(",")) print(string)
        //val retlist = (for (string <- retstring.trim.split(",")) print(JSONString(string)))
        //println("retlist" + retlist)
        val ret = (for (string:String <- retstring.stripPrefix("[").stripSuffix("]").split(",")) yield(string)).toList
        //val ret = JSONConversions.fromList((for (string <- retstring.trim.split(",")) yield JSONString(string)).toList)
        //val ret :JSONArray =  JSONConversions.fromList((for (item <- solver.getGroundedExt.toArray) JSONString(item.toString.trim.split(","))))
        println("type of solverreturn " + ret.getClass())
        println("should be tgf" + ret)
        for (item <- ret) println(item)
        println("Returnstuff" + JSONConversions.fromList(for (item <- ret) yield JSONString(item)))
        return JSONConversions.fromList(for (item <- ret) yield JSONString(item))

        //import net.sf.jargsemsat.jargsemsat.datastructures.DungAF
        /*val args = new Nothing
        args.add("a")
        args.add("b")
        val atts = new Nothing
        atts.add(Array[String]("a", "b"))
        new DungAF(args, atts).getStableExts*/
      }
      else if (semantic == "stable"){
        val solver = new DungAF(args, atts)
        val ret = solver.getStableExts
        println("type of solverreturn" + ret.getClass())
        println(ret)
        ret
        JSONArray()
      }
      else {println("Error: not a supported semantic")}
      JSONArray()
    }

    else {
      println("Docker implementation broken")
      JSONArray()
          /* println("Calling computer")
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
          val config = ContainerConfig.builder().image(computer).cmd("docker run -v //c\\mmt2\\MMT\\src\\mmt-argsemcomp\\src\\info.kwarc.mmt.argsemcomp\\input.tgf:inputfile.tgf " + computer + "	600	-f inputfile.tgf	-fo	tgf	-p " + semantic).build()
          val container = docker.createContainer(config)
          val id: String = container.id()
          print(id)
          docker.startContainer(id)
          println("Docker container started and input file deleted")
          var logs: String = null
          try {
            val stream = docker.logs(id, LogsParam.stdout, LogsParam.stderr)
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
          accepted */
      }
  }

  def TgfToJson (f: JSON, tgf: JSONArray) : JSON = {
    print("tgf " + tgf(0))
    val nodelist: JSON = f(Left("nodes")).getOrElse(return JSONNull)
    println("got here")
    println("nodelistitem" + nodelist(Right(1)))
    val nodes : List[JSON] = nodelist match {
      case a: JSONArray => for (item: JSON <- a) yield (if (tgf.exists(sth => sth == item(Left("id")).getOrElse(return JSONNull)))
      JSONObject(
          ("id", item(Left("id")).getOrElse(return JSONNull)),
          ("style", JSONString("sceptically_accepted")),
          ("label", item(Left("label")).getOrElse(return JSONNull)),
          ("uri", item(Left("uri")).getOrElse(return JSONNull)),
          ("mathml", item(Left("mathml")).getOrElse(return JSONNull))
          )
        else item)
      case _ => List()
      }
    println("got here too")
    val outputjson : JSONObject = JSONObject(
      ("nodes", JSONConversions.fromList(nodes)),
      ("edges", f(Left("edges")).getOrElse(return JSONNull))
    )
    println("outputjson" + outputjson)
    outputjson
  }
}

