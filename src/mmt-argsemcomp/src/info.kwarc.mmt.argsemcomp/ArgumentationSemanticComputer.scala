package info.kwarc.mmt.argsemcomp
import info.kwarc.mmt.api.utils._
import com.spotify.docker.client.DefaultDockerClient
import com.spotify.docker.client.messages.ContainerConfig
import com.spotify.docker.client.DockerClient.LogsParam
import info.kwarc.mmt.api.web.GraphSolverExtension
import net.sf.jargsemsat.jargsemsat.alg.GroundedSemantics
import net.sf.jargsemsat.jargsemsat.datastructures.DungAF

/** TO DO: Implement proper error catching. Make loggable. Make switch to docker. Take care of border nodes.
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
    val theories: JSONArray = nodelist match {
      case a: JSONArray => for (node: JSON <- a) println("Nodes" + node(Left("id")).getOrElse(JSONNull))
        println("case JSONArray")
        JSONConversions.fromList(for (node: JSON <- a) yield node(Left("id")).getOrElse(JSONNull))
      case _ => JSONArray()
    }
    /* val includes: List[(String,String)] = edgelist match {
      case a: JSONArray => for (edge: JSON <- a if edge(Left("style")).getOrElse(return (List(),List())) == JSONString("include")) yield (edge(Left("from")).toString, edge(Left("to")).toString)
      case _ => List()
    } */

    val includes: JSONArray = edgelist match {
      case a: JSONArray => JSONConversions.fromList(for (edge: JSON <- a if edge(Left("style")).getOrElse(return JSONNull) == JSONString("include")) yield JSONArray(edge(Left("from")).getOrElse(JSONNull), edge(Left("to")).getOrElse(JSONNull)))
      case _ => JSONArray()
    }
    println("theories " + theories)
    println("includes " + includes)
    JSONArray(theories, includes)
  }

  def CallComputer (tgf: JSON, semantic: String, computer: String) : JSONArray = {

    if (computer == "default solver") {
      println("Input Json" + tgf)
      println("tgf items class " + tgf(Right(0)).getClass)
      var args:java.util.Vector[String]= new java.util.Vector[String]()
      println("InitArgs " + args)
      //args.add("test")
      println("TestArgs " + args)

      tgf(Right(0)).getOrElse(JSONArray()) match
      {case a:JSONArray => for (edge <-a) args.add(edge.toString)
        case _ => println("Not a JSONArray")
      }
      //print("Args"+ args)
      // for (node <- arguments) args.add(node.toString)

      var atts:java.util.Vector[net.sf.jargsemsat.jargsemsat.datastructures.Pair[String,String]]= new java.util.Vector[net.sf.jargsemsat.jargsemsat.datastructures.Pair[String,String]]()
      tgf(Right(1)).getOrElse(JSONArray()) match
      {case a:JSONArray => for (edge <-a) atts.add(new net.sf.jargsemsat.jargsemsat.datastructures.Pair[String,String](edge(Right(0)).getOrElse(JSONNull).toString, edge(Right(1)).getOrElse(JSONNull).toString))
        case _ => println("Not a JSONArray")
      }

      println("Args " + args)
      println("Atts " + atts)
      val solver = new DungAF(args, atts)

      if (semantic == "grounded") {
        val output = solver.getGroundedExt.toArray
        val ret = JSONConversions.fromList((for (string <- output) yield JSONString(string.toString.stripPrefix("\"").stripSuffix("\""))).toList)
        return ret
      }
      else if (semantic == "stable"){
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
    println("InputJSON" + f)
    val nodelist: JSON = f(Left("nodes")).getOrElse(return JSONNull)
    val idlist:List[JSON] = nodelist match {
      case a: JSONArray => for (item: JSON <- a) yield item(Left("id")).getOrElse(return JSONNull)
      case _  => List()}
    println("tgf " + tgf)
    println ("idlist" + idlist)
    println("Exists?" + tgf.exists(sth => idlist.contains(sth)))
    println("Exists not accepted?" + idlist.exists(sth => !tgf.contains(sth)))
    println(tgf.getClass())
    println("equal?" + tgf.toList==idlist)
    println("got here")
    println("nodelist" + nodelist)
    val accepted_nodes : List[JSON] = nodelist match {
      case a: JSONArray => for (item: JSON <- a if tgf.contains(item(Left("id")).getOrElse(JSONNull))) yield
      JSONObject(
          ("id", item(Left("id")).getOrElse(JSONNull)),
          ("style", JSONString("sceptically_accepted")),
          ("label", item(Left("label")).getOrElse(JSONNull)),
          ("uri", item(Left("uri")).getOrElse(JSONNull)),
          ("mathml", item(Left("mathml")).getOrElse(JSONNull))
          )
      case _ => List()
    }

    val rejected_nodes : List[JSON] = nodelist match {
      case a: JSONArray => for (item: JSON <- a if !tgf.contains(item(Left("id")).getOrElse(JSONNull))) yield
        JSONObject (
      ("id", item (Left ("id") ).getOrElse (JSONNull) ),
      ("style", JSONString ("rejected") ),
      ("label", item (Left ("label") ).getOrElse (JSONNull) ),
      ("uri", item (Left ("uri") ).getOrElse (JSONNull) ),
      ("mathml", item (Left ("mathml") ).getOrElse (JSONNull) )
      )
      case _ => List()
    }
    println("got here too" + rejected_nodes)
    val outputjson : JSONObject = JSONObject(
      ("nodes", JSONConversions.fromList(accepted_nodes++rejected_nodes)),
      ("edges", f(Left("edges")).getOrElse(return JSONNull))
    )
    println("outputjson" + outputjson)
    outputjson
  }
}

