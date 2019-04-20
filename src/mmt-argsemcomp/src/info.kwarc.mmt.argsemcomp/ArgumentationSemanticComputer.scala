package info.kwarc.mmt.argsemcomp
import info.kwarc.mmt.api.utils._
import com.spotify.docker.client.DefaultDockerClient
import com.spotify.docker.client.messages.ContainerConfig
import com.spotify.docker.client.DockerClient.LogsParam
import info.kwarc.mmt.api.web.GraphSolverExtension
import net.sf.jargsemsat.jargsemsat.alg.GroundedSemantics
import net.sf.jargsemsat.jargsemsat.datastructures.DungAF
import scala.collection.JavaConverters._

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
      case a: JSONArray => JSONConversions.fromList(for (edge: JSON <- a if edge(Left("style")).getOrElse(return JSONNull) == JSONString("view")) yield JSONArray(edge(Left("from")).getOrElse(JSONNull), edge(Left("to")).getOrElse(JSONNull)))
      case _ => JSONArray()
    }
    println("theories " + theories)
    println("includes " + includes)
    JSONArray(theories, includes)
  }

  def CallComputer (tgf: JSON, semantic: String, computer: String) : Set[Set[JSONString]] = {

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
        //val output = solver.getGroundedExt.toArray
        //val ret = JSONConversions.fromList((for (string <- output) yield JSONString(string.toString.stripPrefix("\"").stripSuffix("\""))).toList)
        val ret = Set((for (item <- solver.getGroundedExt.asScala) yield JSONString(item.toString.stripPrefix("\"").stripSuffix("\""))).toSet)
        return ret
      }
      else if (semantic == "stable"){
        val output = solver.getStableExts.asScala
        val output2 = output.map(x => x.asScala.map(y => JSONConversions.fromString(y.toString.stripPrefix("\"").stripSuffix("\""))))
        val output3 = output2.map(x => x.toSet)
        val ret = output3.toSet
        //val ret = for (item:java.util.HashSet[java.lang.String] <- solver.getStableExts.asScala.toSet) yield item.asScala.toSet.map(x => JSONConversions.fromString(x.toString.stripPrefix("\"").stripSuffix("\"")))
        println("type of modified return" + ret.getClass)
        print("item classses ")
        //for (item <- ret) for (itemitem <- item) println(itemitem.getClass)
        println(ret)
        return ret
      }
      else {println("Error: not a supported semantic")}
      Set()
    }

    else {
      println("Docker implementation broken")
      Set()
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

  def TgfToJson (f: JSON, tgf: Set[Set[JSONString]]) : JSON = {
    println("InputJSON" + f)
    val nodelist: JSON = f(Left("nodes")).getOrElse(return JSONNull)
    println("tgf" + tgf)
    /*nodelist match {
      case a: JSONArray => for (item: JSON <- a) println (tgf.exists(x => x.contains(JSONString(item(Left("id")).getOrElse(JSONNull).toString.stripPrefix("\"").stripSuffix("\"")))))
      case _ => List()}

    nodelist match {
      case a: JSONArray => for (item: JSON <- a) println ("JSONStrings" + item(Left("id")).getOrElse(JSONNull).getClass)
      case _ => List()}*/

    val sceptically_accepted_nodes : List[JSON] = nodelist match {
      case a: JSONArray => for (item: JSON <- a if tgf.forall(x => x.contains(JSONString(item(Left("id")).getOrElse(JSONNull).toString.stripPrefix("\"").stripSuffix("\""))))) yield
        JSONObject(
          ("id", item(Left("id")).getOrElse(JSONNull)),
          ("style", JSONString("sceptically_accepted")),
          ("label", item(Left("label")).getOrElse(JSONNull)),
          ("uri", item(Left("uri")).getOrElse(JSONNull)),
          ("mathml", item(Left("mathml")).getOrElse(JSONNull))
        )
      case _ => List()
    }
    println(sceptically_accepted_nodes)

    val credulously_accepted_nodes : List[JSON] = nodelist match {
      case a: JSONArray => for (item: JSON <- a
                                if tgf.exists(x => x.contains(JSONString(item(Left("id")).getOrElse(JSONNull).toString.stripPrefix("\"").stripSuffix("\""))))
                                if !tgf.forall(x => x.contains(JSONString(item(Left("id")).getOrElse(JSONNull).toString.stripPrefix("\"").stripSuffix("\"")))))
        yield JSONObject(
          ("id", item(Left("id")).getOrElse(JSONNull)),
          ("style", JSONString("credulously_accepted")),
          ("label", item(Left("label")).getOrElse(JSONNull)),
          ("uri", item(Left("uri")).getOrElse(JSONNull)),
          ("mathml", item(Left("mathml")).getOrElse(JSONNull))
          )
      case _ => List()
    }
    println("CredNodes" + credulously_accepted_nodes)

    val rejected_nodes : List[JSON] = nodelist match {
      case a: JSONArray => for (item: JSON <- a if !tgf.exists(x => x.contains(JSONString(item(Left("id")).getOrElse(JSONNull).toString.stripPrefix("\"").stripSuffix("\""))))) yield
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
      ("nodes", JSONConversions.fromList(credulously_accepted_nodes++sceptically_accepted_nodes++rejected_nodes)),
      ("edges", f(Left("edges")).getOrElse(return JSONNull))
    )
    println("outputjson" + outputjson)
    outputjson
  }
}

