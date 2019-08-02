package info.kwarc.mmt.argsemcomp
import info.kwarc.mmt.api.utils._
//import com.spotify.docker.client.DefaultDockerClient
//import com.spotify.docker.client.messages.ContainerConfig
//import com.spotify.docker.client.DockerClient.LogsParam
//import info.kwarc.mmt.api.refactoring.AcrossLibraryTranslator
import info.kwarc.mmt.api.web.GraphSolverExtension
//import net.sf.jargsemsat.jargsemsat.alg.GroundedSemantics
import net.sf.jargsemsat.jargsemsat.datastructures.DungAF

import scala.collection.JavaConverters._

// TODO: Make switch to docker. Take care of border nodes. Switch to attacks instead of includes


class ArgumentationSemanticComputer extends GraphSolverExtension {
  val key = "default"
  override val logPrefix = "argcomp"
  private case class CatchError(s : String) extends Throwable

  def apply (f: JSON, sem: String, comp: String = "default") :JSON = {
    log("Starting Argumentation Semantic Computer")
    val ret = TgfToJson(f, CallComputer(JsonToTgf(f), sem, comp))
    ret}

  def JsonToTgf (f: JSON) : JSON = {
    /** Gets the node and edge uris from the input JSON and arranges them in a JSONArray in the tgf format https://en.wikipedia.org/wiki/Trivial_Graph_Format
      * f: A Theory Graph JSON
      * returns: A tgf-formatted JSONArray
    */
    log("Stripping node and edge uris from input-JSON to make a tgf-JSON")
    val nodelist: JSON = f(Left("nodes")).getOrElse(return JSONNull)
    val edgelist: JSON = f(Left("edges")).getOrElse(return JSONNull)
    val theories: JSONArray = nodelist match {
      case a: JSONArray =>
        JSONConversions.fromList(for (node: JSON <- a) yield node(Left("id")).getOrElse(JSONNull))
      case _ => JSONArray()
    }

    val views: JSONArray = edgelist match {
      case a: JSONArray => JSONConversions.fromList(for (edge: JSON <- a if edge(Left("style")).getOrElse(return JSONNull) == JSONString("view")) yield JSONArray(edge(Left("from")).getOrElse(JSONNull), edge(Left("to")).getOrElse(JSONNull)))
      case _ => JSONArray()
    }
    //TODO: Change this to attacks once they are implemented.

    JSONArray(theories, views)
  }

  def CallComputer (tgf: JSON, semantic: String, computer: String) : Set[Set[JSONString]] = {
    /**
      * Takes a tgf-formated JSONArray and feeds it to an external solver (for now: jArgSemSAT https://github.com/federicocerutti/jArgSemSAT).
      * tgf: A tgf-formatted JSONArray
      * semantic: one of "grounded", or "stable" (for now). TODO: Implement the more semantics.
      * computer: the solver to be used. For now only jArgSemSAT ("default). TODO: implement more solvers.
      * Returns: a set of collections (accepted, rejected, undecided nodes) of uris in JSONString form.
      */
    if (computer == "default") {
      log("Computing acceptance status using jArgSemSat")
      var args:java.util.Vector[String]= new java.util.Vector[String]()

      tgf(Right(0)).getOrElse(JSONArray()) match
      {case a:JSONArray => for (edge <-a) args.add(edge.toString)
        case _ => CatchError("Not a JSONArray")
      }

      var atts:java.util.Vector[net.sf.jargsemsat.jargsemsat.datastructures.Pair[String,String]]= new java.util.Vector[net.sf.jargsemsat.jargsemsat.datastructures.Pair[String,String]]()
      tgf(Right(1)).getOrElse(JSONArray()) match
      {case a:JSONArray => for (edge <-a) atts.add(new net.sf.jargsemsat.jargsemsat.datastructures.Pair[String,String](edge(Right(0)).getOrElse(JSONNull).toString, edge(Right(1)).getOrElse(JSONNull).toString))
        case _ => CatchError("Not a JSONArray")
      }

      //println("Args " + args)
      //println("Atts " + atts)
      val solver = new DungAF(args, atts)

      if (semantic == "grounded") {
        val ret = Set((for (item <- solver.getGroundedExt.asScala) yield JSONString(item.toString.stripPrefix("\"").stripSuffix("\""))).toSet)
        return ret
      }
      else if (semantic == "stable"){
        val output = solver.getStableExts.asScala
        val output2 = output.map(x => x.asScala.map(y => JSONConversions.fromString(y.toString.stripPrefix("\"").stripSuffix("\""))))
        val output3 = output2.map(x => x.toSet)
        val ret = output3.toSet
        return ret
      }
      else {CatchError("Not a supported semantic")}
      Set()
    }

    else {
      CatchError("Docker implementation broken")
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
    /** Takes the theory graph JSON and the sets of accepted (rejected, undecided) nodes and merges them into a context graph JSON by replacing the styles of the accepted (rejected, undecided) nodes
      * with the corresponding style attributes (skeptically_accepted, rejected, credulously_accepted).
      * f: A theory graph JSON
      * tgf: A set of collections of JSONStrings (accepted, rejected, undecided).
      * Returns: A context graph JSON.
      */
    log("Creating Context Graph")
    val nodelist: JSON = f(Left("nodes")).getOrElse(return JSONNull)
    val sceptically_accepted_nodes : List[JSON] = nodelist match {
      case a: JSONArray => for (item: JSON <- a if tgf.forall(x => x.contains(JSONString(item(Left("id")).getOrElse(JSONNull).toString.stripPrefix("\"").stripSuffix("\""))))) yield
        JSONObject(
          ("id", item(Left("id")).getOrElse(JSONNull)),
          ("style", JSONString("sceptically_accepted")),
          ("label", item(Left("label")).getOrElse(JSONNull)),
          ("url", item(Left("url")).getOrElse(JSONNull)),
        )
      case _ => List()
    }
    //println(sceptically_accepted_nodes)

    val credulously_accepted_nodes : List[JSON] = nodelist match {
      case a: JSONArray => for (item: JSON <- a
                                if tgf.exists(x => x.contains(JSONString(item(Left("id")).getOrElse(JSONNull).toString.stripPrefix("\"").stripSuffix("\""))))
                                if !tgf.forall(x => x.contains(JSONString(item(Left("id")).getOrElse(JSONNull).toString.stripPrefix("\"").stripSuffix("\"")))))
        yield JSONObject(
          ("id", item(Left("id")).getOrElse(JSONNull)),
          ("style", JSONString("credulously_accepted")),
          ("label", item(Left("label")).getOrElse(JSONNull)),
          ("url", item(Left("url")).getOrElse(JSONNull)),
          )
      case _ => List()
    }
    //println("CredNodes" + credulously_accepted_nodes)

    val rejected_nodes : List[JSON] = nodelist match {
      case a: JSONArray => for (item: JSON <- a if !tgf.exists(x => x.contains(JSONString(item(Left("id")).getOrElse(JSONNull).toString.stripPrefix("\"").stripSuffix("\""))))) yield
        JSONObject (
      ("id", item (Left ("id") ).getOrElse (JSONNull) ),
      ("style", JSONString ("rejected") ),
      ("label", item (Left ("label") ).getOrElse (JSONNull) ),
      ("url", item (Left ("url") ).getOrElse (JSONNull) ),
      )
      case _ => List()
    }
    //println("got here too" + rejected_nodes)
    val outputjson : JSONObject = JSONObject(
      ("nodes", JSONConversions.fromList(credulously_accepted_nodes++sceptically_accepted_nodes++rejected_nodes)),
      ("edges", f(Left("edges")).getOrElse(return JSONNull))
    )
    outputjson
  }
}

