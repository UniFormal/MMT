package info.kwarc.mmt.odk.LMFDB

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.backend.{NotApplicable, Storage}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.{FinalConstant, Constant}
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.odk._

import scala.util.Try

class Plugin extends frontend.Plugin {
  val theory = Path.parseM("http://www.opendreamkit.org/?TypeSystem",NamespaceMap.empty)
  val dependencies = List("info.kwarc.mmt.lf.Plugin")
  override def start(args: List[String]) {
    // content enhancers
    controller.backend.addStore(LmfdbStore)
  }
}

object LmfdbStore extends Storage {
  object DBs {
    val doc = DPath(URI("http://www.lmfdb.org/"))

    object Elliptic_curves {
      val path = MPath(doc,LocalName("elliptic_curves"))
      val schema = MPath(doc,LocalName("elliptic_curves_schema"))
      val meta = MPath(doc,LocalName("elliptic_curves_meta"))
    }
  }

  def toOML(json:JSONObject,schema:DeclaredTheory,pars:List[(String,String,Codec)])
           (implicit controller: Controller): List[OML] = {
    val typesystem = Typesystem.theory
    pars.map(triple => {
      val name = LocalName(triple._2)
      val dfO = json.find(p => p._1 == JSONString(triple._1)).map(_._2)
      val df = if (dfO.isDefined) triple._3.fromJSON(dfO.get)
        else throw new Exception("Error: couldn't find json value " + triple._1 + " in \n"+json)
      OML(VarDecl(name, None, Some(df), None))
    })
  }

  def load(path: Path)(implicit controller: Controller) {
    val (ndoc,mod,dec) = path.toTriple
    if (ndoc.isDefined && ndoc.get==DBs.doc) {
      if (mod.contains(DBs.Elliptic_curves.path.name)) {
        val th = controller.localLookup.getO(DBs.Elliptic_curves.path).map(_.asInstanceOf[DeclaredTheory]).getOrElse({
          val t = new DeclaredTheory(DBs.doc, DBs.Elliptic_curves.path.name, Some(DBs.Elliptic_curves.meta))
          controller add t
          t
        })
        if (dec.isDefined) getelliptic(dec.get,th)
      }
      else throw NotApplicable("")
    }
    else throw NotApplicable("")
  }

  def getelliptic(curve:LocalName,th:DeclaredTheory)(implicit controller: Controller) = {
    val regex = """(\d+)(\w)(\d+)""".r
    val meta = controller.get(DBs.Elliptic_curves.meta).asInstanceOf[DeclaredTheory]// TODO: change accordingly
    val ectp = meta.get("ec").asInstanceOf[FinalConstant] // TODO: change accordingly
    val dbpath = "elliptic_curves/curves"
    val schema = controller.get(DBs.Elliptic_curves.schema).asInstanceOf[DeclaredTheory]

    curve.toString match {
      case regex(n1,a,n2) =>
        val query = "&label=" + n1 + a + n2
        val json = lmfdbquery(dbpath,query) match {
          case j:JSONObject => j.find(p => p._1==JSONString("data")).map(_._2) match {
            case Some(j2:JSONArray) => j2.values match {
              case j3:Seq[JSONObject] => JSONObject(j3.toList.flatMap(obj => obj.map))
            }
            case _ => throw new Exception("Error: ill formed json return from lmfdb")
          }
          case _ => throw new Exception("Error querying lmfd: Not a JSON Object!")
        }
        val df = OMA(schema.toTerm,toOML(json,schema,List(
          ("2adic_gens","2adic_generators",TMList(TMInt)),
          ("2adic_index","2adic_index",TMInt),
          ("2adic_label","2adic_label",TMString),
          ("2adic_log_level","2adic_log_level",TMInt),
          ("ainvs","a_invariants",TMList(TMInt)),
          ("cm","has_complex_multiplication",TMInt),
          ("conductor","conductor",TMInt),
          ("degree","modular_degree",TMInt)
        ))) // TODO
        val c = Constant(th.toTerm,curve,None,Some(ectp.toTerm),Some(df),None)
        th add c
        c
      case _ => throw NotApplicable("")
    }

  }

  def lmfdbquery(db:String, query:String) : JSON = {
    val url = "http://www.lmfdb.org/api/" + db + "?_format=json" + query
    val url2 = "http://beta.lmfdb.org/api/" + db + "?_format=json" + query
    var attempt = Try(io.Source.fromURL(url))
    if (attempt.isFailure) attempt = Try(io.Source.fromURL(url2))
    JSON.parse(attempt.getOrElse(
      throw new Exception("Error trying to query lmfdb! Query: " + url + "\nError message:" + attempt.failed.get.getMessage)
    ).mkString)
  }
}
