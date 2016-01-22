package info.kwarc.mmt.odk.LMFDB

import info.kwarc.mmt.api._
import backend._
import frontend._
import modules._
import objects._
import symbols._
import utils._
import valuebases._

import info.kwarc.mmt.odk._

import scala.util.Try

class Plugin extends frontend.Plugin {
  val theory = Typesystem.path
  val dependencies = List("info.kwarc.mmt.lf.Plugin")
  override def start(args: List[String]) {
    controller.backend.addStore(LMFDBStore)
  }
}

object LMFDB {
   val path = DPath(URI("http","www.lmfdb.org/"))
}

case class DBField(jsonKey: String, mmtName: String, codec: Codec[JSON])
case class DBSchema(fields: List[DBField])
case class DB(meta: MPath, tp: GlobalName, schemaTheory: MPath, schema: DBSchema, dbpath: String)

object Elliptic_curves {
   val path   =  LMFDB.path ? "elliptic_curves"
   val schemaThy = LMFDB.path ? "elliptic_curves_schema"
   val meta   = LMFDB.path ? "elliptic_curves_meta"
   
   val schema = DBSchema(List(
      DBField("2adic_gens",      "2adic_generators", TMList(TMInt)),
      DBField("2adic_index",     "2adic_index", TMInt),
      DBField("2adic_label",     "2adic_label", TMString),
      DBField("2adic_log_level", "2adic_log_level", TMInt),
      DBField("ainvs",           "a_invariants", TMList(TMInt)),
      DBField("cm",              "has_complex_multiplication", TMInt),
      DBField("conductor",       "conductor", TMInt),
      DBField("degree",          "modular_degree", TMInt)
   ))
   
   val db = DB(meta, meta ? "ec", schemaThy, schema, "elliptic_curves/curves")
}

object LMFDBStore extends Storage {

  def load(path: Path)(implicit controller: Controller) {
    val (ndoc,mod,dec) = path.toTriple
    if ((ndoc contains LMFDB.path) && (mod contains Elliptic_curves.path.name)) {
       val th = controller.localLookup.getO(Elliptic_curves.path) match {
          case Some(thy: DeclaredTheory) => thy 
          case Some(_) => throw BackendError("unexpected type", Elliptic_curves.path)
          case None => 
             val t = new DeclaredTheory(LMFDB.path, Elliptic_curves.path.name, Some(Elliptic_curves.meta))
             controller add t
             t
       }
       if (dec.isDefined) getElement(dec.get, th, Elliptic_curves.db)
    }
    else throw NotApplicable("")
  }

  private def getElement(name: LocalName, th: DeclaredTheory, db: DB)(implicit controller: Controller) {
    val regex = """(\d+)(\w)(\d+)""".r
    val meta = controller.globalLookup.getAs(classOf[DeclaredTheory], db.meta)// TODO: change accordingly
    val tp = controller.globalLookup.getAs(classOf[Constant], db.tp) // TODO: change accordingly
    val schema = controller.globalLookup.getAs(classOf[DeclaredTheory], db.schemaTheory)
    
    name.toString match {
      case regex(n1,a,n2) =>
        val query = "&label=" + n1 + a + n2
        val json = lmfdbquery(db.dbpath, query) match {
            case j: JSONObject => j.find(p => p._1 == JSONString("data")).map(_._2) match {
               case Some(j2: JSONArray) =>
                  val flat = j2.values.toList.flatMap { // TODO this flattening looks wrong
                     case a: JSONObject => a.map.toList
                     case j => Nil
                  }
                  JSONObject(flat)
               case _ =>
                  throw BackendError("Error: ill-formed JSON returned dfrom LMFDB", th.path ? name)
          }
          case _ => throw BackendError("Error querying LMFDB: not a JSON Object", th.path ? name)
        }
        val omls = toOML(json, db)
        val df = OMA(schema.toTerm, omls) // TODO
        val c = Constant(th.toTerm, name, None, Some(tp.toTerm), Some(df), None)
        controller.add(c)
      case _ => throw NotApplicable("")
    }
  }

  private def toOML(json: JSONObject, db: DB)(implicit controller: Controller): List[OML] = {
    db.schema.fields map {case DBField(key, name, codec) =>
      val dfJ = json(key).getOrElse {
         throw BackendError("could not find JSON value " + key + " in \n" + json, db.schemaTheory) //TODO use correct path
      }
      val df = codec.decode(dfJ)
      OML(VarDecl(LocalName(name), None, Some(df), None))
    }
  }

  private def lmfdbquery(db:String, query:String) : JSON = {
    val url = "http://www.lmfdb.org/api/" + db + "?_format=json" + query
    val url2 = "http://beta.lmfdb.org/api/" + db + "?_format=json" + query
    var attempt = Try(io.Source.fromURL(url))
    if (attempt.isFailure) attempt = Try(io.Source.fromURL(url2))
    val s = attempt.getOrElse(
        throw new GeneralError("Error trying to query lmfdb! Query: " + url + "\nError message:" + attempt.failed.get.getMessage)
    ).mkString
    JSON.parse(s)
  }
}
