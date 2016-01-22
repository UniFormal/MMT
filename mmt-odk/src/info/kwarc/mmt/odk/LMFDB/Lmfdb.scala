package info.kwarc.mmt.odk.LMFDB

import info.kwarc.mmt.api._
import backend._
import frontend._
import info.kwarc.mmt.api.metadata.{Linker, MetaData}
import info.kwarc.mmt.odk.codecs.{LMFDBCoder, TMString, TMInt, TMList}
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
   val path = DPath(URI("http","www.lmfdb.org"))
   val implKey = LMFDB.path ? "meta" ? "implements"
   val codecKey = codecs.Codecs.path ? "codec"
}

case class DBField(jsonKey: String, codec: Codec[JSON])
case class DBSchema(fields: List[DBField])
case class DB(tp: GlobalName, schemaTheory: MPath, dbpath: String)

object Elliptic_curves {
   val path   =  LMFDB.path ? "elliptic_curves"
   val schemaThy = LMFDB.path ? "elliptic_curves_schema"
   val meta   = LMFDB.path ? "elliptic_curves_meta"
   
   val db = DB(meta ? "ec", schemaThy, "elliptic_curves/curves")
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
    val tp = controller.globalLookup.getAs(classOf[Constant], db.tp) // TODO: change accordingly
    val schema = controller.globalLookup.getAs(classOf[DeclaredTheory], db.schemaTheory)

    val fields = schema.getConstants.flatMap {c =>
       c.metadata.getValues(LMFDB.codecKey).headOption.toList.collect {
         case codecExp: Term =>
            DBField(c.name.toString, LMFDBCoder.buildCodec(codecExp))
       }
    }
    
    name.toString match {
      case regex(n1,a,n2) =>
        val query = "&label=" + n1 + a + n2
        val json = lmfdbquery(db.dbpath, query) match {
            case j: JSONObject => j.find(p => p._1 == JSONString("data")).map(_._2) match {
               case Some(j2: JSONArray) =>
                  val flat = j2.values.toList.flatMap { // TODO this flattening looks wrong
                     case a: JSONObject => a.map
                     case j => Nil
                  }
                  JSONObject(flat)
               case _ =>
                  throw BackendError("Error: ill-formed JSON returned dfrom LMFDB", th.path ? name)
          }
          case _ => throw BackendError("Error querying LMFDB: not a JSON Object", th.path ? name)
        }
        val omls = toOML(json, db, fields)
        val df = OMA(schema.toTerm, omls) // TODO
        val c = Constant(th.toTerm, name, None, Some(tp.toTerm), Some(df), None)
        controller.add(c)
      case _ => throw NotApplicable("")
    }
  }

  private def toOML(json: JSONObject, db: DB, fields: List[DBField])(implicit controller: Controller): List[OML] = {
    fields map {case DBField(key, codec) =>
      val dfJ = json(key).getOrElse {
         throw BackendError("could not find JSON value " + key + " in \n" + json, db.schemaTheory) //TODO use correct path
      }
      val df = codec.decode(dfJ)
      OML(VarDecl(LocalName(key), None, Some(df), None))
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
