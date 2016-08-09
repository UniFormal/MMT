package info.kwarc.mmt.odk.LMFDB

import info.kwarc.mmt.api._
import backend._
import frontend._
import info.kwarc.mmt.api.metadata.{Linker, MetaData}
import info.kwarc.mmt.lf.records.Intro
import info.kwarc.mmt.odk.codecs.{LMFDBCoder, TMString, TMInt, TMList}
import modules._
import objects._
import symbols._
import utils._
import valuebases._

import info.kwarc.mmt.odk._

import scala.util.Try

class Plugin extends frontend.Plugin {
  val theory = Math.path
  val dependencies = List("info.kwarc.mmt.lf.Plugin")
  override def start(args: List[String]) {
    controller.backend.addStore(LMFDBStore)
    controller.extman.addExtension(new ImplementsRuleGenerator)
  }
}

object LMFDB {
   val path = DPath(URI("http","www.lmfdb.org"))
   val schemaPath = path / "schema"
   val dbPath = path / "db"
}

object Metadata {
   val path = LMFDB.path ? "Metadata"
   val implements = path ? "implements"
   val key = path ? "key"
   val codec = path ? "codec"
}

case class DBField(jsonKey: String, codec: Codec[JSON])
case class DBSchema(fields: List[DBField])

case class DB(suffix: LocalName, mod: LocalName) {
   def schemaTheory: MPath = (LMFDB.schemaPath / suffix) ? mod
   def dbTheory: MPath = (LMFDB.dbPath / suffix) ? mod
   def dbpath: String = suffix.toString + "/" + mod.toString
}

object LMFDBStore extends Storage {

  def load(path: Path)(implicit controller: Controller) {
    val (ndocO,modO,_) = path.toTriple
    val ndoc = ndocO.getOrElse {
       throw NotApplicable("")
    }
    val mod = modO.getOrElse {
       throw NotApplicable("")
    }
    val dbsuffix = ndoc.dropPrefix(LMFDB.dbPath).getOrElse {
       throw NotApplicable("")
    }

    val db = DB(dbsuffix, mod)
    
    val schema = controller.globalLookup.getAs(classOf[DeclaredTheory], db.schemaTheory)

    val th = controller.localLookup.getO(db.dbTheory) match {
       case Some(thy: DeclaredTheory) => thy 
       case Some(_) => throw BackendError("unexpected type", db.dbTheory)
       case None =>
          val t = new DeclaredTheory(db.dbTheory.parent, db.dbTheory.name, schema.meta)
          controller add t
          t
    }
    path match {
       case p: GlobalName => getElement(p, schema, db)
       case _ =>
    }
  }
  
  override def loadFragment(needed: Path, known: Path)(implicit controller: Controller) {
    load(needed)
  }

  private def getElement(path: GlobalName, schema: DeclaredTheory, db: DB)(implicit controller: Controller) {
    def error(msg: String) = throw BackendError(msg, path)
    val tp = schema.metadata.getValues(Metadata.implements).headOption.getOrElse {
       error("metadata key 'implements' not found in schema: " + schema.path)
    } match {
       case t: Term => t
       case _ => error("metadata key 'key' not found in schema: " + schema.path)
    }
    val key = schema.metadata.getValues(Metadata.key).headOption.getOrElse {
       error("metadata key 'key' not found in schema: " + schema.path)
    } match {
       case StringLiterals(k) => k
       case _ => error("metadata key 'key' is not a string in schema: " + schema.path)
    }
    
    val fields = schema.getConstants.flatMap {c =>
       c.metadata.getValues(Metadata.codec).headOption.toList.collect {
         case codecExp: Term =>
            DBField(c.name.toString, LMFDBCoder.buildCodec(codecExp))
       }
    }
    
     val query = "&" + key + "=" + path.name.toString
     val json = lmfdbquery(db.dbpath, query) match {
         case j: JSONObject => j.find(p => p._1 == JSONString("data")).map(_._2) match {
            case Some(j2: JSONArray) =>
               val flat = j2.values.toList.flatMap { // TODO this flattening looks wrong
                  case a: JSONObject => a.map
                  case j => Nil
               }
               JSONObject(flat)
            case _ =>
               error("Error: ill-formed JSON returned dfrom LMFDB")
       }
       case _ => error("Error querying LMFDB: not a JSON Object")
     }
     val omls = toOML(json, db, fields)
     val df = Intro(schema.toTerm,omls)
     val c = Constant(OMMOD(db.dbTheory), path.name, Nil, Some(tp), Some(df), None)
     controller.add(c)
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
