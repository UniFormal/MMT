package info.kwarc.mmt.odk.LMFDB

import info.kwarc.mmt.api._
import backend._
import frontend._
import info.kwarc.mmt.api.metadata.{Linker, MetaData}
import info.kwarc.mmt.LFX.Records.Recexp
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
   val constructor = path ? "constructor"
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

  // A list of cached curves for offline curves
  // HACK HACK HACK
  // Remove this at some point or out-source it in a iseful way.
  private def offlineCurves = Map[String, String](
    ("http://www.lmfdb.org/api/elliptic_curves/curves?_format=json&label=11a1", "{\"collection\": \"curves\", \"data\": [{\"2adic_gens\": [], \"2adic_index\": 1, \"2adic_label\": \"X1\", \"2adic_log_level\": 0, \"_id\": \"ObjectId('4f71d4304d47869291435e6e')\", \"ainvs\": [\"0\", \"-1\", \"1\", \"-10\", \"-20\"], \"cm\": 0, \"conductor\": 11, \"degree\": 1, \"galois_images\": [\"5Cs.1.1\"], \"gens\": [], \"iso\": \"11a\", \"iso_nlabel\": 0, \"isogeny_matrix\": [[1, 5, 25], [5, 1, 5], [25, 5, 1]], \"jinv\": \"-122023936/161051\", \"label\": \"11a1\", \"lmfdb_iso\": \"11.a\", \"lmfdb_label\": \"11.a2\", \"lmfdb_number\": 2, \"non-surjective_primes\": [5], \"number\": 1, \"rank\": 0, \"real_period\": 1.26920930427955, \"regulator\": 1.0, \"sha\": 1, \"sha_an\": 1.0, \"sha_primes\": [], \"special_value\": 0.253841860855911, \"tamagawa_product\": 5, \"torsion\": 5, \"torsion_generators\": [\"(5, 5)\"], \"torsion_primes\": [5], \"torsion_structure\": [\"5\"], \"x-coordinates_of_integral_points\": \"[5,16]\"}], \"database\": \"elliptic_curves\", \"next\": \"/api/elliptic_curves/curves?_format=json&label=11a1&_offset=1\", \"offset\": 1, \"query\": \"/api/elliptic_curves/curves?_format=json&label=11a1&_offset=0\", \"start\": 0, \"timestamp\": \"2016-01-25T00:58:14.913417\"}"),
    ("http://www.lmfdb.org/api/elliptic_curves/curves?_format=json&label=35a2", "{\"start\": 0, \"query\": \"/api/elliptic_curves/curves/?_format=json&label=35a2&_offset=0\", \"database\": \"elliptic_curves\", \"timestamp\": \"2016-08-23T10:43:54.847869\", \"offset\": 1, \"data\": [{\"torsion_structure\": [], \"ainvs\": [\"0\", \"1\", \"1\", \"-131\", \"-650\"], \"x-coordinates_of_integral_points\": \"[]\", \"cm\": 0, \"number\": 2, \"rank\": 0, \"sha_primes\": [], \"galois_images\": [\"3B.1.2\"], \"heights\": [], \"torsion\": 1, \"iso_nlabel\": 0, \"min_quad_twist\": {\"disc\": 1, \"label\": \"35a2\"}, \"sha_an\": 1.0, \"local_data\": [{\"ord_disc\": 9, \"ord_cond\": 1, \"kod\": \"\\\\( I_{9} \\\\)\", \"ord_den_j\": 9, \"p\": 5, \"cp\": 1, \"rootno\": 1, \"red\": -1}, {\"ord_disc\": 1, \"ord_cond\": 1, \"kod\": \"\\\\( I_{1} \\\\)\", \"ord_den_j\": 1, \"p\": 7, \"cp\": 1, \"rootno\": -1, \"red\": 1}], \"conductor\": 35, \"lmfdb_iso\": \"35.a\", \"2adic_label\": \"X1\", \"xainvs\": \"[0,1,1,-131,-650]\", \"jinv\": \"-250523582464/13671875\", \"label\": \"35a2\", \"2adic_log_level\": 0, \"tamagawa_product\": 1, \"lmfdb_number\": 1, \"torsion_generators\": [], \"degree\": 6, \"2adic_gens\": [], \"torsion_primes\": [], \"signD\": -1, \"real_period\": 0.702911239134905, \"isogeny_matrix\": [[1, 9, 3], [9, 1, 3], [3, 3, 1]], \"special_value\": 0.702911239134905, \"non-surjective_primes\": [3], \"lmfdb_label\": \"35.a1\", \"2adic_index\": 1, \"equation\": \"\\\\( y^2 + y = x^{3} + x^{2} - 131 x - 650  \\\\)\", \"gens\": [], \"regulator\": 1.0, \"sha\": 1, \"iso\": \"35a\", \"_id\": \"ObjectId('4f71d4314d47869291435eb4')\"}], \"collection\": \"curves\", \"next\": \"/api/elliptic_curves/curves/?_format=json&label=35a2&_offset=1\"}")
  )

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

    // find the type we are implementing
    val tp = schema.metadata.getValues(Metadata.implements).headOption.getOrElse {
       error("metadata key 'implements' not found in schema: " + schema.path)
    } match {
       case t: Term => t
       case _ => error("metadata key 'implements'  found in schema: " + schema.path)
    }

    // getting from records --> instance
    val constructor = schema.metadata.getValues(Metadata.constructor).headOption.getOrElse {
      error("metadata key 'constructor' not found in schema: " + schema.path)
    } match {
      case t: Term => t
      case _ => error("metadata key 'constructor'  found in schema: " + schema.path)
    }

    // HACK HACK HACK; parse the ODK String
    val spath = OMS(Path.parseS("http://www.opendreamkit.org/?Strings?string", NamespaceMap.empty))

    val key = schema.metadata.getValues(Metadata.key).headOption.getOrElse {
      error("metadata key 'key' not found in schema: " + schema.path)
    } match {
       case StringLiterals(k) => k // This no longer works; we are using the HACK below
       case UnknownOMLIT(a, `spath`) => a.toString
       case s => println(s.getClass); error("metadata key 'key' is not a string in schema: " + schema.path)
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
               error("Error: ill-formed JSON returned from LMFDB")
       }
       case _ => error("Error querying LMFDB: not a JSON Object")
     }
     val omls = toOML(json, db, fields)

     val df = Recexp(omls : _*)

     val c = Constant(OMMOD(db.dbTheory), path.name, Nil, Some(tp), Some(constructor(df)), None)
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

    // HACKED IN offline curves, remove this later please
    val s = attempt.getOrElse(
      offlineCurves.getOrElse(url, offlineCurves.getOrElse(url2, {
        throw new GeneralError("Error trying to query lmfdb! Query: " + url + "\nError message:" + attempt.failed.get.getMessage)
      })).toBuffer
    ).mkString
    JSON.parse(s)
  }
}