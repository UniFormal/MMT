package info.kwarc.mmt.odk.LMFDB

import info.kwarc.mmt.api._
import backend._
import frontend._
import ontology._
import info.kwarc.mmt.api.metadata.{Linker, MetaData}
import info.kwarc.mmt.LFX.Records.Recexp
import info.kwarc.mmt.api.ontology.{BaseType, Query, QueryEvaluator, QueryExtension}
import info.kwarc.mmt.odk.codecs.{LMFDBCoder, TMInt, TMList, TMString}
import modules.{DeclaredTheory, _}
import objects._
import symbols._
import utils._
import valuebases._
import info.kwarc.mmt.odk._

import scala.collection.mutable
import scala.collection.mutable.HashSet
import scala.util.Try

class Plugin extends frontend.Plugin {
  val theory = Math.path
  val dependencies = List("info.kwarc.mmt.lf.Plugin")
  override def start(args: List[String]) {
    controller.backend.addStore(LMFDBStore)
    controller.extman.addExtension(new ImplementsRuleGenerator)
    controller.extman.addExtension(LMFDBEvaluator)
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

object DB {

  def schema2db(sPath : MPath) : MPath = fromPath(sPath).get.dbTheory
  def db2Schema(dbPath : MPath) : MPath = fromPath(dbPath).get.schemaTheory

  def fromPath(pth : Path, allowDBPath: Boolean = true, allowSchemaPath : Boolean = true) : Option[DB] = {
    // extract relevant components
    val (ndoc, mod, _) = pth.toTriple

    // if either of the components is empty, return
    if(ndoc.isEmpty || mod.isEmpty)
      return None

    // try to remove both of the prefixes
    val suffixS = if(allowSchemaPath) ndoc.get.dropPrefix(LMFDB.schemaPath) else None
    val suffixD = if(allowDBPath) ndoc.get.dropPrefix(LMFDB.dbPath) else None

    // if we have neither, we are not a valid theory
    if (suffixD.isEmpty && suffixD.isEmpty)
      return None

    // and get the db object
    Some(DB(suffixS.getOrElse({suffixD.get}), mod.get))
  }
}
/**
  * Shared code for LMFDB Queries between the Storage and the backend
  */
trait LMFDBBackend {

  protected def toOML(json: JSONObject, db: DB, fields: List[DBField])(implicit controller: Controller): List[OML] = {
    fields map {case DBField(key, codec) =>
      val dfJ = json(key).getOrElse {
        throw BackendError("could not find JSON value " + key + " in \n" + json, db.schemaTheory) //TODO use correct path
      }
      val df = codec.decode(dfJ)
      OML(LocalName(key), None, Some(df))
    }
  }

  //
  // GET OBJECTS FROM LMFDB
  //

  /**
    * Tries to retrieve json from a URL
    * @param url url to retrieve json from
    * @return
    */
  private def get_json(url: URI) : Option[JSON] = {
    // println("Trying: "+url) // TO SEE SOME PROGRESS
    val attempt = Try(io.Source.fromURL(url.toString))
    if (attempt.isFailure) None else Some(attempt.get.toBuffer.mkString).map(JSON.parse)
  }

  /**
    * Retrieves JSON from a URL and recurses into a key called next until retrieval fails
    * @param url
    * @return
    */
  private def get_json_withnext(url : URI, next_key : String = "next") : List[JSON] = {

    // get the current page
    val data = get_json(url) match {
      case None => return Nil
      case Some(x) => x
    }

    // and iterate if possible
    data match {
      case j:JSONObject => j(next_key) match {
        case Some(JSONString(s)) =>
          val nurl = url.resolve(s)
          if(nurl != url){
            data :: get_json_withnext(url.resolve(s))
          } else {
            List(data)
          }

        case _ => List(data)
      }
      case _ => List(data)
    }
  }

  protected def lmfdbquery(db:String, query:String) : List[JSON] = {
    // get the url
    val url = URI("http://www.lmfdb.org/api/" + db + "?_format=json" + query)

    // get all the data items
    get_json_withnext(url).flatMap(
      _.asInstanceOf[JSONObject]("data").get.asInstanceOf[JSONArray].values
    )
  }

  protected def getTP(schema : DeclaredTheory, err : String => Unit) : Term = schema.metadata.getValues(Metadata.implements).headOption.getOrElse {
    err("metadata key 'implements' not found in schema: " + schema.path)
  } match {
    case t: Term => t
    case _ => err("metadata key 'implements'  found in schema: " + schema.path); null
  }

  protected def getConstructor(schema : DeclaredTheory, err: String => Unit) : Term = schema.metadata.getValues(Metadata.constructor).headOption.getOrElse {
    err("metadata key 'constructor' not found in schema: " + schema.path)
  } match {
    case t: Term => t
    case _ => err("metadata key 'constructor'  found in schema: " + schema.path); null
  }

  protected def getKey(schema : DeclaredTheory, err: String => Unit) : String = {
    // HACK HACK HACK; parse the ODK String
    val spath = OMS(Path.parseS("http://www.opendreamkit.org/?Strings?string", NamespaceMap.empty))

    schema.metadata.getValues(Metadata.key).headOption.getOrElse {
      err("metadata key 'key' not found in schema: " + schema.path); null
    } match {
      // case StringLiterals(k) => k // This no longer works; we are using the HACK below
      case UnknownOMLIT(a, `spath`) => a.toString
      case s => println(s.getClass); err("metadata key 'key' is not a string in schema: " + schema.path); null
    }
  }
}



object LMFDBStore extends Storage with LMFDBBackend {

  def load(path: Path)(implicit controller: Controller) {

    val db = DB.fromPath(path, allowSchemaPath = false).getOrElse {
      throw NotApplicable("")
    }

    val schema = controller.globalLookup.getAs(classOf[DeclaredTheory], db.schemaTheory)

    val th = controller.localLookup.getO(db.dbTheory) match {
       case Some(thy: DeclaredTheory) => thy 
       case Some(_) => throw BackendError("unexpected type", db.dbTheory)
       case None =>
          val t = Theory.empty(db.dbTheory.parent, db.dbTheory.name, schema.meta)
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
    val tp = getTP(schema, error)

    // getting from records --> instance
    val constructor = getConstructor(schema, error)

    val key = getKey(schema, error)


    val fields = schema.getConstants.flatMap {c =>
       c.metadata.getValues(Metadata.codec).headOption.toList.collect {
         case codecExp: Term =>
            DBField(c.name.toString, LMFDBCoder.buildCodec(codecExp))
       }
    }
    
     val query = "&" + key + "=" + path.name.toString
     val json = JSONObject(lmfdbquery(db.dbpath, query).flatMap { // TODO this flattening looks wrong
        case a: JSONObject => a.map
        case j => Nil
      })
     val omls = toOML(json, db, fields)

     val df = Recexp(omls : _*)

     val c = Constant(OMMOD(db.dbTheory), path.name, Nil, Some(tp), Some(constructor(df)), None)
     controller.add(c)
  }
}

object LMFDBEvaluator extends QueryExtension("lmfdb") with LMFDBBackend {

  private def error(msg: String) = {
    throw ImplementationError("LMFDB QueryEvaluator() failed to evaluate query. " + msg)
  }

  private def getAll(db : DB) : List[GlobalName] = {
    // get the schema theory, this is a DeclaredTheory by precondition
    val schema = controller.get(db.schemaTheory) match {
      case dt:DeclaredTheory => dt
      case _ => error("Schema-Theory missing from controller")
    }

    // get the key needed
    val key = getKey(schema, error)

    // get a list of data items returned
    val datas : List[JSON] = lmfdbquery(db.dbpath, "&_fields=" + key)

    // and now get a list of names
    val labels = datas.map({
      case o : JSONObject => o(key).getOrElse {
        error("Ill-formed JSON returned from database")
      }
      case _ => error("Ill-formed JSON returned from database")
    })

    // and return a list of globalnames in the db theory
    labels.map {
      case s : JSONString => db.dbTheory ? s.value
      case _ => error("Ill-formed JSON returned from database")
    }

  }


  /**
    * Evaluates a Query using the lmfdb api
    * @param q           Query to evaluate
    * @param e           A QueryEvaluator to use for recursive queries
    * @param substiution Substiution (Context) to apply QueryEvaluation in
    * @return
    */
  def evaluate(q: Query, e: QueryEvaluator)(implicit substiution: QueryEvaluator.QuerySubstitution): HashSet[List[BaseType]] = q match {
    case Related(to: Query, ToObject(Declares)) =>

      // get the db instance from the path
      val tPath = e.evalSinglePath(to) match {
        case m : MPath => m
        case _ => error("Ill-typed query: Expected ToObject(Declares) to give an MPath")
      }

      // get the right db
      val db = DB.fromPath(tPath, allowSchemaPath = false).getOrElse {
        error("Unable to assign lmfdb database. ")
      }

      // create a new result set
      ResultSet.fromElementList(getAll(db))
    case _ =>
      throw ImplementationError("LMFDB QueryEvaluator() does not support query " + q)
  }
}