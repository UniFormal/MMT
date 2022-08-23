package info.kwarc.mmt.odk.LMFDB

import info.kwarc.mmt.api._
import backend._
import frontend._
import metadata.GlobalNameLinker
import ontology._
import ontology.{BaseType, Query, QueryEvaluator}
import modules._
import objects._
import symbols._
import utils._
import valuebases._
import info.kwarc.mmt.api.web.WebQuery

import info.kwarc.mmt.lf.Apply

import info.kwarc.mmt.MitM.{MitM, MitMSystems}
import info.kwarc.mmt.MitM.VRESystem._

import info.kwarc.mmt.odk._
import info.kwarc.mmt.odk.codecs.LMFDBCoder


import scala.collection.mutable.HashSet
import scala.util.Try

class Plugin extends frontend.Plugin {
  val theory = MitM.mathpath
  val dependencies = List("info.kwarc.mmt.lf.Plugin")
  override def start(args: List[String]): Unit = {
    controller.backend.addStore(new LMFDBStore {
      def debug(s: String) = report("lmfdb", s)
    })
    controller.extman.addExtension(new ImplementsRuleGenerator)
    controller.extman.addExtension(new LMFDBSystem)
  }
}

object LMFDB {
   val path = DPath(URI("http","www.lmfdb.org"))
   val schemaPath = path / "schema"
   val dbPath = path / "db"
   val uri = URI("http://beta.lmfdb.org") / "api"

  val databases = List(
    dbPath ? "ec_curves",
    dbPath ? "gps_transitive",
    dbPath ? "hmf_fields",
    dbPath ? "hmf_forms",
    dbPath ? "hmf_hecke"
  )
}


object Metadata {
   val path = LMFDB.path ? "Metadata"
   val implements = path ? "implements"
   val ImplementsLinker = new GlobalNameLinker(implements)
   val extend = path ? "extends"
   val constructor = path ? "constructor"
   val ConstructorLinker = new GlobalNameLinker(constructor)
   val key = path ? "key"
   val codec = path ? "codec"
}

case class DBField(jsonKey: String, codec: Codec[JSON])
case class DBTheory(tp : Term, constructor : Term, parents : List[(DB,Theory)] = Nil) {
  def +(th : Theory)(implicit controller: Controller) = DBTheory(tp,constructor,
    (DB.fromPath(th.path).getOrElse{throw BackendError("Not a schema theory",th.path)},th) :: parents)
}

case class DB(suffix: LocalName, mod: LocalName) {
   def schemaPath: MPath = (LMFDB.schemaPath / suffix) ? mod
   def dbPath: MPath = (LMFDB.dbPath / suffix) ? mod
   def dbpath: String = suffix.toString + "/" + mod.toString
   /** the uri to a given query */
   def queryurl(query: String): URI = {
     LMFDB.uri / s"${dbpath.stripPrefix("/")}?_format=json$query"
   }
}

object DB {
  // def schema2db(sPath : MPath) : MPath = fromPath(sPath).get.dbPath
  // def db2Schema(dbPath : MPath) : MPath = fromPath(dbPath).get.schemaPath

  /** turn an LMFDB db or schema path into a DB object that repreesnts both
   *  @param pth the path
   *  @param allowDPBPath test if pth is a db path
   *  @param allowSchemaPath test if pth is a schema path
   */
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
    if (suffixS.isEmpty && suffixD.isEmpty)
      return None
    // and get the db object
    Some(DB(suffixS.getOrElse({suffixD.get}), mod.get))
  }
}
/**
  * Shared code for LMFDB Queries between the Storage and the backend
  */
trait LMFDBBackend {

  def debug(s : String) : Unit

  protected def toOML(json: JSONObject, db: DB, fields: List[DBField])(implicit controller: Controller): List[OML] = {
    fields map {case DBField(key, codec) =>
      val dfJ = json(key).getOrElse {
        throw BackendError("could not find JSON value " + key + " in \n" + json, db.schemaPath) //TODO use correct path
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
    println(url) // for debug
    val attempt = Try(io.Source.fromURL(url.toString))
    if (attempt.isFailure) None else {
      val json = Try(JSON.parse(attempt.get.toBuffer.mkString))
      if (json.isFailure) throw ParseError(url.toString).setCausedBy(json.failed.get)
      json.toOption
    }
  }

  /**
    * Retrieves JSON from a URL and recurses into a key called next until retrieval fails
    * @param url
    * @return
    */
  private def get_json_withnext(url : URI, getter: JSON => List[JSON], limit: Option[Int], next_key : String = "next") : List[JSON] = {
    // get the current page
    val data = get_json(url) match {
      case None => return Nil
      case Some(x) => x
    }
    // get the next url
    val nextUrl = data match {
      case j: JSONObject => j(next_key) match {
        case Some(JSONString(s)) =>
          val nurl = url.resolve(s)
          if(nurl != url){ Some(nurl) } else { None }
        case _ => None
      }
      case _ => None
    }
    // get the current data
    val ary = getter(data)
    // should we get more data?
    val shouldGetNext = nextUrl match {
      case Some(_) => limit match {
        case Some(l) => ary.length < l
        case None => true
      }
      case None => false
    }
    // if we have more elements, get them
    if(shouldGetNext) {
      ary ::: get_json_withnext(nextUrl.get, getter, limit.map(_ - ary.length), next_key)
    // else take as many as the limit wants
    } else {
      limit match {
        case Some(l) => ary.take(l)
        case None => ary
      }
    }
  }

  /** runs a simple lmfdb query */
  protected def lmfdbquery(db:DB, query: String, limit: Option[Int]) : List[JSON] = {
    // get the url
    val url = db.queryurl(query)
    debug(s"attempting to retrieve json from $url")
    // get all the data items
    get_json_withnext(url, _.asInstanceOf[JSONObject]("data").get.asInstanceOf[JSONArray].values.toList, limit)
  }

  /** finds an implementor */
  protected def findImplementor(schema: Theory, forSymbol: GlobalName, err : String => Unit)(implicit controller: Controller) : (DB, GlobalName) = {
    val symbol = collectImplementMetaData(schema, forSymbol).map(_.path).headOption.getOrElse({
      err(s"no implementor found for $forSymbol in ${schema.path}")
      null
    })

    val db = DB.fromPath(symbol, allowDBPath = false, allowSchemaPath = true).getOrElse({
      err(s"implementor $symbol (for $forSymbol) not in any schema theory")
      null
    })

    (db, symbol)
  }

  protected def collectImplementMetaData(schema: Theory, forSymbol: GlobalName)(implicit controller: Controller): List[Declaration] = {
    val parents = schema.metadata.getValues(Metadata.extend)
      .collect({ case OMID(path: MPath) => controller.getTheory(path)})
      .flatMap(collectImplementMetaData(_, forSymbol))
    val current = schema.getDeclarations.filter({ d =>
        d.metadata.getValues(Metadata.implements)
          .collect({ case OMLIT(uri: URI, _) => uri})
          .contains(URI(forSymbol.toString))
    })
    parents ::: current
  }

  /** find the extensions of db in reverse order, that is the smallest non-extending theory first */
  protected def findExtensions(db: DB)(implicit controller: Controller): List[DB] = {
    // TODO: Tom: See if we have use getTP(db)
    // get the schema theory
    val schema = controller.getTheory(db.schemaPath)

    // find the parents
    val parents = schema.metadata.getValues(Metadata.extend)
      .collect({ case OMID(path: MPath) => DB.fromPath(path, allowDBPath = false, allowSchemaPath = true)}).flatten

    // and recursve
    parents.flatMap(findExtensions) ::: List(db)
  }

  protected def getKey(schema: Theory) : String = {
    def err(s: String) = throw BackendError(s,schema.path)
    // HACK HACK HACK; parse the ODK String
    val spath = OMS(Path.parseS("http://mathhub.info/MitM/Foundation?Strings?string", NamespaceMap.empty))

    schema.metadata.getValues(Metadata.key).headOption.getOrElse {
      err("metadata key 'key' not found in schema: " + schema.path); null
    } match {
      case StringLiterals(k : String) =>
        k // This no longer works; we are using the HACK below
      case UnknownOMLIT(a, `spath`) =>
        a.toString
      case s => err("metadata key 'key' is not a string in schema: " + schema.path); null
    }
  }

  def getTP(db : DB)(implicit controller: Controller) : DBTheory = getTP(db,db.schemaPath)

  private def getTP(db : DB, mp : MPath)(implicit controller: Controller) : DBTheory = {
    def err(s: String) = throw BackendError(s,mp)
    val th = Try(controller.getTheory(mp)).toOption match {
      case Some(t : Theory) =>
        // db.addParent(t)
        t
      case _ =>
        err("Not a DeclaredTheory or Theory not found")
    }
    th.metadata.getValues(Metadata.implements).headOption.getOrElse {
      val ext = th.metadata.getValues(Metadata.extend).headOption match {
        case Some(OMMOD(mp2)) =>
          return getTP(db,mp2) + th
        case _ =>
          err("metadata key 'implements' or 'extends' not found in schema")
      }
    } match {
      case t: Term =>
        th.metadata.getValues(Metadata.constructor).headOption.getOrElse {
          err("metadata key 'constructor' not found in schema")
        } match {
          case t2: Term =>
            DBTheory(t,t2) + th
          case _ =>
            err("metadata key 'constructor'  found in schema")
        }
      case _ =>
        err("metadata key 'implements' or 'extends' not found in schema")
    }
  }
}

object LMFDBStore {
  /** produces the db-theory for a given schema-theory */
  def getOrAddVirtualTheory(controller: Controller, schemaTheory: Theory): Option[Theory] = {
    val db = DB.fromPath(schemaTheory.path, allowDBPath = false).getOrElse(return None)
    val dbp = db.dbPath
    // create it if it is not in memory yet
    controller.localLookup.getO(dbp) match {
      case Some(dbt: Theory) => Some(dbt)
      case Some(_) => None
      case None =>
        val dbThy = Theory.empty(dbp.parent, dbp.name, schemaTheory.meta)
        controller.add(dbThy)
        Some(dbThy)
    }
  }
}


abstract class LMFDBStore extends Storage with LMFDBBackend {
  def load(path: Path)(implicit controller: Controller): Unit = {
    val db = DB.fromPath(path, allowSchemaPath = false).getOrElse {
      throw NotApplicable("not an LMFDB theory")
    }
    path.dropComp match {
       case p: GlobalName => loadConstant(p, db)
       case mp : MPath =>
        // retrieving the schema-theory may already create the db-theory
        val sch = Try(controller.getTheory(db.schemaPath)).toOption match {
           case Some(t : Theory) => t
           case _ => throw NotApplicable("schema theory not found")
        }
        LMFDBStore.getOrAddVirtualTheory(controller, sch)
       case _ => throw ImplementationError("invalid dropComp path")
    }
  }

  override def loadFragment(needed: Path, known: Path)(implicit controller: Controller): Unit = {
    load(needed)
  }

  private def loadConstant(path: GlobalName, db: DB)(implicit controller: Controller): Unit = {

    // find the type we are implementing, getting from records --> instance
    val dbT = getTP(db)

    val omls = dbT.parents.flatMap { case (ndb,s) =>
      val key = getKey(s)
      val fields = s.getConstants.flatMap {c =>
        c.metadata.getValues(Metadata.codec).headOption.toList.collect {
          case codecExp: Term =>
            DBField(c.name.toString, LMFDBCoder.buildCodec(codecExp))
        }
      }
      // TODO
      val query = "&" + key + "=" + path.name.toString
      val json = lmfdbquery(ndb, query, None).head.asInstanceOf[JSONObject]
      toOML(json,db,fields)
    }
    /*

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
     */

     val df = LFX.RecExp(omls : _*)

     val c = Constant(OMMOD(db.dbPath), path.name, Nil, Some(dbT.tp), Some(Apply(dbT.constructor,df)), None)
     controller.add(c)
  }
}

class LMFDBSystem extends VRESystem("lmfdb",MitMSystems.lmfdbsym) with LMFDBBackend with LMFDBQueryIterators with LMFDBQueryParser {//QueryExtension("lmfdb") with LMFDBBackend {
  def warmup(): Unit = {}


  val namespace = LMFDB.path

  def debug(s : String): Unit = log(s)

  protected def error(msg: String) = {
    throw ImplementationError("LMFDB QueryEvaluator() failed to evaluate query. " + msg)
  }

  /** retrieves a set of urls from a set of databases subject to a given set of conditions */
  private def getSubjectToJoin(primary: DB, queries: List[(DB, String, JSON)], from: Option[Int], until: Option[Int])(implicit controller: Controller): List[GlobalName] = {
    // group the passed in queries by database
    val dbQueryMap = queries.groupBy(_._1).view.mapValues({l => JSONObject(l.map({ case (d, k, v) => (JSONString(k), v)}))})

    // generate all the queries we need
    val groupedQueries = findExtensions(primary).map(db => (dbQueryMap.getOrElse(db, JSONObject()), db))

    // and make the iterator
    val iterator = makeQueryIterator(groupedQueries)

    // get all the results
    val theResults = (from, until) match {
      case (None, None) => iterator.all
      case (Some(n), None) => iterator.drop(n).all
      case (None, Some(m)) => iterator.take(m)
      case (Some(n), Some(m)) => iterator.slice(n, m)
    }

    val schema = controller.get(primary.schemaPath) match {
      case dt:Theory => dt
      case _ => error("Schema-Theory missing from controller")
    }
    val key = getKey(schema)

    theResults.map({l =>
      // create all the constants as side effects
      l.foreach({
        case (j: JSONObject, db: DB) =>
          constantFromJson(getTP(db), j)
      })

      // and build the global names
      primary.dbPath ? l.last._1.getAsString(key)
    })
  }

  /**
    * builds a constant from some constructed json
    * @param dbT
    * @param j
    * @param controller
    * @return
    */
  private def constantFromJson(dbT : DBTheory, j : JSONObject)(implicit controller: Controller) : Option[GlobalName] = {
    val (omls,path) = dbT.parents.headOption.map { case (db,schema) =>
      val key = getKey(schema)
      val cpath = db.dbPath ? j(key).collect{case JSONString(st) => st}.getOrElse(???)
      if (dbT.parents.head._2.isDeclared(cpath.name)) return Some(cpath)

      log("materializing " + cpath)
      val oldfields : List[OML] = dbT.parents.tail.map { case (idb,s) =>
        //val ikey = getKey(s)
        val idbT = getTP(idb)
        val ipath = idb.dbPath ? cpath.name
        controller.getO(ipath) match {
          case Some(c : FinalConstant) =>
            c.df match {
              case Some(Apply(idbT.constructor,LFX.RecExp(bd))) =>
                bd.fields
            }
          case _ => ???
        }
      }.reverse.flatten
      val fields = schema.getConstants.flatMap {c =>
        c.metadata.getValues(Metadata.codec).headOption.toList.collect {
          case codecExp: Term =>
            DBField(c.name.toString, LMFDBCoder.buildCodec(codecExp))
        }
      }
      (oldfields ::: toOML(j,db,fields),cpath)
    }.getOrElse(???)

    val df = LFX.RecExp(omls : _*)

    // create the constant and add it to the ontroller
    val c = Constant(OMMOD(path.module), path.name, Nil, Some(dbT.tp), Some(Apply(dbT.constructor,df)), None)
    controller.add(c)
    None
  }

  /**
    * Evaluates a Query using the lmfdb api
    * @param q           Query to evaluate
    * @param e           A QueryEvaluator to use for recursive queries
    * @param substiution Substiution (Context) to apply QueryEvaluation in
    * @return
    */
  override def evaluate(q: Query, e: QueryEvaluator)(implicit substiution: QueryEvaluator.QuerySubstitution): HashSet[List[BaseType]] = {
    val (db, frm, until, query) = translateQuery(q, e)
    val results = getSubjectToJoin(db, query, frm, until)(controller)
    ResultSet.fromElementList(results)
  }

  override def call(t: Term)(implicit trace: MitMComputationTrace): Term = throw LocalError("LMFDB is not callable") // TODO awkward
}

trait LMFDBQueryParser { this: LMFDBSystem =>
  /**
    * attempts to translate a query to an lmfdb query.
    *
    * This function does a partial evaluation, in the sense that every Related(..., ToObject(Declares)) query is evaluated.
    *
    * May throw an [[ImplementationError]] if the query can not be evaluated using lmfdb.
    *
    *
    * @param q
    * @param e
    * @param substiution
    * @return
    */
  protected def translateQuery(q: Query, e: QueryEvaluator)(implicit substiution: QueryEvaluator.QuerySubstitution): (DB, Option[Int], Option[Int], List[(DB, String, JSON)]) = q match {
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

      (db, None, None, Nil)

    case Comprehension(domain: Query, varname: LocalName, pred: Prop) =>
      val (db, frm, until, q) = translateQuery(domain, e)
      (db, frm, until, translateProp(varname, pred, db)(controller))

    case Slice(qq, frm, until) =>
      val (db, f, u, q) = translateQuery(qq, e)
      if (f.nonEmpty || u.nonEmpty) {
        error("Nested slicing not supported")
      }
      (db, frm, until, q)
    case _ =>
      error("Unable to translate query into an lmfdb query, evaluation failed")
  }

  /** translates a proposition (about a given variable assumed to be an lmfdb query) into an lmfdb query */
  private def translateProp(varname: LocalName, p : Prop, db: DB)(implicit controller: Controller) : List[(DB, String, JSON)] = p match {
    case Holds(Bound(`varname`), Equals(q, l, r)) =>

      // match the symbol that is being applied
      val symbol = l match {
        case Apply(OMS(s), OMV(`q`)) => s
        case OMA(OMS(s), List(OMV(`q`))) => s
        case o@_ =>
          error(s"Unable to translate predicated into an lmfdb field selector. Expected LF Application, on the left hand side $o. ")
      }

      // find the symbol that is implemented by the operation
      val (actualDB, implementedSymbol) = findImplementor(controller.getTheory(db.schemaPath), symbol, error)

      // the name of the field to look for
      val field = implementedSymbol.name.toPath

      // find the codec we are using for the value
      val codec = controller.getConstant(implementedSymbol).metadata.getValues(Metadata.codec).headOption.map {
        case codecExp: Term =>
          LMFDBCoder.buildCodec(codecExp)
      }.getOrElse(error(s"unable to find codec for $symbol, evaluation failed. "))

      // encode the actual value
      val value = codec.encode(r)

      // and make it a JSONObject
      List((actualDB, field, value))
    case And(left, right) =>
      // and => we just join the query and all fields will need to be evaluated
      val lMap = translateProp(varname, left, db)
      val rMap = translateProp(varname, right, db)

      lMap ::: rMap
    case o@_ =>
      error(s"Unable to translate predicate into an lmfdb query, unsupported predicated $o. ")
  }
}