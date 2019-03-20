package info.kwarc.mmt.sql

import info.kwarc.mmt.api._
import metadata._
import uom._

import info.kwarc.mmt.lf._


object MathData extends TheoryScala {
  val _base = DPath(utils.URI("http://data.mathhub.info/schemas"))
  val _name = LocalName("MathData")
  
  val bool   = _path ? "bool"
  val int    = _path ? "int"
  val string = _path ? "string"
  val uuid   = _path ? "uuid"
}

object DbData extends TheoryScala {
  val _base = DPath(utils.URI("http://data.mathhub.info/schemas"))
  val _name = LocalName("DbData")
  
  val bool   = _path ? "db_bool"
  val int    = _path ? "db_int"
  val string = _path ? "db_string"
  val uuid   = _path ? "db_uuid"
  object array extends UnaryLFConstantScala(_path, "db_array")
}

object Codecs extends TheoryScala {
  val _base = MathData._base
  val _name = LocalName("Codecs")
  object codec extends BinaryLFConstantScala(_path, "codec")
  val BoolIdent   = _path ? "BoolIdent"
  val IntIdent    = _path ? "IntIdent"
  val StringIdent = _path ? "StringIdent"
  val UUIDIdent   = _path ? "UUIDIdent"
  
  val codecAnnotator = new TermAnnotator(codec.path)
}

object SchemaLang extends TheoryScala {
  val _base = MathData._base
  val _name = LocalName("MDDL")
  val foreignKey = new Tagger(_path ? "foreignKey")
  val opaque     = new Tagger(_path ? "opaque")
  val hidden     = new Tagger(_path ? "hidden")
  val collection = new Tagger(_path ? "collection")
  val schemaGroup = _path ? "schemaGroup"
}