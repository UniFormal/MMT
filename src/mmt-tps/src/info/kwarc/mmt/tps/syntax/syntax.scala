package info.kwarc.mmt.tps.syntax

import info.kwarc.mmt.api.utils.{XMLToScala,Group}
import XMLToScala.checkString
import info.kwarc.mmt.tps.syntax

import scala.util.Try

/* Conventions needed for the XML parser
 *  * class names and field names correspond to the tag and attribute names in the XML syntax
 *  * exception: field names starting with _ have no analogue in the XML syntax, and the parser uses the next available XML object(s) to fill them
 */

// ****************************************
// ********** toplevel structure

/**
 * a pvs file containing some modules
 */

trait Module
trait Declaration
trait omobject

//meta stuff

trait meta {
  val _s:String
}
case class metadata($type:String,_metas:List[meta])

case class title(_s:String) extends meta
case class date(_s:String) extends meta
case class creator(role:String,_t:String) extends meta {
  override val _s = _t+":"+role
}
case class $format(_s:String) extends meta
case class source(_s:String) extends meta
case class rights(_s:String) extends meta
case class description(_s:String) extends meta
case class license(_lic:List[licenseinf]) extends meta {
  override val _s : String = _lic.map(x => x.getClass.getSimpleName+": "+x._s).mkString("\n")
}
//case class $type(_lic:List[licenseinf]) extends meta

trait licenseinf {
  val s : List[use]
  val _s : String = s.map(x => x.getClass.getSimpleName+"="+x.s).mkString(", ")
}
case class permissions(s:List[use]) extends licenseinf
case class prohibitions(s:List[use]) extends licenseinf
case class requirements(s:List[use]) extends licenseinf
trait use {
  val s:String
}
case class derivative_works(s:String) extends use
case class distribution(s:String) extends use
case class reproduction(s:String) extends use
case class commercial_use(s:String) extends use
case class copyleft(s:String) extends use
case class attribution(s:String) extends use
case class notice(s:String) extends use

//toplevel

case class omdoc(xmlns:String, dc:String, id:String, _meta:metadata, _modules: List[Module])

case class theory(id:String,_decls:List[Declaration]) extends Module

//decls

case class imports(from:String) extends Declaration

case class symbol(name:String,_meta:metadata,_tp:$type) extends Declaration

case class $type(system:String, _OMOBJ:OMOBJ)

case class definition($type:String, id:String, $for:String,_OMOBJ:OMOBJ) extends Declaration

case class assertion(id:String, $type:String,CMP:String,FMP:OMOBJ) extends Declaration

//notations
trait notat
trait subrendering

case class notation(prototype:omobject, _render:rendering) extends Declaration

case class rendering(precedence:String,_some:notat) {
  lazy val prec : Int = Try(precedence.toInt).getOrElse(0)
}
case class mo(_mo:String) extends notat with subrendering
case class mrow(_some:List[subrendering]) extends notat
case class render(name:String) extends subrendering {
  lazy val num : Option[Int] = if (name.startsWith("arg")) Some(name.substring(3).toInt) else None
}

//objects

trait vartype

case class OMOBJ(_obj:omobject)
// case class OMBVAR(vars:List[vartype])

case class OMV(name:String) extends omobject with vartype
case class OMATTR(_atp:OMATP,_var:OMV) extends vartype
case class OMATP(_head:OMS,_par:omobject)

case class OMA(_head:omobject, _pars:List[omobject]) extends omobject
case class expr(name:String) extends omobject
case class OMBIND(_symbol:OMS,OMBVAR:List[vartype],_pars:omobject) extends omobject
case class OMS(cd:String,name:String) extends omobject