package info.kwarc.mmt.api.metadata
import info.kwarc.mmt.api._
import objects._
import utils._
import utils.MyList._

import scala.xml._

/**
 * The trait MetaData is mixed into any class that can carry metadata
 */
trait HasMetaData {
   var metadata = new MetaData
   def getMetaDataNode : NodeSeq = if (metadata.getAll.isEmpty) Nil else metadata.toNode 
}

/**
 * represents a list of metadata key-value pairs
 * duplicate keys or key-value pairs are permitted
 */
class MetaData {
   protected var data: List[MetaDatum] = Nil
   /** add metadata item, order of insertion is preserved */
   def add(newdata: MetaDatum*) {data = data ::: newdata.toList}
   def delete(key: GlobalName) {
      data = data.filter(md => md.key != key)
   }
   def update(key: GlobalName, values: List[Obj]) {
      delete(key)
      values map  {value => add(new MetaDatum(key, value))}
   }
   def keys = data.map(_.key).distinct
   /** get all metadata */
   def getAll : List[MetaDatum] = data
   /** get metadata for a certain key */
   def get(key: GlobalName) : List[MetaDatum] = data.filter(_.key == key)
   def getValues(key: GlobalName) : List[Obj] = get(key).map(_.value)
   def getLink(key: GlobalName) : List[URI] = data.mapPartial {
      case Link(k, u) if k == key => Some(u)
      case _ => None
   }
   def toNode = <metadata>{data.map(_.toNode)}</metadata>
   // def toString
}

/** helper object */
object MetaData {
   /** returns the argument Node without its metadata child (if any), at most one such child allowed, may occur anywhere */
   def parseMetaDataChild(node : Node, base: Path) : (Node, Option[MetaData]) = {
      val (newnode, mdxml) = node match {
        case scala.xml.Elem(p,l,a,s,cs @ _*) =>
           var md : Option[Node] = None
           val cs2 = cs flatMap {
              case e @ <metadata>{_*}</metadata> => md match {
                 case None => md = Some(e); Nil
                 case Some(_) => throw ParseError("only one metadata child allowed: " + node)
              }
              case e => e
           }
           (scala.xml.Elem(p,l,a,s,cs2 : _*), md)
        case n => (n, None)
      }
      (newnode, mdxml.map(d => parse(d, base)))
   }
   /** parses a MetaData */
   def parse(node: Node, base: Path) : MetaData = node match {
      case <metadata>{mdxml @ _*}</metadata> =>
         val mdata = new MetaData
         mdxml foreach {n => mdata.add(MetaDatum.parse(n, base))}
         mdata
      case _ => throw ParseError("metadata expected: " + node) // TODO parse meta and link
   }
   def apply(pairs : MetaDatum*) : MetaData = {
     val metadata = new MetaData
     metadata.add(pairs: _*)
     return metadata
   }
}

/**
 * an individual MetaDatum
 * @param key the key, must be a symbol in the theory of the respective metadata theory
 * @param value the object, must be well-formed object over the respective metadata theory
 */
class MetaDatum(val key: GlobalName, val value: Obj) {
   def toNode = this match {
      case Link(key, uri) => <link rel={key.toPath} resource={uri.toString}/> 
      case _ => <meta property={key.toPath}>{value.toOBJNode}</meta> 
   }
   // def toString
}

/** helper object */
object MetaDatum {
   val keyBase = DPath(URI("http", "purl.org") / "dc" / "terms") ? "_"
   /** parses a MetaDatum */
   def parse(node: Node, base: Path) : MetaDatum = node match {
      case <link/> =>
         val key = Path.parseS(xml.attr(node, "rel"), keyBase)
         Link(key, URI(xml.attr(node, "resource")))
      case Elem(_,"meta",_,_,literal @ _*) => //strangely, XML matching does not work
         val key = Path.parseS(xml.attr(node, "property"), keyBase)
         new MetaDatum(key, OMSTR(literal.text)) // TODO: for now parsing everything into a string
         //throw ParseError("object in metadatum must be text node:" + node)
      case _ => throw ParseError("meta or link expected: " + node)
   }
}


/** Helper object to distinguish MetaDatum's whose objects are URIs (<link>) from other ones (<meta>) */
object Link {
   def apply(key: GlobalName, uri: utils.URI) = new MetaDatum(key, OMURI(uri))
   def unapply(d: MetaDatum) : Option[(GlobalName, utils.URI)] = d.value match {
      case OMURI(uri) => Some((d.key, uri))
      case _ => None
   }
}