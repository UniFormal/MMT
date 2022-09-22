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
   def add(newdata: MetaDatum*): Unit = {data = data ::: newdata.toList}
   def delete(key: GlobalName): Unit = {
      data = data.filter(md => md.key != key)
   }
   def update(md: MetaDatum): Unit = {
      update(md.key, md.value)
   }
   def update(key: GlobalName, values: Obj*): Unit = {
      delete(key)
      values foreach  { value => add(MetaDatum(key, value))}
   }
   def update(key: GlobalName, value: URI): Unit = {
      delete(key)
      add(Link(key, value))
   }
   def keys: List[GlobalName] = data.map(_.key).distinct
   /** get all metadata except tags */
   def getAll : List[MetaDatum] = data.filter(_.value != null)
   /** get metadata for a certain key */
   def get(key: GlobalName) : List[MetaDatum] = data.filter(_.key == key)
   def getValues(key: GlobalName) : List[Obj] = get(key).map(_.value)
   def getLinks(key: GlobalName) : List[URI] = data.flatMap {
      case Link(k, u) if k == key => List(u)
      case _ => Nil
   }
   def getTags : List[GlobalName] = data.flatMap {
      case Tag(key) => List(key)
      case _ => Nil
   }
   def toNode = if (data.isEmpty) Nil else <metadata>{data.map(_.toNode)}</metadata>
   override def toString: String = data.map(_.toString).mkString(", ")
}

/** helper object */
object MetaData {
   /** returns the argument Node without its metadata child (if any), at most one such child allowed, may occur anywhere */
   def parseMetaDataChild(node : Node, nsMap: NamespaceMap) : (Node, Option[MetaData]) = {
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
           (scala.xml.Elem(p,l,a,s,true,cs2 : _*), md)
        case n => (n, None)
      }
      (newnode, mdxml.map(d => parse(d, nsMap)))
   }
   /** parses a MetaData */
   def parse(node: Node, nsMap: NamespaceMap) : MetaData = xml.trimOneLevel(node) match {
      case <metadata>{mdxml @ _*}</metadata> =>
         val mdata = new MetaData
         mdxml foreach {n => mdata.add(MetaDatum.parse(n, nsMap))}
         mdata
      case _ => throw ParseError("metadata expected: " + node) // TODO parse meta and link
   }
   def apply(pairs : MetaDatum*) : MetaData = {
     val metadata = new MetaData
     metadata.add(pairs: _*)
     metadata
   }
}

/**
 * an individual MetaDatum
 * @param key the key, must be a symbol in the theory of the respective metadata theory
 * @param value the object (may be null, which indicates tags)
 */
case class MetaDatum(key: GlobalName, value: Obj) {
   def toNode: Elem = this match {
      case Link(key, uri) => <link rel={key.toPath} resource={uri.toString}/>
      case Tag(key) => <tag property={key.toPath}/>
      case _ => <meta property={key.toPath}>{value.toOBJNode}</meta>
   }
   override def toString: String = {
      if (value == null) key.toString
      else {key.toString + " -> " + value.toString}
   }
}

/** helper object */
object MetaDatum {
   val keyBase: MPath = documents.NarrativeMetadata.keyBase
   /** parses a MetaDatum */
   def parse(node: Node, nsMap: NamespaceMap) : MetaDatum = xml.trimOneLevel(node) match {
      case <link/> =>
         val key = Path.parseS(xml.attr(node, "rel"), nsMap(keyBase))
         Link(key, URI(xml.attr(node, "resource")))
      case <tag/> =>
         val key = Path.parseS(xml.attr(node, "property"), nsMap(keyBase))
         Tag(key)
      case n: Elem if n.label == "meta" && n.child.nonEmpty =>
         // some text is split into several child nodes
         val key = Path.parseS(xml.attr(node, "property"), nsMap(keyBase))
         val literal = n.child.head
         val value = if (literal.isInstanceOf[Elem] && n.child.length == 1)
            Obj.parseTerm(literal, nsMap)
         else
            // fallback: string-valued meta-data
            uom.OMLiteral.OMSTR(literal.text)
         new MetaDatum(key, value)
         //throw ParseError("object in metadatum must be text node:" + node)
      case _ => throw ParseError("meta or link or tag expected: " + node)
   }
}


/** Helper object to distinguish MetaDatum's whose objects are URIs (<link>) from other ones (<meta>) */
object Link {
   def apply(key: GlobalName, uri: utils.URI) = new MetaDatum(key, uom.OMLiteral.URI(uri))
   def unapply(d: MetaDatum) : Option[(GlobalName, utils.URI)] = d.value match {
      case uom.OMLiteral.URI(uri) => Some((d.key, uri))
      case _ => None
   }
}

/** apply/unapply methods for tags: a tag is a MetaDatum whose value is null */
object Tag {
   def apply(key: GlobalName) = new MetaDatum(key, null)
   def unapply(d: MetaDatum) : Option[GlobalName] = if (d.value == null) Some(d.key) else None
}
