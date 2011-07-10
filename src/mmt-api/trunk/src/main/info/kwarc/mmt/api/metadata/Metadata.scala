package info.kwarc.mmt.api.metadata
import info.kwarc.mmt.api._
import objects._
import utils._

import scala.xml._

/**
 * The trait MetaData is mixed into any class that can carry metadata
 */
trait HasMetaData {
   private var metadataOpt : Option[MetaData] = None
   /** sets the metadata, may be called once */
   def setMetaData(md: MetaData) {setMetaData(Some(md))}
   /** convenience method to set MetaData */
   def setMetaData(md: Option[MetaData]) {
      metadataOpt match {
         case None => metadataOpt = md
         case Some(_) => throw AddError("metadata already defined") 
      }
   }
   /** get the metadata object */
   def metadata : MetaData = metadataOpt match {
      case None => throw new GetError("no metadata defined")
      case Some(m) => m
   }
}

/**
 * represents a list of metadata key-value pairs
 * duplicates keys or key-value pairs are permitted
 * @param language the theory from which the keys are taken
 */
class MetaData(language: MPath) {
   protected var data: List[MetaDatum] = Nil
   /** add metadata item, order of insertion is preserved */
   def add(newdata: MetaDatum*) {data = data ::: newdata.toList}
   /** get all metadata */
   def getAll : List[MetaDatum] = data
   /** get metadata for a certain key */
   def get(key: GlobalName) : List[MetaDatum] = data.filter(_.key == key)
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
         val lang = Path.parseM(xml.attr(node, "language"), base)
         val mdata = new MetaData(lang)
         mdxml foreach {n => mdata.add(MetaDatum.parse(n, lang))}
         mdata
      case _ => throw ParseError("metadata expected: " + node) // TODO parse meta and link
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
      case _ => <meta property={key.toPath}>{value.toNode}</meta> 
   }
   // def toString
}

/** helper object */
object MetaDatum {
   /** parses a MetaDatum */
   def parse(node: Node, base: Path) : MetaDatum = node match {
      case <link/> =>
         val key = Path.parseS(xml.attr(node, "rel"), base)
         Link(key, URI(xml.attr(node, "resource")))
      case Elem(_,"meta",_,_,literal @ _*) => //strangely, XML matching does not work
         val key = Path.parseS(xml.attr(node, "property"), base)
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