package info.kwarc.mmt.api.backend

import info.kwarc.mmt.api._
import documents._
import modules._
import symbols._
import objects._
import frontend._
import utils._
import parser._

import scala.xml.{MetaData,NamespaceBinding,Elem,NodeSeq}
import scala.xml.parsing.ConstructingParser
import scala.io.Source

/** an awkward conversion from java.io.Reader to scala.io.Source
 *  needed in [[XMLStreamer]] because ParsingStream uses the former, but ConstructingParser the latter 
 */
class SourceFromReader(r: java.io.Reader) extends Source {
   val iter = new Iterator[Char] {
      private var lastRead: Option[Char] = null
      private def readOne {
         if (lastRead == null) {
            val c = r.read
            lastRead = if (c == -1) None else Some(c.toChar)
         }
      }
      def next = {
         readOne
         val c = lastRead.get
         lastRead = null
         c
      }
      def hasNext = {
         readOne
         lastRead.isDefined
      }
   }
}

/** a straightforward ObjectParser that relegates to Obj.parse */
object XMLObjectParser extends ObjectParser {
   def isApplicable(s: String) = s == "openmath"
   def apply(pu: ParsingUnit)(implicit errorCont: ErrorHandler): Term = {
      val xml = scala.xml.XML.loadString(pu.term)
      val o = Obj.parseTerm(xml, pu.nsMap)
      o
   }
}

/** 
 * similar to [[XMLReader]] but streams parsed elements into the continuation function
 */
// TODO DefinedModules and views with from/to children are parsed wrong if are streamed
class XMLStreamer extends Parser(XMLObjectParser) {streamer =>
   val format = "omdoc"
   override val logPrefix = "streamer"
   private lazy val reader = new XMLReader(report)

   /** the structural elements whose children are processed immediately */
   private val containers = List("omdoc", "theory")

   def apply(ps: parser.ParsingStream)(implicit errorCont: ErrorHandler): Document = {
      val parser = new ConsParser(ps.dpath, new SourceFromReader(ps.stream), errorCont)
      try {
         parser.nextch
         parser.document()
      } catch {
         case e: Error => errorCont << e
         case e: Exception => errorCont << LocalError("error while parsing XML").setCausedBy(e)
      }
      parser.root
   }
   
   /** XML parser that streams documents/modules and calls the reader on the other ones */
   private class ConsParser(dpath: DPath, input: Source, errorCont: ErrorHandler) extends ConstructingParser(input, true) {
      /** the stack of currently open tags, innermost first */
      private var openTags : List[StructuralElement] = Nil
      /** holds the root element once parsing has finished */
      var root: Document = null
   
      private def add(se: StructuralElement) {
         try {
            controller.add(se)
         } catch {case e: AddError =>
            // errors in the XML are usually minor and  we can assume we can recover 
            errorCont << e
         }
      }
      /** like add, but also pushes the parsed element onto openTags */
      private def catchSE(se: StructuralElement) {
         add(se)
         // hack: push the expected container element (will also be called for XRef's though)
         se match {
            case _:Document | _:DeclaredModule => openTags ::= se
            case nm: NestedModule => openTags ::= nm.module
            case _ =>
         }
      }
      
      // called when an opening tag is encountered
      override def elemStart(pos: Int, pre: String, label: String, attrs: MetaData, scope: NamespaceBinding) {
         // construct empty element
         lazy val elem = Elem(pre, label, attrs, scope, true)
         // if this is a container element, we read the empty one immediately and push it to openTags
         label match {
            case "omdoc" if openTags.isEmpty =>
               // toplevel container
               streamer.log("streaming in top element")
               reader.readDocument(dpath, elem)(catchSE)
               root = openTags.head.asInstanceOf[Document]
            case l if containers contains l =>
               // nested container
               streamer.log("streaming in " + l)
               reader.readIn(root.getNamespaceMap, openTags.head, elem)(catchSE)
            case _ =>
               // in all other cases, we push a dummy element
               openTags ::= null
         }
      }

      // called when a closing tag is encountered
      // the return value of this method replaces the node
      override def elem(pos: Int, pre: String, label: String, attrs: MetaData,
                        scope: NamespaceBinding, empty: Boolean, nodes: NodeSeq) = {
         // the element to be read
         val n = Elem(pre, label, attrs, scope, empty, nodes:_*)
         // pop n from openTags
         openTags = openTags.tail
         // check where we are in the XML
         openTags match {
            case null :: _ =>
               // n is some inner node: return it (same behavior as the super method)
               n
            case hd :: _ =>
               // n is the child of a container element
               // if n is a container itself, it was already handled in elemStart
               //   otherwise, we process it now
               if (containers.contains(label)) {
                 streamer.log("done streaming in " + label)
               } else {
                 streamer.log("processing " + label)
                 reader.readIn(root.getNamespaceMap, hd, n)(add)
               }
               // either way, n is handled at this point and can be dropped
               NodeSeq.Empty
            case Nil =>
               // n is the root node (which may not be empty)
               streamer.log("done streaming in top element")
               <dummy/>
         }
      }
   }
}