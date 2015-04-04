package info.kwarc.mmt.api.backend

import info.kwarc.mmt.api._
import documents._
import modules._
import frontend._
import utils._

import scala.xml.{MetaData,NamespaceBinding,Elem,NodeSeq}
import scala.xml.parsing.ConstructingParser
import scala.io.Source

/** 
 * similar to [[XMLReader]] but streams parsed elements into the continuation function
 */
// DefinedTheory and DefinedView are parsed wrong
class XMLStreamer(controller: Controller) extends Logger {streamer =>
   private lazy val reader = controller.xmlReader
   val report = controller.report
   val logPrefix = "streamer"
   /** the elements whose children are processed immediately */
   private val containers = List("omdoc", "theory", "view")
   
   /**
    * @param dpath the URI of the document
    * @param input the document
    */
   def readDocument(dpath: DPath, input: Source)(implicit cont: StructuralElement => Unit): Document = {
      val parser = new Parser(dpath, input, cont)
      parser.nextch
      parser.document()
      parser.root
   }

   private class Parser(dpath: DPath, input: Source, cont: StructuralElement => Unit) extends ConstructingParser(input, false) {
      /** the stack of currently open tags, innermost first */
      private var openTags : List[StructuralElement] = Nil
      /** holds the root element once parsing has finished */
      var root: Document = null
   
      /** like cont, but also pushes the parsed element onto openTags */
      private def catchSE(se: StructuralElement) {
         cont(se)
         // hack: push the expected container element (will also be called for XRef's though)
         se match {
            case _:Document | _:DeclaredModule => openTags ::= se
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
                 reader.readIn(root.getNamespaceMap, hd, n)(cont)
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