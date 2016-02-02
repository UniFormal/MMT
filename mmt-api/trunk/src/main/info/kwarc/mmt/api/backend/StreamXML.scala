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
   def apply(pu: ParsingUnit)(implicit errorCont: ErrorHandler) = {
      val xml = scala.xml.XML.loadString(pu.term)
      val o = Obj.parseTerm(xml, pu.nsMap)
      ParseResult.fromTerm(o)
   }
}

/** 
 * similar to [[XMLReader]] but streams parsed elements into the continuation function
 */
class XMLStreamer extends Parser(XMLObjectParser) {streamer =>
   val format = "omdoc"
   override val logPrefix = "streamer"
   private lazy val reader = new XMLReader(controller)

   /** the structural elements whose children are processed immediately */
   private val containers = List("omdoc", "theory", "view")
   private val shapeRelevant = List("metadata", "parameters", "definition", "from", "to")

   def apply(ps: parser.ParsingStream)(implicit errorCont: ErrorHandler): StructuralElement = {
      val parser = new ConsParser(ps.parentInfo, new SourceFromReader(ps.stream), errorCont)
      try {
         parser.nextch
         parser.document()
      } catch {
         case e: Error => errorCont << e
         case e: Exception =>
            val le = LocalError("error while parsing XML").setCausedBy(e)
            if (parser.root == null)
               throw le
            else
               errorCont << le 
      }
      parser.root
   }
   
   private object Stack {
      abstract class StackElement
      case class UnparsedContainer(elem: Elem) extends StackElement {
         var children: List[Elem] = Nil
         def toNode = elem.copy(child = children.reverse)
      }
      case class ParsedContainer(container: StructuralElement) extends StackElement
      case class AppendUnparsed(container: UnparsedContainer) extends StackElement
      case class AppendParsed(container: ParsedContainer) extends StackElement
      case object Other extends StackElement
   }
   import Stack._
   
   /** XML parser that streams documents/modules and calls the reader on the other ones */
   private class ConsParser(parentInfo: ParentInfo, input: Source, errorCont: ErrorHandler) extends ConstructingParser(input, true) {
      /** the stack of currently open tags, innermost first */
      private var openTags : List[StackElement] = Nil
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
         se match {
            case _:Document | _:Module => openTags ::= ParsedContainer(se)
            case nm: NestedModule => openTags ::= ParsedContainer(nm.module)
            case _: MRef => // ignore the MRef generated in addition to a Module 
            case e => throw ImplementationError("non-container element: " + e) // impossible
         }
      }
      
      /** parse the top element, which must be an UnparsedContainer, and replace it with a ParsedContainer */
      private def parseHead {
         // pop the top element and obtain the node read so far
         val uc @ UnparsedContainer(_) = openTags.head
         openTags = openTags.tail
         val node = uc.toNode
         // below, catchSE will push the ParsedContainer that replaces uc
         openTags match {
            case Nil =>
               // special treatment of the toplevel container 
               streamer.log("parsing shape of top element")
               val dpath = parentInfo match {
                  case IsRootDoc(dp) => dp
                  case _ => throw ParseError("parsing of non-root-documents unsupported")
               }
               reader.readDocument(dpath, node)(catchSE)
               val ParsedContainer(doc: Document) = openTags.head
               root = doc
            case ParsedContainer(parent) :: _ => 
               streamer.log("parsing shape of " + node.label + " in " + parent.path)
               reader.readIn(root.getNamespaceMap, parent, node)(catchSE)
            case hd :: _ => throw ImplementationError("illegal head: " + hd) // impossible
         }
      }
      
      // called when an opening tag is encountered
      // invariant: possibly parse the top element; push exactly one element onto the stack
      override def elemStart(pos: Int, pre: String, label: String, attrs: MetaData, scope: NamespaceBinding) {
         // construct empty element
         lazy val elem = Elem(pre, label, attrs, scope, true)
         // based on the label, we remember what to do when we encounter their children or closing tag
         if (containers contains label) {
               // for containers, we collect the opening tag and the shape-relevant components
             streamer.log("streaming in " + label)
             openTags match {
                case (_: UnparsedContainer) :: _ =>
                   // elem is the first non-shape-relevant child of uc: so parse uc first
                   parseHead
                case _ =>
             }
             openTags ::= UnparsedContainer(elem)
         } else {
            openTags.head match {
               case uc: UnparsedContainer =>
                  // shape-relevant children of containers are collected
                  if (shapeRelevant contains label) {
                     streamer.log("found shape relevant child: " + label)
                     openTags ::= AppendUnparsed(uc)
                  } else {
                     // the first non-shape-relevant child of a container: parse the container
                     streamer.log("found other child: " + label)
                     parseHead
                     val pc @ ParsedContainer(_) = openTags.head
                     openTags ::= AppendParsed(pc)
                  }
               case pc: ParsedContainer =>
                  openTags ::= AppendParsed(pc)
               case _ =>
                  // all other tags are irrelevant for streaming
                  openTags ::= Other
            }
         }
      }

      // called when a closing tag is encountered
      // the return value of this method replaces the node
      override def elem(pos: Int, pre: String, label: String, attrs: MetaData,
                        scope: NamespaceBinding, empty: Boolean, nodes: NodeSeq) = {
         // the element that has been read
         lazy val n = Elem(pre, label, attrs, scope, empty, nodes:_*)
         // pop the instruction and execute it
         val instruction = openTags.head
         instruction match {
            case AppendUnparsed(uc) =>
               // shape-relevant child of a container: add to the unparsed container
               streamer.log("done reading shape-relevant child " + label)
               uc.children ::= n
            case AppendParsed(pc) =>
               // non-shape-relevant child of a container: parse and add it to the container
               streamer.log("streaming " + label + " in " + pc.container.path)
               reader.readIn(root.getNamespaceMap, pc.container, n)(add)
            case uc: UnparsedContainer =>
               // unparsed container: parse it (this only happens if a container is empty)
               parseHead
               streamer.log("done streaming in " + label)
            case pc: ParsedContainer =>
               // parsed container: the container and its content have been streamed, nothing to do
               streamer.log("done streaming in " + pc.container.path)
            case Other =>
               // any other element: no special treatment
         }
         openTags = openTags.tail
         // check if we have to return a node
         instruction match {
            case Other =>
               // bubble up the inner node (same behavior as the super method)
               n
            case _ =>
               // the node is handled and can be dropped
               if (openTags.isEmpty) {
                 // at the toplevel, we have to make up a node
                 <dummy/>
               } else {
                  NodeSeq.Empty
               }
         }
      }
   }
}