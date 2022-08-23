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
      private def readOne: Unit = {
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
 * The parser for OMDoc XML.
 * 
 * This class is in charge of streaming XML. To do so, it wraps around [[XMLReader]], which does the actual parsing of XML elements into MMT data structures.
 */
class XMLStreamer extends Parser(XMLObjectParser) {streamer =>
   val format = "omdoc"
   override val logPrefix = "streamer"
   private lazy val reader = new XMLReader(controller)

   /** the XML labels of [[ContainerElement]]s; their children are streamed */
   private val containers = List("omdoc", "theory", "view", "import", "derived")
   /**
    * Container elements may have components that are serialized as XML children.
    * These are called shape-relevant and must occur at the beginning of the XML node.
    * They must be parsed before the container element can be created and are not streamed.
    * This holds the XML labels of the shape-relevant XML nodes.
    */
   private val shapeRelevant = List("metadata", "parameters", "type", "definition", "from", "to")

   def apply(ps: parser.ParsingStream)(implicit cont: StructureParserContinuations): StructuralElement = {
      val errorCont = cont.errorCont
      val parser = new ConsParser(ps.parentInfo, new SourceFromReader(ps.stream), cont)
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

   /** openTags below holds a Stack of one element per open XML node */
   private object Stack {
      abstract class StackElement
      /** a container element, whose shape-relevant children have not been found yet */
      case class UnparsedContainer(elem: Elem) extends StackElement {
         var children: List[Elem] = Nil
         def toNode = elem.copy(child = children.reverse)
      }
      /** a container element, whose shape-relevant children have been found yet */
      case class ParsedContainer(container: ContainerElement[_ <: StructuralElement]) extends StackElement
      /** a shape-relevant child of a container element */
      case class AppendUnparsed(container: UnparsedContainer) extends StackElement
      /** a non-shape-relevant child of a container element */
      case class AppendParsed(container: ParsedContainer) extends StackElement
      /** other XML nodes (e.g., object level nodes) */
      case object Other extends StackElement
   }
   import Stack._

   /** XML parser that streams documents/modules and calls the reader on the other ones */
   private class ConsParser(parentInfo: ParentInfo, input: Source, cont: StructureParserContinuations) extends ConstructingParser(input, true) {
      /** the stack of currently open tags, innermost first */
      private var openTags : List[StackElement] = Nil
      /** holds the root element once parsing has finished */
      var root: Document = null
      
      /** add to controller and call the passed continuations */
      private class addAndCont(streamed: Boolean) extends StructureParserContinuations(cont.errorCont) {
        override def onElement(se: StructuralElement): Unit = {
           try {
              controller.add(se)
              cont.onElement(se)
           } catch {case e: AddError =>
              // errors in the XML are usually minor and we can assume we can recover
              cont.errorCont << e
           
           }
           // if we're streaming, additionally push the parsed container element to the stack
           if (streamed) {
             se match {
                case ce: ContainerElement[_] => openTags ::= ParsedContainer(ce)
                case nm: NestedModule => openTags ::= ParsedContainer(nm.module)
                case _ =>
             }
           }
        }
        override def onElementEnd(se: ContainerElement[_]): Unit = {
          // if we're streaming, we'll call this manually later
          if (!streamed) {
            controller.endAdd(se)
            cont.onElementEnd(se)
          }
        }
      }
      
      private val streamedCont = new addAndCont(true)
      private val notStreamedCont = new addAndCont(false)

      /** parse the top element, which must be an UnparsedContainer
       *  streamedCont will replace it with a ParsedContainer without calling end-of-element continuations
       */
      private def parseHead: Unit = {
         // pop the top element and obtain the node read so far
         val uc @ UnparsedContainer(_) = openTags.head
         openTags = openTags.tail
         val node = uc.toNode
         // read node relative to its parent
         openTags match {
            case Nil =>
               // special treatment of the toplevel container
               streamer.log("parsing shape of top element")
               val dpath = parentInfo match {
                  case IsRootDoc(dp) => dp
                  case _ => throw ParseError("parsing of non-root-documents unsupported")
               }
               reader.readDocument(dpath, node)(streamedCont)
               val ParsedContainer(doc: Document) = openTags.head
               root = doc
            case ParsedContainer(parent) :: _ =>
               streamer.log("parsing shape of " + node.label + " in " + parent.path)
               reader.readIn(root.getNamespaceMap, parent, node)(streamedCont)
            case hd :: _ => throw ImplementationError("illegal head: " + hd) // impossible
         }
      }

      // called when an opening tag is encountered
      // invariant: possibly parse the top element; push exactly one element onto the stack
      override def elemStart(pos: Int, pre: String, label: String, attrs: MetaData, scope: NamespaceBinding): Unit = {
         // if this is the first non-shape-relevant child of an unparsed container, parse the container  
         openTags.headOption match {
           case Some(uc: UnparsedContainer) if !(shapeRelevant contains label) =>
             parseHead
           case _ =>
         }
         if (containers contains label) {
             // push UnparsedContainer with empty XML node
             streamer.log("streaming in " + label)
             val elem = Elem(pre, label, attrs, scope, true)
             openTags ::= UnparsedContainer(elem)
         } else {
            // push AppendParsed or AppendUnparsed if the top of the stack is a container, Other otherwise
            openTags.head match {
               case uc: UnparsedContainer =>
                  if (shapeRelevant contains label) {
                     streamer.log("found shape relevant child: " + label)
                     openTags ::= AppendUnparsed(uc)
                  } else {
                    // impossible because parseHead was called
                    throw ImplementationError("non-shape relevant child of unparsed container")
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
         // if a container element was empty (i.e. had no non-shape-relevant children), we still have to parse it
         openTags.head match {
            case uc: UnparsedContainer =>
               parseHead
            case _ =>
         }
         // pop the instruction and execute it
         val instruction = openTags.head
         openTags = openTags.tail
         val bubbleUpNode = instruction match {
            case AppendUnparsed(uc) =>
               // shape-relevant child of a container: add to the unparsed container
               streamer.log("done reading shape-relevant child " + label)
               uc.children ::= n
               None
            case AppendParsed(pc) =>
               // non-shape-relevant non-container child of a container: parse and add it to the container
               streamer.log("streaming " + label + " in " + pc.container.path)
               reader.readIn(root.getNamespaceMap, pc.container, n)(notStreamedCont)
               None
            case pc: ParsedContainer =>
               // end of a container element: body has already been streamed
               // but we have to call the continuation that was delayed in streamedCont 
               notStreamedCont.onElementEnd(pc.container)
               streamer.log("done streaming in " + pc.container.path)
               None
            case _: UnparsedContainer =>
               // impossible because parseHead was called
               throw ImplementationError("end of unparsed container")
            case Other =>
               // any other node is bubbled up
               Some(n)
         }
         bubbleUpNode.getOrElse {
           if (openTags.isEmpty) {
             // at the toplevel, we have to make up a node
             <dummy/>
           } else {
             // otherwise, we just skip it
             NodeSeq.Empty
           }
         }
      }
   }
}
