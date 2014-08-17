package info.kwarc.mmt.api.backend

import info.kwarc.mmt.api._
import modules._

import scala.xml._
import parsing._
import scala.io.Source

/** 
 *  first attempt at a streaming XML reader
 *  
 *  does not work yet and it's probably better to refactor the old parser to make it stream-ready first
 */
abstract class StreamXML(input: Source) extends ConstructingParser(input, false) {
   private val reader = new XMLReader(???)

   private var openTags : List[StructuralElement] = Nil
   private def labelsInDoc = List("theory", "view")
   
   private implicit def catchSE(se: StructuralElement) {openTags ::= se}
   override def elemStart(pos: Int, pre: String, label: String, attrs: MetaData, scope: NamespaceBinding) {
      lazy val elem = Elem(pre, label, attrs, scope, true)
      label match {
         case "omdoc" =>
            reader.readDocument(???, elem)
         case l if (labelsInDoc contains l) && openTags.head.isInstanceOf[documents.Document] =>
            reader.readInDocument(???, ???, elem)
         case _ =>
            openTags ::= null
      }
   }
   override def elemEnd(pos: Int, pre: String, label: String) {
      openTags = openTags.tail
   }
   
   override def elem(pos: Int, pre: String, label: String, attrs: MetaData,
                     scope: NamespaceBinding, empty: Boolean, nodes: NodeSeq) = {
      val n = Elem(pre, label, attrs, scope, empty, nodes:_*)
      if (openTags.head == null)
         n
      else {
         openTags.head match {
            case d: documents.Document =>
               reader.readInDocument(???,???, n)
            case m: DeclaredModule =>
               reader.readInTheory(???, ???, n)
         }
         NodeSeq.Empty
      }
   }
}