package info.kwarc.mmt.pvs

import syntax._
import info.kwarc.mmt.api._
import documents._
import modules._
import utils._
import archives._
import info.kwarc.mmt.LFX.Subtyping.subtypeOf
import info.kwarc.mmt.api.objects.{Context, OMID}
import info.kwarc.mmt.api.parser.{KeywordBasedParser, ParserExtension, ParserState, SourceRef}
import info.kwarc.mmt.api.symbols.{BoundTheoryParameters, DerivedDeclaration, FinalConstant, TermContainer}
import info.kwarc.mmt.lf.{Apply, Lambda, Pi}

class Plugin extends frontend.Plugin {
  val theory = PVSTheory.thpath
  val dependencies = List("info.kwarc.mmt.lf.Plugin")
  override def start(args: List[String]) {
    val em = controller.extman
    // content enhancers
    em.addExtension(new LambdaPiInclude)
    em.addExtension(new ParIncludeParserExt)
    em.addExtension(new PVSImporter)
  }
}

class ParIncludeParserExt extends ParserExtension {

  def isApplicable(se: StructuralElement, keyword: String): Boolean = se match {
    case th:DeclaredTheory => keyword == "LambdaPiInclude"
    case _ => false
  }

  def apply(sp: KeywordBasedParser, s: ParserState, se: StructuralElement, keyword: String,con:Context) = se match {
    case th:DeclaredTheory if keyword == "LambdaPiInclude" =>
      val path = sp.readMPath(th.path)(s)._2
      controller.add(BoundInclude(th,path))
    case _ => s.errorCont(SourceError("SubTypeParserExt", SourceRef(s.ps.source, s.startPosition.toRegion),
      "not applicable to StructuralElement "+se.getClass.toString))
  }
}

class LambdaPiInclude extends BoundTheoryParameters(Pi.path,Lambda.path,Apply.path)
case class BoundInclude(top:DeclaredTheory,from:MPath) extends DerivedDeclaration(top.toTerm,LocalName(from),"BoundParams",
  List(DeclarationComponent(DomComponent,TermContainer(OMID(from)))))

class PVSImporter extends Importer {
   val key = "pvs-omdoc"
   def inExts = List("xml")
   //override def inDim = RedirectableDimension("pvsxml", Some(Dim("src","pvsxml")))

   private val parseXML = syntax.makeParser

   private var startAt = "/home/raupi/lmh/MathHub/PVS/NASA/source/vectors/pvsxml/vectors_3D_def"
//   private var startAt = "/home/raupi/lmh/MathHub/PVS/Prelude/src/pvsxml/stdtokenizer"
   def importDocument(bf: BuildTask, index: Document => Unit): BuildResult = {
//      if (bf.inFile.toFilePath.toString < startAt) return BuildResult.empty
      val d = bf.inFile.name
      val e = try {
         parseXML(bf.inFile)
      } catch {
         case utils.ExtractError(msg) =>
            //ParseError("error in xml: " + msg)
/*            if (ignoreMsg.exists(msg.startsWith))
               return
            i += 1
            if (i > ignore) { */
               println(msg)
               sys.exit
               //throw utils.ExtractError(msg)
//            } else
  //             return
      }
     //println(e)

      val conv = new PVSImportTask(controller, bf, index)
      e match {
         case d: pvs_file =>
            conv.doDocument(d)
         case m: syntax.Module =>
            conv.doDocument(pvs_file(List(m)))
            //conv.doModule(m)
      }
      // BuildResult.empty
   }
}
