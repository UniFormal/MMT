package info.kwarc.mmt.latex

import info.kwarc.mmt.api._
import utils._
import frontend._
import symbols._
import modules._
import documents._
import objects._
import parser._
import presentation._

object MMTTeX {
  case class ObjectID(sec: Int, subsec: Int, index: Int) {
    def toName = s"${ObjectID.prefix}$sec.$subsec.$index"
    def toLatex = "\\csname " + toName + "\\endcsname"
  }
  object ObjectID {
    val prefix = "mmt@"
    def unapply(s: String): Option[ObjectID] = {
       if (s.startsWith(prefix)) {
         val parts = stringToList(s.substring(prefix.length), "\\.")
         parts match {
           case List(NumericString(sec),NumericString(subsec),NumericString(ind)) =>
              Some(ObjectID(sec,subsec,ind))
           case _ => None
         }
       } else
          None
    }
  }
}

import MMTTeX._

/**
 */
class MMTTeX extends ShellExtension("mmttex") {
   def helpText: String = ""
   
   def run(shell: Shell, args: List[String]) = {
     val tex = File(args(0))
     val base = DPath(FileURI(tex))
     val mmt = tex.addExtension("mmt")
     val sty = mmt.addExtension("sty")
     val eh = new ErrorContainer(None)
     val ps = ParsingStream.fromFile(mmt, dpathOpt = Some(base), formatOpt = Some("mmt"))
     val doc = controller.read(ps, true)(eh)

     val writer = new StyWriter(sty, eh)
     writer.doDocument(doc)
     true
   }

   /** scope that holds all global arguments needed while writing the sty file
    *  created once per file
    */
   private class StyWriter(sty: File, eh: ErrorHandler) {
     private val jobFile = new FileWriter(sty)
     private var modFiles: List[String] = Nil
     private val pres = new MacroBasedPresenter
     
     def doDocument(doc: Document) {
       doc.getModulesResolved(controller.globalLookup) foreach doModule
     }
     
     private def doModule(m: Module) {
        controller.simplifier(m)
        val modFileName = m.name.toPath
        modFiles ::= modFileName
        val modFile = new FileWriter((sty.up / modFileName).addExtension("sty"))
        m.getDeclarations.foreach {
          case c: Constant =>
             val name = c.name.toPath
             name match {
               case ObjectID(id) =>
                 c.df match {
                   case Some(df) =>
                     val dfL = pres.asString(df, Some(c.path $ DefComponent))
                     jobFile << s"\\expandafter\\def${id.toLatex}{$dfL}\n"
                   case None => // malformed mmt file: generate error/warning
                 }
               case _ =>
                 pres(c)(jobFile)
             }
          case nm: NestedModule =>
             doModule(nm.module)
          case _ =>
            // generate \RequirePackage for every include
            // we skip includes etc for now
        }
     }
   }
}

object MacroUsingObjectPresenter extends ObjectPresenter {
  def apply(o: Obj, origin: Option[CPath])(implicit rh : RenderingHandler) {
    rh << doObj(o)
  }

  private def doConstantName(p: GlobalName) = p match {
    case GlobalName(m, !(n)) => "\\" + LatexExporter.translate(m.last + n, LatexExporter.commandEscapes ::: UnicodeConverter.mapsRaw)
    case _ =>
      logError("cannot translate to constant name: " + p)
      s"\\ERROR($p)"
  }

  /** convert a term into a LaTeX expression (using lots of \ { and }) */
  private def doObj(obj: Obj): String = obj match {
    case OMS(p) => doConstantName(p)
    case OMV(n) => n.toPath
    case ComplexTerm(p, subs, con, args) =>
      var res = doConstantName(p)
      //TODO group arguments and variables into ,-separated sequences
      subs.foreach { case Sub(l, t) => res += "{\\mmtlabel{" + l.toPath + "}{" + doObj(t) + "}" }
      con.variables.foreach {case VarDecl(n, _, tp, df, _) =>
        val nL = n.toPath
        val tpL = tp match {
          case None => "{}"
          case Some(t) => "{" + doObj(t) + "}" 
        }
        val dfL = df match {
          case None =>
          case Some(t) => "[" + doObj(t) + "]"
        }
        res += s"{\\mmtvar$dfL{$nL}$tpL" + "}"
      }
      args.foreach { s => res += "{" + doObj(s) + "}" }
      res
    case l: OMLITTrait => l.toString
    case t =>
      logError("unexportable: " + t)
      "ERROR"
  }
}

/** produces content of sty file for every theory */
class MacroBasedPresenter extends Presenter(MacroUsingObjectPresenter) {
  def key = "mmt-latex"
  
  def apply(e : StructuralElement, standalone: Boolean = false)(implicit rh : RenderingHandler) {
    e match {
      case t: DeclaredTheory => doTheory(t)
      case d: Declaration => doDeclaration(d)
      case _ =>
    }
  }

  private def doTheory(dt: DeclaredTheory)(implicit rh : RenderingHandler) {
    controller.simplifier(dt)
    dt.getDeclarationsElaborated.foreach {
      case d => doDeclaration(d)
    }
  }
  
  private def doDeclaration(d: Declaration)(implicit rh : RenderingHandler) {}
  /*
    d match {
      case c: Constant =>
        val numArgs = c.notC.parsing match {
          case None => ""
          case Some(tn) => "[" + tn.arity.length + "]"
        }
        val notBody = c.notC.getPresentDefault match {
          case None => translate(c.name.toString, delimEscapes ::: UnicodeConverter.maps)
          case Some(tn) =>
            doMarkers(tn.presentationMarkers)
        }
        rh << s"\\newcommand{${doConstantName(c.path)}}$numArgs{$notBody}"
      case _ => // generate RequirePackage for every include 
    }
  }
  
  private def requirePackage(p: MPath, bf: BuildTask): String = {
    controller.backend.findOwningArchive(p) match {
      case None => "% skipping import of unknown module " + p.toPath
      case Some(a) =>
        val conP = Archive.MMTPathToContentPath(p)
        val sty = (a / outDim / conP).stripExtension
        val relSty = utils.FileURI(bf.outFile).relativize(utils.FileURI(sty)).pathAsString
        s"\\RequirePackage{\\currfiledir $relSty}"
    }
  }

  /** convert notation markers into the body of a \newcommand */
  private def doMarkers(ms: List[Marker]): String = {
    ms.map {
      case s: SeqArg => "\\mmtfold{" + doDelim(s.sep) + "}{" + "#" + s.number.abs + "}"
      case a: ArgumentMarker => "#" + a.number.abs
      case d: Delimiter => doDelim(d)
      case Var(n, _, Some(sep), _) => "\\mmtfold{" + doDelim(sep) + "}{" + "#" + n + "}"
      case Var(n, _, None, _) => "#" + n
      case AttributedObject => "#1" //probably wrong
      case GroupMarker(elements) => "{" + doMarkers(elements) + "}"
      case ScriptMarker(main, sup, sub, over, under) =>
        var res = doM(main)
        over.foreach { s => res = s"\\overset{${doM(s)}}{$res}" }
        under.foreach { s => res = s"\\underset{${doM(s)}}{$res}" }
        sup.foreach { s => res = s"{$res}^{${doM(s)}}" }
        sub.foreach { s => res = s"{$res}_{${doM(s)}}" }
        res
      case FractionMarker(above, below, line) =>
        val aboveL = doMarkers(above)
        val belowL = doMarkers(below)
        val command = if (line) "frac" else "Frac"
        s"\\$command{$aboveL}{$belowL}"
      case InferenceMarker => "ERROR(INFERENCE)"
      case m =>
        //noinspection SideEffectsInMonadicTransformation
        logError("unexpected notation marker: " + m)
        "ERROR"
    }.mkString("")
  }

  private def doM(m: Marker): String = doMarkers(List(m))

  private def requirePackage(p: MPath, bf: BuildTask): String = {
    controller.backend.findOwningArchive(p) match {
      case None => "% skipping import of unknown module " + p.toPath
      case Some(a) =>
        val conP = Archive.MMTPathToContentPath(p)
        val sty = (a / outDim / conP).stripExtension
        val relSty = utils.FileURI(bf.outFile).relativize(utils.FileURI(sty)).pathAsString
        s"\\RequirePackage{\\currfiledir $relSty}"
    }
  }*/
}
