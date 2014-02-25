package info.kwarc.mmt.api.archives.latex

import info.kwarc.mmt.api._
import modules._
import symbols._
import objects._
import parser._
import archives._
import documents._
import objects.Conversions._

import LatexExporter._

/** an unfinished exporter that generates latex presentations of modules */
class LatexExporter extends Exporter {
   val key = "mmt-latex"
   val outDim = Dim("export", "latex")
   override val outExt = "tex"
   override protected val folderName = "NAMESPACE"
      
   private def doConstantName(p: GlobalName) = p match {
      case GlobalName(OMMOD(m), !(n)) => "\\" + translate(m.last + n, commandEscapes ::: UnicodeConverter.mapsRaw)
      case _ =>
         logError("cannot translate to constant name: " + p)
         s"\\ERROR($p)" 
   }
   private def doDelim(d: Delimiter) =
      if (d.text == "%w" || d.text == " ") "\\;" else translate(d.text, delimEscapes ::: UnicodeConverter.maps)
   /** convert a term into a LaTeX expression (using lots of \ { and }) */
   private def doTerm(t: Term): String = controller.pragmatic.mostPragmatic(t) match {
      case OMS(p) => doConstantName(p)
      case OMV(n) => n.toPath
      case ComplexTerm(p, subs, con, args) =>
         var res = doConstantName(p)
         //TODO group arguments and variables into ,-separated sequences
         subs.foreach {case Sub(l,t) => res += "{\\mmtlabel{" + l.toPath + "}{" + doTerm(t) + "}"}
         con.variables.foreach {case VarDecl(n, tp, df) =>
            val nL = n.toPath
            val tpL = tp match {
               case None => "{}"
               case Some(t) => "{" + doTerm(t) + "}"
            }
            val dfL = df match {
               case None => ""
               case Some(t) => "[" + doTerm(t) + "]"
            }
            res += s"{\\mmtvar$dfL{$nL}$tpL" + "}"
         }
         args.foreach {s => res += "{" + doTerm(s) + "}"}
         res
      case l: OMLITTrait => l.toString
      case t => logError("unexportable: " + t); "ERROR"
   }
   /** convert notation markers into the body of a \newcommand */
   private def doMarkers(ms: List[Marker]): String = {
      ms.map {
         case s: SeqArg => "\\mmtfold{" + doDelim(s.sep) + "}{" + "#" + s.number.abs + "}"
         case a: ArgumentMarker => "#" + a.number.abs
         case d: Delimiter => doDelim(d)
         case Var(n, _, Some(sep)) => "\\mmtfold{" + doDelim(sep) + "}{" + "#" + n + "}"
         case Var(n, _, None) => "#" + n
         case AttributedObject => "#1" //probably wrong
         case GroupMarker(elements) => "{" + doMarkers(elements) + "}"
         case ScriptMarker(main, sup, sub, over, under) =>
            var res = doM(main)
            over.foreach {s => res = s"\\overset{${doM(s)}}{$res}"}
            under.foreach {s => res = s"\\underset{${doM(s)}}{$res}"}
            sup.foreach {s => res = s"{$res}^{${doM(s)}}"}
            sub.foreach {s => res = s"{$res}_{${doM(s)}}"}
            res
         case FractionMarker(above, below, line) =>
            val aboveL = doMarkers(above)
            val belowL = doMarkers(below)
            val command = if (line) "frac" else "Frac"
            s"\\$command{$aboveL}{$belowL}"
         case InferenceMarker => "ERROR(INFERENCE)"
      }.mkString("")
   }
   private def doM(m: Marker): String = doMarkers(List(m))
   private def requirePackage(p: MPath, bf: BuildFile): String = {
      controller.backend.findOwningArchive(p) match {
         case None => "% skipping import of unknown module " + p.toPath
         case Some(a) =>
            val conP = Archive.MMTPathToContentPath(p)
            val sty = (a/outDim/conP).removeExtension
            val relSty = utils.FileURI(bf.outFile).relativize(utils.FileURI(sty)).pathAsString
            s"\\RequirePackage{\\currfiledir $relSty}"
      }
   }
   def exportTheory(t: DeclaredTheory, bf: BuildFile) {
      val styFile = utils.File.Writer(bf.outFile.setExtension("sty"))
      def sty(s: String) = styFile.println(s)
      def tex(s: String) = rh.writeln(s)
      sty("\\RequirePackage{mmt-latex}")
      tex("\\documentclass{article}")
      val name = bf.outFile.removeExtension.segments.last
      tex(s"\\usepackage{$name}")
      tex("\\begin{document}")
      val mtL = t.meta match {
         case None => ""
         case Some(mt) =>
            sty(requirePackage(mt, bf))
            "[" + mt.last + "]" 
      }
      tex(s"\\begin{mmttheory}$mtL{${t.name.toPath}}")
      t.getDeclarations.foreach {
         case c: Constant =>
            val dfL = c.df match {
               case None => ""
               case Some(t) => "[{" + doTerm(t) + "}]"
            }
            val tpL = c.tp match {
               case None => ""
               case Some(t) => doTerm(t)
            }
            tex(s"\\mmtdecl$dfL{\\mathit{${doDelim(Delim(c.name.toPath))}}}{$tpL} \\\\")
            val numArgs = c.notC.oneDim match {
               case None => ""
               case Some(tn) => "[" + tn.arity.length + "]"
            }
            val notBody = c.notC.getPresent match {
               case None => translate(c.name.toString, delimEscapes ::: UnicodeConverter.maps)
               case Some(tn) =>
                  doMarkers(tn.presentationMarkers)
            }
            sty(s"\\newcommand{${doConstantName(c.path)}}$numArgs{$notBody}")
         case PlainInclude(from, _) =>
            tex(s"\\mmtinclude{${from.last}}")
            sty(requirePackage(from, bf))
         case d =>
            tex("% skipping declaration of " + d.name)
      }
      tex("\\end{mmttheory}")
      tex("\\end{document}")
      styFile.close
   }
   def exportView(v: DeclaredView, bf: BuildFile) {
      rh("%% view omitted")
   }
   def exportNamespace(dpath: DPath, bd: BuildDir, namespaces: List[(BuildDir,DPath)], modules: List[(BuildFile,MPath)]) {
      rh("%% namespace omitted")
   }
   def exportDocument(doc : Document, bf:BuildTask) {}
   
}

object LatexExporter {
   /** translate each character in s according to by */
   def translate(s: String, by: List[(Char, String)]) = s.map {c =>
      by.find(_._1 == c) match {
         case None => c.toString
         case Some((_,cE)) => cE
      }
   }.mkString("") 
   /** escapes of characters within latex commands */
   private def commandEscapes = "\\_-1234567890".toList zip List("B", "U", "M", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Zero")
   /** escapes of characters within latex delimiters */
   private def delimEscapes = '\\' -> "\\backslash " :: '~' -> "\\sim " :: "$#%&{}_^".toList.map(c => (c, "\\" + c))
}