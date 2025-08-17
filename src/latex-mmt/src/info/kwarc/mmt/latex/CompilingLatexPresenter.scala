package info.kwarc.mmt.latex

import info.kwarc.mmt.api._
import utils._
import symbols._
import modules._
import documents._
import info.kwarc.mmt.api.objects.Obj.{getConstants, parseTerm}
import info.kwarc.mmt.api.opaque.{ObjFragment, OpaqueText, ScopeFragment, StringFragment, TextFragment}
import objects._
import presentation._
import notations._
import parser._

import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import scala.sys.process.Process


/** helper functions for latex/stex presenter */
object Common {
  def doConstantName(p: GlobalName) = {
    val mod = doLocalName(p.module.name)
    val name = doLocalName(p.name)
    "\\" + mod + "@@" + name
  }
  private def doLocalName(l: LocalName): String = l.steps.map {
    case SimpleStep(n) => Common.translateName(n)
    case ComplexStep(p) => doLocalName(p.name)
  }.mkString("@")

  def doSymbolName(p: GlobalName) = {
    "\\" + "MMT" + Common.translateName(escapeToCamelCase(p.name.dropComplex.toString))
  }

  def escapeToCamelCase(name: String): String = {
    val nameParts = name.split("[_/]")
    nameParts.head + nameParts.tail.map(_.capitalize).mkString
  }

  /** translate each character in s according to by */
  private def translate(s: String, by: Char => Option[String]): String = s.map {c => by(c).getOrElse(c.toString)}.mkString("")
  
  /** translates a unicode-containing string into the latex equivalent */
  def translateDelim(s: String) = translate(s, c => utils.listmap(delimEscapes,c) orElse macroUnicodeMap(c))
  
  /** translates a string making up an MMT name into a latex name */
  def translateName(s: String) = translate(s, c => utils.listmap(commandEscapes, c) orElse rawUnicodeMap.get(c))

  /** translates a unicode-containing string into a LaTeX-acceptable version (used for translating OpaqueText) */
  def translateOpaqueStringFragment(s: String): String = translate(s, c => if (allowedOpaqueChars.contains(c.toString)) Option(c.toString) else macroUnicodeMapForOpaqueStringFragment(c))

  /** escapes of characters within latex commands */
  private val commandEscapes = "\\_-1234567890".toList zip List("B", "U", "M", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Zero")
  /** escapes of characters within latex delimiters */
  private val delimEscapes = '\\' -> "\\backslash " :: '~' -> "\\sim " :: ' ' -> "\\;" :: "$#%&{}_^".toList.map(c => (c, "\\" + c))
  /** Elements in OpaqueText that should NOT be translated */
  private val allowedOpaqueChars = List("{", "}", "[", "]", "(", ")")
  /** maintains a map of unicode-latex replacements, populated on initialization from the resource as jEdit abbreviations
   * later maps overwrite earlier ones; so make sure the resource always contain the non-standard commands (e.g., \ra) before their latex analogs
   */
  private val rawUnicodeMap = new scala.collection.mutable.HashMap[Char,String]()
  private def macroUnicodeMap(c: Char): Option[String] = rawUnicodeMap.get(c).map(x => "\\" + x + " ")
  private def macroUnicodeMapForOpaqueStringFragment(c: Char): Option[String] = rawUnicodeMap.get(c).map(x => "$\\" + x + "$ ")
  private val inverseUnicodeMap = new scala.collection.mutable.HashMap[String,String]
  /** this is not used by default; users can add it as a rule if they have difficulties typing Unicode characters in LaTeX */
  object LatexToUnicodeConverter extends LexerExtension {
    def apply(s: String, i: Int, firstPosition: SourcePosition): Option[Token] = {
      if (s(i) != '\\') return None
      var j = i+1
      val l = s.length
      while (j < l && s(j).isLetter) {j+=1}
      val commandName = s.substring(i+1,j)
      val command = "\\" + commandName
      val wsBefore = i == 0 || TokenList.isWhitespace(s(i-1))
      val t = inverseUnicodeMap.get(commandName) match {
        case Some(unicode) =>
          Token(unicode, firstPosition, wsBefore, Some(command))
        case None =>
          Token(command, firstPosition, wsBefore)
      }
      Some(t)
    }
    def trigger = Some("\\")
  }
  private def init: Unit = {
    val basicmap = UnicodeMap.readMap("unicode/unicode-latex-map")
    fillMap(basicmap)
  }
  init

  private def fillMap(map: List[(String,String)]): Unit = {
    map.foreach {case (c,r) =>
      // command starts with j
      val command = c.substring(1)
      if (r.length == 1) {
        val char = r(0)
        rawUnicodeMap(char) = command
        inverseUnicodeMap(command) = r
      } else {
        // better to ignore this because they can be useful in other contexts
        // throw GeneralError(s"expected single character in symbol map: $c|$r")
      }
    }
  }
}

import Common._

/**
 * compiles MMT to LaTeX by producing
 * * a sty-file for each theory
 * * containing a macro definition for each constant
 *
 * type and definiens are ignored, only notations are used
 *
 * This is 'compiling' in the sense that MMT/X is turned into LaTeX/X.
 * It is to be distinguished from visualizing presenters that turn MMT/X into LaTeX/MMT/X, e.g., by generating LaTeX that represent MMT source code.
 * A preliminary such presenter exists in commented-out code.
 */
class MacroGeneratingPresenter extends Presenter(new MacroUsingPresenter) {
  def key = "mmt-sty"

  override val outExt = "sty"

  def apply(e : StructuralElement, standalone: Boolean = false)(implicit rh : RenderingHandler): Unit = {
    e match {
      case t: Theory => doTheory(t)
      case d: Declaration => doDeclaration(d)
      case _ =>
    }
  }

  private def doTheory(dt: Theory)(implicit rh : RenderingHandler): Unit = {
    controller.simplifier(dt)
    dt.meta foreach {m =>
      doDeclaration(PlainInclude(m, dt.path))
    }
    dt.getDeclarationsElaborated.foreach {
      case d => doDeclaration(d)
    }
  }

  private def doDeclaration(d: Declaration)(implicit rh : RenderingHandler): Unit = {
    d match {
      case c: Constant => doConstant(c)
      case i @ Include(id) =>
        // skip redundant includes
        if (i.isGenerated)
          return
        // ignore includes of theories written in scala
        if (id.from.doc.uri.scheme contains "scala")
          return
        val arch = controller.backend.findOwningArchive(id.from).getOrElse {
          logError("cannot find archive holding " + id.from + " (ignoring)")
          return
        }
        val pLAbs = arch / outDim / "content" / archives.Archive.MMTPathToContentPath(id.from.mainModule).stripExtension
        val pLRel = arch.root.up.up.relativize(pLAbs)
        val pL = pLRel.segments.mkString("/")
        rh << "\\RequireMMTPackage{" + pL + "}\n"
      case _ =>
    }
  }

  private def doConstant(c: Constant)(implicit rh : RenderingHandler): Unit = {
    val not = c.notC.getPresentDefault
    var numArgs = not match {
      case None => 0
      case Some(tn) =>
        tn.arity.length
    }
    val notBody = not match {
      case None => Common.translateDelim(c.name.toString)
      case Some(tn) =>
        // if an InferenceMarker occurs, the generated macro expects an additional argument for the inferred type
        val dm = new DoMarkers(c.path, numArgs+1)
        val tnS = dm.doMarkers(tn.presentationMarkers)
        if (dm.infMarkerUsed)
          numArgs += 1
        tnS
    }
    val numArgsS = if (numArgs == 0) "" else "[" + numArgs.toString + "]"
    rh << s"\\newcommand{${doConstantName(c.path)}}$numArgsS{$notBody}\n"
  }

  /** helper class to encapsulate the state of doMarkers */
  private class DoMarkers(p: GlobalName, infMarkerPos: Int) {
    var infMarkerUsed: Boolean = false
    /** convert notation markers into the body of a \newcommand */
    def doMarkers(ms: List[Marker]): String = {
      val msS = ms.map {
        case a: Arg => "#" + a.number.abs
        case a: ImplicitArg => s"\\mmt@implicit{#${a.number.abs}}"
        case s: SeqArg => "\\mmt@fold{" + doDelim(s.sep) + "}{" + "#" + s.number.abs + "}"
        case d: Delimiter => doDelim(d)
        case Var(n, _, Some(sep), _) => "\\mmt@fold{" + doDelim(sep) + "}{" + "#" + n + "}"
        case Var(n, _, None, _) => "#" + n
        case GroupMarker(elements) => "{" + doMarkers(elements) + "}"
        case ScriptMarker(main, sup, sub, over, under) =>
          var res = doM(main)
          over.foreach  {s => res = s"\\overset{${doM(s)}}{$res}"}
          under.foreach {s => res = s"\\underset{${doM(s)}}{$res}"}
          sup.foreach   {s => res = s"{$res}^{${doM(s)}}"}
          sub.foreach   {s => res = s"{$res}_{${doM(s)}}"}
          res
        case FractionMarker(above, below, line) =>
          val aboveL = doMarkers(above)
          val belowL = doMarkers(below)
          val command = if (line) "frac" else "Frac"
          s"\\$command{$aboveL}{$belowL}"
        case InferenceMarker =>
          infMarkerUsed = true
          "#" + infMarkerPos
        case m =>
          logError("unexpected notation marker: " + m)
          "ERROR"
      }
      msS.mkString("")
    }
    private def doM(m: Marker) = doMarkers(List(m))

    private def doDelim(d: Delimiter) = {
      val dL = Common.translateDelim(d.expand(p, Nil).text)
      s"\\mmt@symref{${p.toPath}}{$dL}"
    }
  }

/*
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
*/
}

/**
 * Converts MMT to sTeX by producing
 * a smodule for each theory
 * containing a symdef definition for each constant.
 * Additionally, every MMT Document gets an informal version
 * realized in a sTeX file as well.
 * By using the OpaqueText String escape, LaTex can be embedded
 * in the MMT Document and gets passed through to the sTeX file.
 */
class ModuleGeneratingPresenter extends Presenter(new SymbolUsingPresenter) {
  def key = "mmt-stex"

  override val outExt = "tex"

  private val mathclassMap = UnicodeMap.readMap("unicode/latex-mathclass-map")

  def apply(structuralElement: StructuralElement, standalone: Boolean = false)(implicit rh: RenderingHandler): Unit = {
    structuralElement match {
      case theory: Theory => doTheory(theory, standalone)
      case declaration: Declaration => doDeclaration(declaration, standalone)
      case document: Document => doDocument(document, standalone)
      case _ =>
    }
  }

  private def doTheory(theory: Theory, standalone: Boolean)(implicit rh: RenderingHandler): Unit = {
    controller.simplifier(theory)
    if (standalone) {
      rh << "\\documentclass[12pt]{article}" + "\n"
      rh << "\\usepackage{stex}" + "\n"
      rh << "\\libinput{mmt-stex}" + "\n"
      rh << "\\begin{document}" + "\n"
      rh << "\n"
    }
    rh << s"\\begin{smodule}{${theory.name.toString}}" + "\n"
    theory.meta foreach { mPath =>
      doDeclaration(PlainInclude(mPath, theory.path), standalone)
    }
    theory.getDeclarationsElaborated.foreach { declaration =>
      doDeclaration(declaration, standalone)
    }
    rh << "\\end{smodule}" + "\n"
    if (standalone) {
      rh << "\n"
      rh << "\\end{document}"
      rh << "\n"
      rh << s"% Generated with mmt-stex from ${theory.parent.toString.replace("_", "\\_").replace(".omdoc", ".mmt")}" + "\n"
    }
  }

  private def doDeclaration(declaration: Declaration, standalone: Boolean)(implicit rh: RenderingHandler): Unit = {
    declaration match {
      case constant: Constant => doConstant(constant, standalone)
      case i @ Include(id) if standalone =>
        // skip redundant includes
        if (i.isGenerated)
          return
        // ignore includes of theories written in scala
        if (id.from.doc.uri.scheme contains "scala")
          return
        rh << doInclude(id.from, standalone) + "\n"
      case _ =>
    }
  }

  private def doInclude(mPath: MPath, standalone: Boolean): String = {
    val archive = controller.backend.findOwningArchive(mPath).getOrElse {
      logError("cannot find archive holding " + mPath + " (ignoring)")
      return "Could not build the Include String"
    }
    val archiveId = archive.archString
    val moduleName = mPath.name.toString
    val pLAbs = archive / outDim / "content" / archives.Archive.MMTPathToContentPath(mPath.mainModule).stripExtension
    val pLRel = archive.root.up.up.relativize(pLAbs)
    val pL = pLRel.segments.dropWhile(_ != "mmt-stex").mkString("/")
    val modulePath = pL.slice(0, pL.lastIndexOf("/")) + "?" + moduleName
    if (standalone){
      s"\\importmodule[$archiveId]{$modulePath}"
    }
    else {
      s"\\usemodule[$archiveId]{$modulePath}"
    }
  }

  private def doConstant(constant: Constant, standalone: Boolean)(implicit rh: RenderingHandler): Unit = {
    val constantName = constant.name.dropComplex.toString
    val macroName = Common.translateName(Common.escapeToCamelCase(constantName))

    var symdef = s"\\symdef{MMT$macroName}[name=$macroName"
    val (args, argsList, argsInOrderAsProvided) = doArgs(constant)

    if (argsList.isEmpty) {
      symdef += s"]{$macroName}"
    }
    else {
      val precedence = doPrecedence(constant)
      val notation = doNotation(constant, argsInOrderAsProvided)

      symdef += s"$args$precedence]{$notation}"
    }
    if (standalone) {
      rh << symdef + "\n"
    }
    else {
      rh << convertToInformal(constant, macroName, argsList) + "\\\\" + "\n"
    }
  }

  private def doArgs(constant: Constant): (String, List[String], List[Int]) = {
    val notation = constant.notC.getParseDefault
    notation match {
      case None => ("", List(), List())
      case Some(textNotation: TextNotation) =>
        var markers = textNotation.presentationMarkers
        if (markers.exists(marker => marker.isInstanceOf[ScriptMarker])) {
          markers = textNotation.parsingMarkers
        }
        val argNums = markers.collect {
          case arg: Arg => arg.number
          case seqArg: SeqArg => seqArg.number
          case variable: Var => variable.number
        }
        if (argNums.isEmpty) return ("", List(), List())
        val argsSorted = argNums.distinct.sorted
        val rankMapping = argsSorted.zipWithIndex.map { case (value, index) => value -> (index + 1) }.toMap
        val argsInOrderAsProvided = argNums.map(rankMapping)
        val argsPos = markers.collect {
          case _: Arg => "i"
          case _: SeqArg => "a"
          case variable: Var => if (variable.isSequence) "B" else "b"
        }
        val zippedPosAndNumArgs = argsInOrderAsProvided.zip(argsPos)
        val zippedSorted = zippedPosAndNumArgs.sortBy(_._1)
        val argsPosSortedList = zippedSorted.map(_._2)
        val argsPosSortedString = ",args=" + argsPosSortedList.mkString("")
        (argsPosSortedString, argsPosSortedList, argsInOrderAsProvided)
    }
  }

  // MMT: has a higher precedence (and thus a higher binding strength)
  // stex: The lower a precedence, the stronger a notation binds its arguments
  //       If symbol takes no args -> no prec (NOT prec=0 !!!)
  private def doPrecedence(constant: Constant): String = {
    val notation = constant.notC.getParseDefault
    notation match {
      case None => ""
      case Some(textNotation: TextNotation) =>
        val prec = textNotation.precedence.toString.toInt
        val stexPrec = -prec
        s",prec=${stexPrec.toString}"
    }
  }

  private def doNotation(constant: Constant, argsInOrderAsProvided: List[Int]): String = {
    val notation = constant.notC.getParseDefault
    notation match {
      case None => ""
      case Some(textNotation: TextNotation) =>
        var markers = textNotation.presentationMarkers
        if (markers.exists(marker => marker.isInstanceOf[ScriptMarker])) {
          markers = textNotation.parsingMarkers
        }

        val numDelimiter = markers.count(marker => marker.isInstanceOf[Delimiter])
        val useMainCompIdentifier = if (numDelimiter > 1) false else true

        var argCounter = 0
        markers.map {
          case _: Arg =>
            val argNum = argsInOrderAsProvided(argCounter)
            argCounter += 1
            s"#$argNum "
          case seqArg: SeqArg =>
            val argNum = argsInOrderAsProvided(argCounter)
            argCounter += 1
            val seqDelimiter = doDelimiter(seqArg.sep, constant.path, useMainCompIdentifier)
            s"\\argsep{#$argNum}{$seqDelimiter} "
          case delimiter: Delimiter =>
            val translatedDelimiter = doDelimiter(delimiter, constant.path, useMainCompIdentifier)
            translatedDelimiter
          case variable: Var =>
            val argNum = argsInOrderAsProvided(argCounter)
            argCounter += 1
            variable.sep match {
              case Some(delimiter) =>
                val variableSeqDelimiter = doDelimiter(delimiter, constant.path, useMainCompIdentifier)
                s"\\argsep{#$argNum}{$variableSeqDelimiter} "
              case None =>
                s"#$argNum "
            }
          case _ =>
        }.mkString
    }
  }

  private def doDelimiter(delimiter: Delimiter, constantGlobalName: GlobalName, useMainCompIdentifier: Boolean): String = {
    val translatedDelimiter = Common.translateDelim(delimiter.expand(constantGlobalName, Nil).text)
    var wrappedDelimiter = catchUnknownMacros(translatedDelimiter.trim)

    var hasMathClass = false
    mathclassMap.foreach { symbolClassPair =>
      if (symbolClassPair._1 == wrappedDelimiter) {
        if (useMainCompIdentifier) {
          wrappedDelimiter = s" ${symbolClassPair._2}{\\maincomp{$wrappedDelimiter}} "
        }
        else {
          wrappedDelimiter = s" ${symbolClassPair._2}{\\comp{$wrappedDelimiter}} "
        }
        hasMathClass = true
      }
    }
    if (!hasMathClass) {
      if (useMainCompIdentifier) {
        wrappedDelimiter = s" \\mathord{\\maincomp{$wrappedDelimiter}} "
      }
      else {
        wrappedDelimiter = s" \\mathord{\\comp{$wrappedDelimiter}} "
      }
    }
    wrappedDelimiter
  }

  // Some macros from unicode-latex-map do not get recognized, convert to a accepted equivalent
  private def catchUnknownMacros(delimiter: String): String = {
    delimiter match {
      case "\\der" => "\\rightassert"
      case "\\rA" => "\\rightarrow"
      case "\\lrA" => "\\leftrightarrow"
      case "\\lbracket" => "\\lbrack"
      case "\\rbracket" => "\\rbrack"
      case "\\{" => "\\lbrace"
      case "\\}" => "\\rbrace"
      case "↯" => "contra"
      case _ => delimiter
    }
  }

  private def convertToInformal(constant: Constant, constantName: String, argsList: List[String]): String = {
    val numArgs = argsList.length
    if (numArgs == 0) {
      // Check if constant has a type defined
      val constAnalyzedTerm = constant.tpC.getAnalyzedIfFullyChecked
      constAnalyzedTerm match {
        case Some(_) =>
          val constType = getConstants(constAnalyzedTerm.get).last.name.toString
          if (constType == "type")
            s"The symbol \\textbf{$constantName} represents a \\textcolor{violet}{$constType}. \\\\" + "\n" + s"Notation: $$\\MMT$constantName$$. \\\\" + "\n"
          else
            s"The constant \\textbf{$constantName} has the type: \\textcolor{violet}{$constType}. \\\\" + "\n" + s"Notation: $$\\MMT$constantName$$. \\\\" + "\n"
        case None =>
          s"The constant \\textbf{$constantName} has the Notation: $$\\MMT$constantName$$. \\\\" + "\n"
      }
    }
    else {
      val notationArgs = ('a' to 'z').slice(0, numArgs)
      val argsElaborated = argsList.zipWithIndex.map {
        case ("i", index) => s"\\textit{${notationArgs(index)}} is a simple Argument. \\\\"
        case ("a", index) => s"\\textit{${notationArgs(index)}} is an Argument Sequence. \\\\"
        case ("b", index) => s"\\textit{${notationArgs(index)}} is an Argument bound by the constant. \\\\"
        case ("B", index) => s"\\textit{${notationArgs(index)}} is an Argument Sequence bound by the constant. \\\\"
      }.mkString("\n")

      s"""
         |Let \\textbf{$constantName} be a constant with Arguments: \\textit{${notationArgs.mkString(", ")}} \n
         |such that: $$\\MMT$constantName${notationArgs.map(c => s"{$c}").mkString}$$. \n
         |$argsElaborated
         |""".stripMargin
    }
  }

  private def doDocument(document: Document, standalone: Boolean)(implicit rh: RenderingHandler): Unit = {
    if (standalone) {
      rh << "\\documentclass[12pt]{article}" + "\n"
      rh << "\\usepackage{stex}" + "\n"
      rh << "\\libinput{mmt-stex}" + "\n"
      rh << "\\begin{document}" + "\n"
      rh << "\n"
    }
    document.getDeclarations foreach {
      case nestedDocument: Document =>
        if (nestedDocument.level == SectionLevel || nestedDocument.level == SectionInModuleLevel){
          doSection(nestedDocument)
        }
        doDocument(nestedDocument, standalone = false)
      case dRef: DRef => controller.globalLookup.get(dRef.target) match {
        case document: Document =>
          doDocument(document, standalone = false)
        case _ =>
      }
      case mRef: MRef => controller.globalLookup.get(mRef.target) match {
        case theory: Theory =>
          rh << doInclude(mRef.target, standalone = false) + "\n"
          rh << s"\\begin{theorysection}{${theory.name}}" + "\n"
          doDocument(theory.asDocument, standalone = false)
          rh << "\\end{theorysection}" + "\n"
        case view: View => doDocument(view.asDocument, standalone = false)
        case _ =>
      }
      case sRef: SRef => controller.globalLookup.get(sRef.target) match {
        case declaration: Declaration => doDeclaration(declaration, standalone = false)
        case _ =>
      }
      case opaqueText: OpaqueText => doOpaque(opaqueText.text)
      case _ =>
    }
    if (standalone) {
      rh << s"\\MMTsTeXfootnote{${document.path.toString.replace("_", "\\_").replace(".omdoc", ".mmt")}}" + "\n"
      rh << "\n"
      rh << "\\end{document}" + "\n"
    }
  }

  private def doSection(section: Document)(implicit rh: RenderingHandler): Unit = {
    val sectionLabel = section.name.toString
    val sectionTitle = NarrativeMetadata.title.get(section).getOrElse(sectionLabel)
    rh << s"\\section*{$sectionTitle} \\label{$sectionLabel}" + "\n"
  }

  private def doOpaque(textFragment: TextFragment)(implicit rh: RenderingHandler): Unit = {
    textFragment match {
      case stringFragment: StringFragment => doStringFragment(stringFragment, doNotEscape = false, newlineAfter = true)
      case objFragment: ObjFragment => doObjFragment(objFragment)
      case scopeFragment: ScopeFragment => doScopeFragment(scopeFragment)
    }
  }

  private def doStringFragment(stringFragment: StringFragment, doNotEscape: Boolean, newlineAfter: Boolean)(implicit rh: RenderingHandler): Unit = {
    val fragmentValue = if (doNotEscape) {
      stringFragment.value
    } else {
      stringFragment.value.replace("_", "\\_")
    }
    val resultString = Common.translateOpaqueStringFragment(fragmentValue)
    if (newlineAfter) rh << resultString + "\n" else rh << resultString
  }

  private def doObjFragment(objFragment: ObjFragment)(implicit rh: RenderingHandler): Unit = {
    val objAnalyzed = objFragment.tc.getAnalyzedIfFullyChecked.get
    objAnalyzed match {
      case term: Term =>
        apply(term, None)
      case _ =>
    }
  }

  private def doScopeFragment(scopeFragment: ScopeFragment)(implicit rh: RenderingHandler): Unit = {
    val doNotEscape = scopeFragment.body.head match {
      case StringFragment(value) => value.startsWith("\\begin{lstlisting}")
      case _ => false
    }

    scopeFragment.body.foreach {
      case stringFragment: StringFragment => doStringFragment(stringFragment, doNotEscape, newlineAfter = false)
      case objFragment: ObjFragment => doObjFragment(objFragment)
      case scopeFragment: ScopeFragment => doScopeFragment(scopeFragment)
    }

    rh << "\n"
  }
}

/**
 * convert a term into expressions processable by sTeX
 *
 * Used for [[ModuleGeneratingPresenter]]
 */
class SymbolUsingPresenter extends ObjectPresenter {
  def apply(o: Obj, origin: Option[CPath])(implicit rh : RenderingHandler): Unit = {
    val con = origin match {
      case Some(CPath(p: ContentPath,_)) => Context(p.module)
      case _ => Context.empty
    }
    rh << "$" + doObj(o)(con) + "$"
  }

  private def removeImplicit(arity: Arity, termArgs: List[Term]): (List[ArgumentComponent], List[Term]) = {
    val implArgs = arity.flatImplicitArguments(arity.length)
    if (implArgs.isEmpty) {
      return (arity.arguments, termArgs)
    }
    val indexedArgs = arity.arguments.zipWithIndex
    val argsToKeep = indexedArgs.collect {
      case (arg, index) if !implArgs.contains(arg) => index
    }
    val arityArgsWithoutImpl = argsToKeep.map(arity.arguments)
    val termArgsWithoutImpl = argsToKeep.map(termArgs)
    (arityArgsWithoutImpl, termArgsWithoutImpl)
  }

  /** translates an MMT name into a sTeX symbol name
   *  some subtleties of complex steps are still ignored, but the resulting names should be unique in most cases
   */
  private def doObj(obj: Obj)(implicit context: Context): String = {
    val objS = obj match {
      case OMS(p) => doSymbolName(p)
      case OMV(n) => n.toPath
      case t @ ComplexTerm(_, _, _, _) =>
        val tS = controller.pragmatic.makePragmatic(t)(p => Presenter.getNotations(controller, p, true)) match {
          case None =>
            val ComplexTerm(p, subs, con, args) = t
            Presenter.getNotations(controller, p, true).headOption match {
              case Some(not) =>
                doComplexWithNotation(t, p, subs, con, args, not)
              case None =>
                doComplexDefault(p, subs, con, args)
            }
          case Some(tP) =>
            val PragmaticTerm(p, subs, con, args, not, _) = tP
            doComplexWithNotation(t, p, subs, con, args, not)
        }
        tS
      case l: OMLITTrait =>
        l.toString
      case o: OML =>
        doObj(o.vd)
      case VarDecl(n, _, tp, df, _) =>
        val nL = n.toPath
        val (tpLatex,tpInfText) = tp match {
          case None => ("","")
          case Some(t) =>
            val tL = doObj(t)
            val inferred = parser.SourceRef.get(t).isEmpty
            val tT = if (inferred) controller.presenter.asString(t) else ""
            (tL,tT)
        }
        val dfL = df match {
          case None => ""
          case Some(t) => doObj(t)
        }
        // if the type was inferred, we add its text rendering as an optional argument (e.g., to display as a tooltip)
        nL
      case s: Sub =>
        doObj(VarDecl(s.name, df = s.target))
      case t =>
        logError("unexportable: " + t)
        "\\MMTHelpererror{unknown object}"
    }
     objS
  }

  private def doComplexWithNotation(strict: Term, p: GlobalName, subs: Substitution, con: Context, args: List[Term], not: TextNotation)
                                   (implicit context: Context): String = {
    val arity = not.arity
    val (arityArgsWithoutImpl, termArgsWithoutImpl) = removeImplicit(arity, args)
    val arityCopyWithoutImpl = Arity(not.arity.subargs, not.arity.variables, arityArgsWithoutImpl)
    val subsG = arityCopyWithoutImpl.groupArgs(subs.map(_.target), true)
    val conG = arityCopyWithoutImpl.groupVars(con)
    val argsG = arityCopyWithoutImpl.groupArgs(termArgsWithoutImpl, false)
    var res = doSymbolName(p)
    def append(l: Seq[String]): Unit = {
      res += l.mkString("{",",","}")
    }
    subsG.foreach {a =>
      val aS = a map doObj
      append(aS)
    }
    var extCon = context
    conG.foreach {c =>
      val cS = c.variables map {vd =>
        val r = doObj(vd)(extCon)
        extCon ++= vd
        r
      }
      append(cS)
    }
    argsG.foreach {a =>
      val aS = a map {x =>
        doObj(x)(extCon)}
      append(aS)
    }
    res
  }
  private def doComplexDefault(p: GlobalName, subs: Substitution, con: Context, args: List[Term])(implicit context: Context): String = {
    val name = doSymbolName(p)
    val subsS = (subs map doObj).mkString(",")
    val conS = (con mapVarDecls {case (vdCon,vd) => doObj(vd)(context++vdCon)}).mkString(",")
    val argsCon = context ++ con
    val argsS = (args map {a => doObj(a)(argsCon)}).mkString(",")
    s"\\MMTHelpercomplex{$name}{$subsS}{$conS}{$argsS}"
  }
}

/**
 * convert a term into a LaTeX expression (using lots of \ { and })
 * 
 * the LaTeX is relative to the macros generated by [[MacroGeneratingPresenter]] 
 */
class MacroUsingPresenter extends ObjectPresenter {
  def apply(o: Obj, origin: Option[CPath])(implicit rh : RenderingHandler): Unit = {
    val con = origin match {
      case Some(CPath(p: ContentPath,_)) => Context(p.module)
      case _ => Context.empty
    }
    rh << doObj(o)(con)
  }

  /** translates an MMT name into a LaTeX name
   *  some subtleties of complex steps are still ignored, but the resulting names should be unique in most cases
   */
  private def doObj(obj: Obj)(implicit context: Context): String = {
    val objS = obj match {
      case OMS(p) => doConstantName(p)
      case OMV(n) => n.toPath
      case t @ ComplexTerm(_, _, _, _) =>
        val tS = controller.pragmatic.makePragmatic(t)(p => Presenter.getNotations(controller, p, true)) match {
          case None =>
            val ComplexTerm(p, subs, con, args) = t
            Presenter.getNotations(controller, p, true).headOption match {
              case Some(not) =>
                doComplexWithNotation(t, p, subs, con, args, not)
              case None =>
                doComplexDefault(p, subs, con, args)
            }
          case Some(tP) =>
            val PragmaticTerm(p, subs, con, args, not, _) = tP
            doComplexWithNotation(t, p, subs, con, args, not)
        }
        "\\mmt@group{" + tS + "}"
      case l: OMLITTrait =>
        "\\mmt@lit{" + l.toString + "}"
      case o: OML =>
        doObj(o.vd) 
      case VarDecl(n, _, tp, df, _) =>
        val nL = n.toPath
        val (tpLatex,tpInfText) = tp match {
          case None => ("","")
          case Some(t) =>
            val tL = doObj(t)
            val inferred = parser.SourceRef.get(t).isEmpty
            val tT = if (inferred) controller.presenter.asString(t) else ""
            (tL,tT)
        }
        val dfL = df match {
          case None => ""
          case Some(t) => doObj(t)
        }
        // if the type was inferred, we add its text rendering as an optional argument (e.g., to display as a tooltip)
        s"{\\mmt@vardecl[$tpInfText]{$nL}{$tpLatex}{$dfL}}"
      case s: Sub =>
        doObj(VarDecl(s.name, df = s.target))
      case t =>
        logError("unexportable: " + t)
        "\\mmt@error{unknown object}"
    }
    objS
  }

  private def doComplexWithNotation(strict: Term, p: GlobalName, subs: Substitution, con: Context, args: List[Term], not: TextNotation)
                                   (implicit context: Context): String = {
    val arity = not.arity
    val subsG = arity.groupArgs(subs.map(_.target), true)
    val conG = arity.groupVars(con)
    val argsG = arity.groupArgs(args, false) 
    var res = doConstantName(p)
    def append(l: Seq[String]): Unit = {
      res += l.mkString("{",",","}")
    }
    subsG.foreach {a =>
      val aS = a map doObj
      append(aS)
    }
    var extCon = context
    conG.foreach {c =>
      val cS = c.variables map {vd =>
        val r = doObj(vd)(extCon)
        extCon ++= vd
        r
      }
      append(cS)
    }
    argsG.foreach {a =>
      val aS = a map {x => doObj(x)(extCon)}
      append(aS)
    }
    // add the inferred type as a final argument
    if (not.markers exists {m => m.atomicDescendants contains InferenceMarker}) {
      val strictI = checking.Solver.infer(controller, context, strict, None) match {
         case Some(tp) =>
            doObj(tp)
         case None =>
            "\\mmt@error{type inference failed}"
      }
      res += "{" + strictI + "}"
    }
    res
  }
  private def doComplexDefault(p: GlobalName, subs: Substitution, con: Context, args: List[Term])(implicit context: Context): String = {
    val name = doConstantName(p)
    val subsS = (subs map doObj).mkString(",")
    val conS = (con mapVarDecls {case (vdCon,vd) => doObj(vd)(context++vdCon)}).mkString(",")
    val argsCon = context ++ con
    val argsS = (args map {a => doObj(a)(argsCon)}).mkString(",")
    s"\\mmt@complex{$name}{$subsS}{$conS}{$argsS}"
  }
}
