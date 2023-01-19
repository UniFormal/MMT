package info.kwarc.mmt.api.refactoring

import java.awt.event.{ActionEvent, ActionListener}
import java.awt.{BorderLayout, ComponentOrientation, Container, Dimension, FlowLayout}
import java.io.PrintWriter

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives.{Archive, BuildTarget,Build}
import info.kwarc.mmt.api.modules.{Theory, View}
import info.kwarc.mmt.api.objects.{Context, OMID, Term, Traverser}
import info.kwarc.mmt.api.symbols.{Constant, Declaration, PlainInclude}
import info.kwarc.mmt.api.utils.FilePath
import javax.swing._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Created by michael on 15.05.17.
  */
class GraphOptimizationTool extends BuildTarget {
  var printErrors = true      //print Errors on error stream, otherwise not at all
  var predictFuture = true    //declarations used in future lite code are counted as used by optimization
  var ignoreUnion = true      //Unions are not optimized if true
  var protectStructures = true //protect structures by never optimizing their dependencies

  /*variable for interactive mode*/
  var command : String = ""
  val dialogue = new Dialogue(this)

  private def printError(string : String): Unit = {
    if (printErrors) Console.err.println(string)
  }


  /** Returns theory inclusions
    *
    * This method returns a theory's inclusions, by looking at its direct inclusions and then being applied recursively to the included theories
    * @param theoryPath This is the path of the theory to be analyzed
    * @param replacementmap This is a map containing inclusion replacements for each theory
    * @return This is a list of all inclusions, including indirect
    */
  protected def includes(theoryPath : MPath,
                         replacementmap : mutable.HashMap[MPath, mutable.HashMap[Path, mutable.HashSet[MPath]]] = mutable.HashMap[MPath, mutable.HashMap[Path, mutable.HashSet[MPath]]]()
                        ) : mutable.HashSet[MPath] = {
    var ret = mutable.HashSet[MPath]()
    try controller.getTheory(theoryPath)
    catch {
      case e : Exception =>
        printError("Error:" + e.toString + ", while looking up includes of " + theoryPath + "(skipped)")
        return ret
    }
    try {
      ret ++= (controller.getTheory(theoryPath).meta match {case Some(meta)  => includes(meta, replacementmap) case _ => None}) ++= controller.getTheory(theoryPath).meta
    } catch {
      case e : GetError => printError("Error:" + e.toString + ", while looking up includes of " + theoryPath + "(skipped)")
    }
    for (include <- directIncludes(theoryPath, replacementmap)) {
      ret += include
      try {
        ret ++= includes(include, replacementmap)
      } catch {
        case e : GetError => printError("Error:" + e.toString + ", while looking up includes of " + theoryPath + "(skipped)")
      }
    }
    ret
  }

  /** Returns direct inclusions
    *
    * This method returns a theory's direct inclusions by analyzing its content.
    * It takes into account changes to the graph using replacementmap.
    * @param theoryPath This is the path of the theory to be analyzed
    * @param replacementmap This is a map containing inclusion replacements for each theory
    * @return This is a list of all direct inclusions
    */
  protected def directIncludes (theoryPath : MPath,
                                replacementmap : mutable.HashMap[MPath, mutable.HashMap[Path, mutable.HashSet[MPath]]]
                               ) : mutable.HashSet[MPath] = {
    var ret = mutable.HashSet[MPath]()
    try {
      controller.getTheory(theoryPath)
    }
    catch {
      case e : GetError =>
        printError("Error:" + e.toString + ", while looking up direct includes of " + theoryPath + "(skipped)")
        return ret
    }
    val theory : Theory = controller.get(theoryPath).asInstanceOf[Theory]
    try {
      val replacement = replacementmap(theoryPath)
      for (declaration <- theory.getPrimitiveDeclarations) {
        declaration match {
          case PlainInclude(from, _) =>
            if (!replacement.contains(from)) {
              ret += from
            }
            else {
              ret ++= replacement(from)
            }
          case _ => ()
        }
      }
    } catch {
      case _ : java.util.NoSuchElementException =>
        for (declaration <- theory.getPrimitiveDeclarations) {
          declaration match {
            case PlainInclude(from, _) =>
              ret += from
            case _ => ()
          }
        }
    }
    ret
  }

  /** Sorts theories
    *
    * Sort theories in topological order, with innermost theories coming first.
    * @param theories This is an Iterable of theories to be sorted
    * @return This is a sorted List of theories
    */
  protected def sortTheories (theories: Iterable[MPath], map : mutable.HashMap[MPath, mutable.HashMap[Path, mutable.HashSet[MPath]]]) : List[MPath] = {
    /*set of already sorted theories for quick check*/
    var sorted = mutable.HashSet[MPath]()
    /*actually sorted list*/
    var orderedTheories = ListBuffer[MPath]()
    /*set of theories still to be sorted*/
    var unsorted = mutable.HashSet[MPath]()
    unsorted ++= theories
    /* insert until sorted */
    var change = true
    while (!unsorted.equals(mutable.HashSet.empty) && change) {
      change = false
      for (theoryPath <- unsorted) {
        /*cycle through unsorted until theory is found with all dependencies (in optimization scope) sorted*/
        if (includes(theoryPath, map).intersect(unsorted).isEmpty) {
          orderedTheories += theoryPath
          unsorted -= theoryPath
          sorted += theoryPath
          change = true
        }
      }
    }
    orderedTheories.toList
  }

  protected def getDependencies(mpath : MPath,
                                replacementmap : mutable.HashMap[MPath, mutable.HashMap[Path, mutable.HashSet[MPath]]] = mutable.HashMap[MPath, mutable.HashMap[Path, mutable.HashSet[MPath]]]()
                               ) : mutable.HashSet[MPath] = {
    try {
      controller.get(mpath) match {
        case dt: Theory =>
          var res = mutable.HashSet.empty[MPath]
          res ++= includes(mpath, replacementmap)
          res ++= dt.getNamedStructures.map({ struct => struct.from.toMPath })
          dt.getNamedStructures.foreach({ struct => res ++= getDependencies(struct.from.toMPath, replacementmap) })
          res
        case vw: View =>
          mutable.HashSet[MPath]() += vw.from.toMPath += vw.to.toMPath
      }
    } catch {
      case e : Error => {
        printError("Error:" + e.toString + ", while looking up dependencies of " + mpath + "(skipped)")
        mutable.HashSet[MPath]()
      }
    }
  }

  protected def transitiveClosure(theories : mutable.HashSet[MPath]): mutable.HashSet[MPath] = {
    var closure = mutable.HashSet[MPath]()
    val archives = controller.backend.getArchives
    for (archive <- archives) {
      archive.allContent foreach {
        path =>
          if (theories.contains(path)) {
            closure += path
          }
          else{
              for (dep <- getDependencies(path)) {
                if (theories.contains(dep)) {
                  closure += path
                }
              }
        }
      }
    }
    closure
  }

  /** Traverser finding used theories
    *
    * This object is a traverser and searches a theory for all theories that are used
    */
  protected object FindUsedTheories extends Traverser[mutable.HashSet[MPath]] {
    /** Traverses terms
      *
      * Traverses over terms, finding any used theories
      * @param t This is the current subterm
      * @param con This is the current context
      * @param state This is the traverser's state
      * @return
      */
    def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
      // look for URIs
      case OMID(path) =>
        // cut path to module path
        state.add(path.module)
        OMID(path)

      // in all other cases, traverse
      case t =>
        Traverser(this, t)
    }

    def apply(t: Term, state: State): Term = apply(t, state, Context())

    /** Applies to Declaration
      *
      * Searches a Declaration for its used theories, adds them to state
      * @param decl This is the Declaration to be searched
      * @param state This is the traverser's state
      */
    def apply(decl: Declaration, state: State): Unit = {
      decl match {
        case PlainInclude(from, to) =>
        case c: Constant =>
          c.df match {
            case Some(t) => apply(t, state)
            case _ =>
          }
          c.tp match {
            case Some(t) => apply(t, state)
            case _ =>
          }
        case _ =>
      }
    }

    /** Applies to Theory
      *
      * Searches a Theory for its used theories, adds them to state
      * @param dt This is the Theory to be searched
      * @return This is a Set of used theories (as MPaths)
      */
    def apply(dt: Theory): State = {
      val state: State = mutable.HashSet[MPath]()
      dt.getDeclarations.foreach(FindUsedTheories(_, state))
      state
    }
    def apply(vw: View): State = {
      val state: State = mutable.HashSet[MPath]()
      vw.getDeclarations.foreach(FindUsedTheories(_, state))
      state
    }

    def apply(se: StructuralElement): State = {
      se match {
        case vw : View => apply(vw)
        case theory : Theory => apply(theory)
      }
    }
  }

  /** Find simply redundant inclusions
    *
    * This method finds inclusions that are redundant, since they are already part of a different inclusion
    * @param theoryPath This is the path of the theory to be optimized
    * @param replacementmap This is a map containing inclusion replacements for each theory
    * @return This is a list containing the suggested removals for the theory
    */
  def findRedundantIncludes(theoryPath : MPath,
                            replacementmap : mutable.HashMap[MPath, mutable.HashMap[Path, mutable.HashSet[MPath]]] = mutable.HashMap[MPath, mutable.HashMap[Path, mutable.HashSet[MPath]]]()
                           ) : List[Path] = {
    var ret = ListBuffer[Path]()
    var subIncludes = mutable.HashSet[MPath]()
    for (include <- directIncludes(theoryPath, replacementmap)) {
      for (subInclude <- includes(include, replacementmap)) {
        subIncludes += subInclude
      }
    }
    for (directInclude <- directIncludes(theoryPath, replacementmap)) {
      if (subIncludes.contains(directInclude)) {
        ret += directInclude
      }
    }
    ret.toList
  }

  /** Finds superfluous inclusions
    *
    * This method optimizes a theory by reducing its inclusions to those used inside the theory
    * @param theoryPath This is the path of the theory to be optimized
    * @param replacementmap This is a map containing inclusion replacements for each theory
    * @param futureUse This is a map containing used theories in the futureLite-code
    * @return This is a map containing the suggested replacements for the theory
    */
  def findUnusedIncludeReplacements(theoryPath : MPath,
                                    replacementmap : mutable.HashMap[MPath, mutable.HashMap[Path, mutable.HashSet[MPath]]] = mutable.HashMap[MPath, mutable.HashMap[Path, mutable.HashSet[MPath]]](),
                                    futureUse : mutable.HashMap[MPath, mutable.HashSet[MPath]] = mutable.HashMap[MPath, mutable.HashSet[MPath]]()
                                   ) : mutable.HashMap[Path, mutable.HashSet[MPath]] = {
    /* replacements will map the replacement suggestions for each optimization candidate
    *  theory inclusions that can be removed entirely will receive an empty set*/
    var replacements : mutable.HashMap[Path, mutable.HashSet[MPath]] = new mutable.HashMap[Path, mutable.HashSet[MPath]]

    val theory : Theory = controller.get(theoryPath).asInstanceOf[Theory]
    var usedTheories : mutable.HashSet[MPath] = FindUsedTheories(theory)
    if (ignoreUnion && usedTheories.isEmpty) return replacements
    if (predictFuture) usedTheories ++= futureUse(theoryPath)

    /* Find candidates for optimization.
    *  Every directly included theory from which there is no used symbol can be optimized in at least some way.*/
    var optimizationCandidates = mutable.HashSet[MPath]()
    optimizationCandidates ++= directIncludes(theoryPath, replacementmap)
    optimizationCandidates --= usedTheories

    /*Find replacement for every candidate*/
    for (optimizationCandidate <- optimizationCandidates) {
      /* Replacement strategy depends on whether
      *  candidate.includes intersects usedTheories
      *  is a subset of
      *  (usedTheories intersects directIncludes).includes
      *  */

      /* find candidate.includes intersects usedTheories */
      var candidateIncludes = mutable.HashSet[MPath]()
      candidateIncludes ++= includes(optimizationCandidate, replacementmap)
      candidateIncludes = candidateIncludes.intersect(usedTheories)

      /* find transitive closure of includes of used directIncludes*/
      var usedDirectIncludes = mutable.HashSet[MPath]()
      usedDirectIncludes ++= directIncludes(theoryPath, replacementmap)
      usedDirectIncludes = usedDirectIncludes.intersect(usedTheories)
      controller.get(optimizationCandidate).asInstanceOf[Theory].meta match {
        case Some(p : MPath) => usedDirectIncludes += p
        case None =>
      }

      var transitiveUsedIncludes = mutable.HashSet[MPath]()
      for (usedDirectInclude <- usedDirectIncludes) {
        transitiveUsedIncludes += usedDirectInclude
        transitiveUsedIncludes ++= includes(usedDirectInclude)
      }

      if (candidateIncludes.subsetOf(transitiveUsedIncludes)) {
        replacements.put(optimizationCandidate, new mutable.HashSet[MPath]())
      }
      else {
        var replacementIncludes = (mutable.HashSet[MPath]() ++=candidateIncludes)--=transitiveUsedIncludes
        var transitiveReplacementIncludes = mutable.HashSet[MPath]()
        for (replacementInclude <- replacementIncludes) {
          transitiveReplacementIncludes ++= includes(replacementInclude)
        }
        replacementIncludes --= transitiveReplacementIncludes
        replacements.put(optimizationCandidate, replacementIncludes)
      }
    }
    replacements
  }

  /** Optimizes multiple theories
    *
    * This method optimizes all given theories in the Iterable.
    * Boolean toggle for interactive mode.
    * @param theories Theories to be optimized as Iterable[MPath]
    * @return This is a map containing the suggested replacements for all analyzed theories
    */
  def findReplacements(theories: Iterable[MPath], interactive : Boolean) : mutable.HashMap[MPath, mutable.HashMap[Path, mutable.HashSet[MPath]]] = {
    var theorySet = mutable.HashSet[MPath]()
    for (theoryPath <- theories) {
      theorySet += theoryPath
    }
    var replacements = mutable.HashMap[MPath, mutable.HashMap[Path, mutable.HashSet[MPath]]]()
    var never = mutable.HashSet[Path]()
    var no = mutable.HashMap[MPath, mutable.HashSet[Path]]()
    var futureUse = mutable.HashMap[MPath, mutable.HashSet[MPath]]()
    var futureLight = sortTheories(transitiveClosure(theorySet), replacements)
    var futureStructure = mutable.HashSet[MPath]()
    if (protectStructures) for (future <- futureLight) {
      controller.get(future) match {
        case dt : Theory =>
          dt.getNamedStructures.foreach(
            {
              structure => futureStructure += structure.from.toMPath
            }
          )
        case _ =>
      }
    }

    /*start interactive window*/
    if (interactive) {
      dialogue.showWindow()
    }
    var changed = true
    while (changed) {
      changed = false
      /*Search future lite code for used theories*/
      for (theoryPath <- futureLight.reverse) {
        try {
          futureUse.put(theoryPath, FindUsedTheories(controller.get(theoryPath)))
        } catch {
          case _: Error => {
            printError("Error: while looking into Future " + theoryPath + "(skipped)")
            futureUse.put(theoryPath, mutable.HashSet[MPath]())
          }
        }
      }
      for (futurePath <- futureLight) {
        if (!theorySet.contains(futurePath)) for (theoryPath <- getDependencies(futurePath, replacements)) {
          if (theorySet.contains(theoryPath)) futureUse(theoryPath) ++= includes(theoryPath).intersect(futureUse(futurePath))
        }
      }

      /* find replacements */
      for (theoryPath <- sortTheories(theories, replacements).reverse) {
        if (protectStructures && futureStructure.contains(theoryPath)) {
          if (replacements.get(theoryPath).isEmpty) replacements.put(theoryPath, mutable.HashMap[Path, mutable.HashSet[MPath]]())
        }
        else try {
          /* remove unused includes */
          if (!interactive) replacements.put(theoryPath, findUnusedIncludeReplacements(theoryPath, replacements, futureUse))
          else {
            //TODO refactor this
            if (replacements.get(theoryPath).isEmpty) replacements.put(theoryPath, mutable.HashMap[Path, mutable.HashSet[MPath]]())
            //TODO untangle suggestions to avoid redundancy
            var suggestions = findUnusedIncludeReplacements(theoryPath, replacements, futureUse)
            for (key <- suggestions.keySet) {
              if (!never.contains(key) && !(no.get(theoryPath).isDefined && no(theoryPath).contains(key))) {
                command = "waiting"
                dialogue.suggest(theoryPath, key, suggestions(key))
                while (command == "waiting")
                  synchronized {
                    wait()
                  }

                if (command == "yes") {
                  replacements(theoryPath).put(key, suggestions(key))
                  changed = true
                }
                if (command == "no") {
                  if (no.get(theoryPath).isEmpty) no.put(theoryPath, mutable.HashSet[Path]())
                  no(theoryPath).add(key)
                }
                if (command == "never") never += key
                if (command == "later") changed=true //well not really, but it still could
              }
            }
          }
          /* remove redundant includes */
          for (redundant <- findRedundantIncludes(theoryPath, replacements)) {
            if (!interactive) replacements(theoryPath).put(redundant, mutable.HashSet[MPath]())
            else {
              if (!never.contains(redundant) && !(no.get(theoryPath).isDefined && no(theoryPath).contains(redundant))) {
                command = "waiting"
                dialogue.suggest(theoryPath, redundant, mutable.HashSet[MPath]())
                while (command == "waiting")
                  synchronized {
                    wait()
                  }

                if (command == "yes") {
                  replacements(theoryPath).put(redundant, mutable.HashSet[MPath]())
                  changed = true
                }
                if (command == "no") {
                  if (no.get(theoryPath).isEmpty) no.put(theoryPath, mutable.HashSet[Path]())
                  no(theoryPath).add(redundant)
                }
                if (command == "never") never += redundant.asInstanceOf[MPath]
                if (command == "later") changed=true //well not really, but it still could
              }
            }
          }
        } catch {
          case _: Error => {
            printError("Error: while optimizing " + theoryPath + " (skipped)")
            replacements.put(theoryPath, mutable.HashMap[Path, mutable.HashSet[MPath]]())
          }
        }
        if (predictFuture)
          for (include <- getDependencies(theoryPath, replacements).intersect(theorySet)) {
            futureUse(include) ++= futureUse(theoryPath)
          }
      }
    }
    /*stop interactive window*/
    if (interactive) {
      dialogue.done()
    }
    convertToInclusionURIs(replacements)
  }

  /** Optimizes all known theories
    *
    * This method optimizes all theories inside the onthology
    * @return This is a map containing the suggested replacements for all analyzed theories
    */
  def findReplacements(interactive : Boolean = false) : mutable.HashMap[MPath, mutable.HashMap[Path, mutable.HashSet[MPath]]] = {
    val theories = mutable.HashSet[MPath]()

    controller.backend.getArchives.foreach(archive => archive.allContent.foreach(
      mpath => try {
        controller.get(mpath) match {
          case dt : Theory => theories += mpath
          case _ =>
        }
      } catch {
        case e: Error =>
      }
    ))
    findReplacements(theories, interactive)
  }

  protected def convertToInclusionURIs (map : mutable.HashMap[MPath, mutable.HashMap[Path, mutable.HashSet[MPath]]]) : mutable.HashMap[MPath, mutable.HashMap[Path, mutable.HashSet[MPath]]] = {
    val converted = mutable.HashMap[MPath, mutable.HashMap[Path, mutable.HashSet[MPath]]]()
    for (theory <- map.keySet) {
      val replacements = map(theory)
      val convertedInner = mutable.HashMap[Path, mutable.HashSet[MPath]]()
      for (dec <- controller.getTheory(theory).getPrimitiveDeclarations) {
        dec match {
          case PlainInclude(from, _) =>
            if (replacements.keySet.contains(from)) convertedInner.put(dec.path, replacements(from))
          case _ =>
        }
      }
      converted.put(theory, convertedInner)
    }
    converted
  }

  /** Converts map to xml
    *
    * This method converts a given mapping as generated by findReplacements to an XML representation
    * @param map This is a map containing inclusion replacements for each theory
    * @return XML-String
    */
  def toXML(map : mutable.HashMap[MPath, mutable.HashMap[Path, mutable.HashSet[MPath]]]) : String = {
    val sb = new mutable.StringBuilder()
    sb ++= "<optimizations>\n"
    for (theoryPath <- map.keySet) {
      sb ++= replaceTheoryToXML(theoryPath, map)
    }
    sb ++= "</optimizations>\n"
    sb.toString
  }

  /** Xml conversion subroutine
    *
    * This method is a subroutine of allToXML
    * @param theoryPath
    * @param map This is a map containing inclusion replacements for each theory
    * @return XML-String, or empty if mapping is
    */
  protected def replaceTheoryToXML(theoryPath : MPath, map : mutable.HashMap[MPath, mutable.HashMap[Path, mutable.HashSet[MPath]]]) : String = {
    val sb = new mutable.StringBuilder()
    if (map(theoryPath).keySet.isEmpty) return ""
    sb ++= "<theory Path=" ++= "\"" ++= theoryPath.toString ++= "\"" ++= ">\n"
    sb ++= replaceInclusionToXML(map(theoryPath))
    sb ++= "</theory>\n"
    sb.toString
  }

  /** Converts replacement to xml
    *
    * This method converts a given mapping of replacements for a single theory to an XML representation
    * @param map This is a map containing inclusion replacements for one theory
    * @return XML-String
    */
  protected def replaceInclusionToXML(map : mutable.HashMap[Path, mutable.HashSet[MPath]]) : String = {
    var sb : mutable.StringBuilder = new mutable.StringBuilder()
    for (path <- map.keySet) {
      if (map(path).isEmpty)
        sb ++= "\t<removeInclusion Path=" ++= "\"" ++= path.toString ++= "\"" ++= "/>\n"
      else {
        sb ++= "\t<replaceInclusion Path=" ++= "\"" ++= path.toString ++= "\"" ++= ">\n"
        for (theoryPath <- map(path)) sb ++= "\t\t<replacement Path=" ++= "\"" ++= theoryPath.toString ++= "\"/" ++= ">\n"
        sb ++= "\t</replaceInclusion>\n"
      }
    }
    sb.toString
  }

  protected class Dialogue(parent : GraphOptimizationTool) extends JFrame("Styler") {
    val got = parent
    /*panels*/
    val suggestionPanel = new JScrollPane()
    val controls = new JPanel()

    /*text label for displaying suggested changes*/
    val suggestionBox = new JLabel()

    /*control buttons*/
    val yesButton = new JButton("yes")
    yesButton.addActionListener(new ActionListener(){
      def actionPerformed(e : ActionEvent): Unit ={
        got.synchronized {
          got.command="yes"
          disableButtons()
          got.notify()
        }
      }
    })
    val noButton = new JButton("no")
    noButton.addActionListener(new ActionListener(){
      def actionPerformed(e : ActionEvent): Unit ={
        got.synchronized {
          got.command="no"
          disableButtons()
          got.notify()
        }
      }
    })
    val laterButton = new JButton("later")
    laterButton.addActionListener(new ActionListener(){
      def actionPerformed(e : ActionEvent): Unit ={
        got.synchronized {
          got.command="later"
          disableButtons()
          got.notify()
        }
      }
    })
    val neverButton = new JButton("never")
    neverButton.addActionListener(new ActionListener(){
      def actionPerformed(e : ActionEvent): Unit ={
        got.synchronized {
          got.command="never"
          disableButtons()
          got.notify()
        }
      }
    })

    addComponentsToPane(getContentPane)
    disableButtons()

    def addComponentsToPane(panel : Container) : Unit = {
      suggestionPanel.setLayout(new ScrollPaneLayout())
      controls.setLayout(new FlowLayout())

      //add suggestionBox
      suggestionPanel.setPreferredSize(new Dimension(500, 500))
      suggestionPanel.setViewportView(suggestionBox)

      //Add buttons
      controls.add(yesButton)
      controls.add(noButton)
      controls.add(neverButton)
      controls.add(laterButton)

      //Left to right component orientation is selected by default
      controls.setComponentOrientation(ComponentOrientation.LEFT_TO_RIGHT)

      //add panels
      panel.add(suggestionPanel, BorderLayout.CENTER)
      panel.add(controls,BorderLayout.SOUTH)
    }

    def showWindow() : Unit = {
      //display the window
      pack()
      setVisible(true)
    }

    def disableButtons() : Unit = {
      yesButton.setEnabled(false)
      noButton.setEnabled(false)
      neverButton.setEnabled(false)
      laterButton.setEnabled(false)
    }


    def done() : Unit = {
      disableButtons()
      setVisible(false)
    }

    def suggest(in : MPath, theoryPath : Path, replacements : mutable.HashSet[MPath]) : Unit = {
      /*display suggestion*/
      val sb = new mutable.StringBuilder()
      sb ++= "<html>"
      sb ++= "in:<br>"
      sb ++= in.toString ++= "<br>"
      if (replacements.isEmpty) {
        sb ++= "remove:<br>" ++= theoryPath.toString
      }
      else {
        sb ++= "replace:<br>" ++= theoryPath.toString
        sb ++= "<br>"
        sb ++= "with:"
        for (replacement <- replacements) {
          sb ++= "<br>" ++= replacement.toString
        }
      }
      sb ++= "</html>"
      suggestionBox.setText(sb.toString())

      /*enable buttons*/
      yesButton.setEnabled(true)
      noButton.setEnabled(true)
      neverButton.setEnabled(true)
      laterButton.setEnabled(true)

      /*resize window*/
      pack()
    }
  }

  /** a string identifying this build target, used for parsing commands, logging, error messages */
  override def key: String = "got"

  /** clean this target in a given archive */
  override def clean(a: Archive, in: FilePath): Unit = {
    val file = new java.io.File(a.root.toString + "/export/got/"+a.id+".xml")
    file.delete()
  }

  /** build or update this target in a given archive */
  def build(a: Archive, w: Build, in: FilePath, errorCont: Option[ErrorHandler]): Unit = {
    val theories = a.allContent.flatMap({p:MPath => controller.get(p) match { case dt : Theory => Some(p) case _ => None}})
    val replacements = findReplacements(theories, interactive = false)
    val output = toXML(replacements)
    val file = new java.io.File(a.root.toString + "/export/got/"+a.id+".xml")
    file.getParentFile.mkdirs
    val pw = new PrintWriter(file)
    pw.write(output)
    pw.close()
  }
}