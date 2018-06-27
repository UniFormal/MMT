package info.kwarc.mmt.got

import java.awt.event.{ActionEvent, ActionListener}
import java.awt.{BorderLayout, ComponentOrientation, Container, Dimension, FlowLayout}
import javax.swing._

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend.Extension
import info.kwarc.mmt.api.modules.{DeclaredTheory, View}
import info.kwarc.mmt.api.objects.{Context, OMID, Term, Traverser}
import info.kwarc.mmt.api.symbols.{Constant, Declaration, PlainInclude, Structure}

import scala.collection.mutable.{HashMap, HashSet, ListBuffer, StringBuilder}

/**
  * Created by michael on 15.05.17.
  */
class GraphOptimizationTool extends Extension {
  var printErrors = true      //print Errors on error stream, otherwise not at all
  var predictFuture = true    //declarations used in future lite code are counted as used by optimization
  var ignoreUnion = true      //Unions are not optimized if true
  var protectStructures = true //protect structures by never optimizing their dependencies

  /*variable for interactive mode*/
  var command : String = ""
  val dialogue = new Dialogue(this)


  /** Returns theory inclusions
    *
    * This method returns a theory's inclusions, by looking at its direct inclusions and then being applied recursively to the included theories
    * @param theoryPath This is the path of the theory to be analyzed
    * @param replacementmap This is a map containing inclusion replacements for each theory
    * @return This is a list of all inclusions, including indirect
    */
  protected def includes(theoryPath : MPath,
                         replacementmap : HashMap[MPath, HashMap[Path, HashSet[MPath]]] = HashMap[MPath, HashMap[Path, HashSet[MPath]]]()
                        ) : HashSet[MPath] = {
    var ret = HashSet[MPath]()
    try controller.getTheory(theoryPath)
    catch {
      case e : GetError =>
        if (printErrors) Console.err.println("Error:" + e.toString + ", while looking up includes of " + theoryPath + "(skipped)")
        return ret
    }
    try {
      ret ++= (controller.getTheory(theoryPath).meta match {case Some(meta)  => includes(meta, replacementmap) case _ => None}) ++= controller.getTheory(theoryPath).meta
    } catch {
      case e : GetError => if (printErrors) Console.err.println("Error:" + e.toString + ", while looking up includes of " + theoryPath + "(skipped)")
    }
    for (include <- directIncludes(theoryPath, replacementmap)) {
      ret += include
      try {
        ret ++= includes(include, replacementmap)
      } catch {
        case e : GetError => if (printErrors) Console.err.println("Error:" + e.toString + ", while looking up includes of " + theoryPath + "(skipped)")
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
                                replacementmap : HashMap[MPath, HashMap[Path, HashSet[MPath]]]
                               ) : HashSet[MPath] = {
    var ret = HashSet[MPath]()
    try controller.getTheory(theoryPath)
    catch {
      case e : GetError =>
        if (printErrors) Console.err.println("Error:" + e.toString + ", while looking up direct includes of " + theoryPath + "(skipped)")
        return ret
    }
    val theory : DeclaredTheory = controller.get(theoryPath).asInstanceOf[DeclaredTheory]
    try {
      val replacement = replacementmap.get(theoryPath).get
      for (declaration <- theory.getPrimitiveDeclarations) {
        declaration match {
          case PlainInclude(from, _) =>
            if (!replacement.contains(from)) {
              ret += from
            }
            else {
              ret ++= replacement.get(from).get
            }
          case _ => Unit
        }
      }
    } catch {
      case _ : java.util.NoSuchElementException =>
        for (declaration <- theory.getPrimitiveDeclarations) {
          declaration match {
            case PlainInclude(from, _) =>
              ret += from
            case _ => Unit
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
  protected def sortTheories (theories: Iterable[MPath], map : HashMap[MPath, HashMap[Path, HashSet[MPath]]]) : List[MPath] = {
    /*set of already sorted theories for quick check*/
    var sorted = HashSet[MPath]()
    /*actually sorted list*/
    var orderedTheories = ListBuffer[MPath]()
    /*set of theories still to be sorted*/
    var unsorted = HashSet[MPath]()
    unsorted ++= theories
    /* insert until sorted */
    var change = true
    while (!unsorted.equals(HashSet.empty) && change) {
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
    return orderedTheories.toList
  }

  protected def getDependencies(mpath : MPath,
                                replacementmap : HashMap[MPath, HashMap[Path, HashSet[MPath]]] = HashMap[MPath, HashMap[Path, HashSet[MPath]]]()
                               ) : HashSet[MPath] = {
    try {
      controller.get(mpath) match {
        case dt: DeclaredTheory =>
          var res: HashSet[MPath] = HashSet[MPath]()
          res ++= includes(mpath, replacementmap)
          res ++= dt.getNamedStructures.map({ struct => struct.from.toMPath })
          dt.getNamedStructures.foreach({ struct => res ++= getDependencies(struct.from.toMPath, replacementmap) })
          res
        case vw: View =>
          HashSet[MPath]() += vw.from.toMPath += vw.to.toMPath
      }
    } catch {
      case e : Error => {
        if (printErrors) Console.err.println("Error:" + e.toString + ", while looking up dependencies of " + mpath + "(skipped)")
        HashSet[MPath]()
      }
    }
  }

  protected def transitiveClosure(theories : HashSet[MPath]): HashSet[MPath] = {
    var closure = HashSet[MPath]()
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
    return closure
  }

  /** Traverser finding used theories
    *
    * This object is a traverser and searches a theory for all theories that are used
    */
  protected object FindUsedTheories extends Traverser[HashSet[MPath]] {
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

    /** Applies to DeclaredTheory
      *
      * Searches a DeclaredTheory for its used theories, adds them to state
      * @param dt This is the DeclaredTheory to be searched
      * @return This is a Set of used theories (as MPaths)
      */
    def apply(dt: DeclaredTheory): State = {
      val state: State = HashSet[MPath]()
      for (decl <- dt.getDeclarations) {
        FindUsedTheories(decl, state)
      }
      state
    }
    def apply(vw: View): State = {
      val state: State = HashSet[MPath]()
      for (decl <- vw.getDeclarations) {
        FindUsedTheories(decl, state)
      }
      state
    }

    def apply(se: StructuralElement): State = {
      se match {
        case vw : View => apply(vw)
        case theory : DeclaredTheory => apply(theory)
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
                            replacementmap : HashMap[MPath, HashMap[Path, HashSet[MPath]]] = HashMap[MPath, HashMap[Path, HashSet[MPath]]]()
                           ) : List[Path] = {
    var ret = ListBuffer[Path]()
    var subIncludes = HashSet[MPath]()
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
                                    replacementmap : HashMap[MPath, HashMap[Path, HashSet[MPath]]] = HashMap[MPath, HashMap[Path, HashSet[MPath]]](),
                                    futureUse : HashMap[MPath, HashSet[MPath]] = HashMap[MPath, HashSet[MPath]]()
                                   ) : HashMap[Path, HashSet[MPath]] = {
    /* replacements will map the replacement suggestions for each optimization candidate
    *  theory inclusions that can be removed entirely will receive an empty set*/
    var replacements : HashMap[Path, HashSet[MPath]] = new HashMap[Path, HashSet[MPath]]

    val theory : DeclaredTheory = controller.get(theoryPath).asInstanceOf[DeclaredTheory]
    var usedTheories : HashSet[MPath] = FindUsedTheories(theory)
    if (ignoreUnion && usedTheories.isEmpty) return replacements
    if (predictFuture) usedTheories ++= futureUse.get(theoryPath).get

    /* Find candidates for optimization.
    *  Every directly included theory from which there is no used symbol can be optimized in at least some way.*/
    var optimizationCandidates = HashSet[MPath]()
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
      var candidateIncludes = HashSet[MPath]()
      candidateIncludes ++= includes(optimizationCandidate, replacementmap)
      candidateIncludes = candidateIncludes.intersect(usedTheories)

      /* find transitive closure of includes of used directIncludes*/
      var usedDirectIncludes = HashSet[MPath]()
      usedDirectIncludes ++= directIncludes(theoryPath, replacementmap)
      usedDirectIncludes = usedDirectIncludes.intersect(usedTheories)
      controller.get(optimizationCandidate).asInstanceOf[DeclaredTheory].meta match {
        case Some(p : MPath) => usedDirectIncludes += p
        case None =>
      }

      var transitiveUsedIncludes = HashSet[MPath]()
      for (usedDirectInclude <- usedDirectIncludes) {
        transitiveUsedIncludes += usedDirectInclude
        transitiveUsedIncludes ++= includes(usedDirectInclude)
      }

      if (candidateIncludes.subsetOf(transitiveUsedIncludes)) {
        replacements.put(optimizationCandidate, new HashSet[MPath]())
      }
      else {
        var replacementIncludes = (HashSet[MPath]() ++=candidateIncludes)--=transitiveUsedIncludes
        var transitiveReplacementIncludes = HashSet[MPath]()
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
  def findReplacements(theories: Iterable[MPath], interactive : Boolean) : HashMap[MPath, HashMap[Path, HashSet[MPath]]] = {
    var theorySet = HashSet[MPath]()
    for (theoryPath <- theories) {
      theorySet += theoryPath
    }
    var replacements = HashMap[MPath, HashMap[Path, HashSet[MPath]]]()
    var never = HashSet[Path]()
    var no = HashMap[MPath, HashSet[Path]]()
    var futureUse = HashMap[MPath, HashSet[MPath]]()
    var futureLight = sortTheories(transitiveClosure(theorySet), replacements)
    var futureStructure = HashSet[MPath]()
    if (protectStructures) for (future <- futureLight) {
      controller.get(future) match {
        case dt : DeclaredTheory =>
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
            if (printErrors) Console.err.println("Error: while looking into Future " + theoryPath + "(skipped)")
            futureUse.put(theoryPath, HashSet[MPath]())
          }
        }
      }
      for (futurePath <- futureLight) {
        if (!theorySet.contains(futurePath)) for (theoryPath <- getDependencies(futurePath, replacements)) {
          if (theorySet.contains(theoryPath)) futureUse.get(theoryPath).get ++= includes(theoryPath).intersect(futureUse.get(futurePath).get)
        }
      }

      /* find replacements */
      for (theoryPath <- sortTheories(theories, replacements).reverse) {
        if (protectStructures && futureStructure.contains(theoryPath)) {
          if (replacements.get(theoryPath).isEmpty) replacements.put(theoryPath, HashMap[Path, HashSet[MPath]]())
        }
        else try {
          /* remove unused includes */
          if (!interactive) replacements.put(theoryPath, findUnusedIncludeReplacements(theoryPath, replacements, futureUse))
          else {
            //TODO refactor this
            if (replacements.get(theoryPath).isEmpty) replacements.put(theoryPath, HashMap[Path, HashSet[MPath]]())
            //TODO untangle suggestions to avoid redundancy
            var suggestions = findUnusedIncludeReplacements(theoryPath, replacements, futureUse)
            for (key <- suggestions.keySet) {
              if (!never.contains(key) && !(!no.get(theoryPath).isEmpty && no.get(theoryPath).get.contains(key))) {
                command = "waiting"
                dialogue.suggest(theoryPath, key, suggestions.get(key).get)
                while (command == "waiting")
                  synchronized {
                    wait()
                  }

                if (command == "yes") {
                  replacements.get(theoryPath).get.put(key, suggestions.get(key).get)
                  changed = true
                  //TODO apply change to source
                }
                if (command == "no") {
                  if (no.get(theoryPath).isEmpty) no.put(theoryPath, HashSet[Path]())
                  no.get(theoryPath).get.add(key)
                }
                if (command == "never") never += key
                if (command == "later") changed=true //well not really, but it still could
              }
            }
          }
          /* remove redundant includes */
          for (redundant <- findRedundantIncludes(theoryPath, replacements)) {
            if (!interactive) replacements.get(theoryPath).get.put(redundant, HashSet[MPath]())
            else {
              if (!never.contains(redundant) && !(!no.get(theoryPath).isEmpty && no.get(theoryPath).get.contains(redundant))) {
                command = "waiting"
                dialogue.suggest(theoryPath, redundant, HashSet[MPath]())
                while (command == "waiting")
                  synchronized {
                    wait()
                  }

                if (command == "yes") {
                  replacements.get(theoryPath).get.put(redundant, HashSet[MPath]())
                  changed = true
                  //TODO apply change to source
                }
                if (command == "no") {
                  if (no.get(theoryPath).isEmpty) no.put(theoryPath, HashSet[Path]())
                  no.get(theoryPath).get.add(redundant)
                }
                if (command == "never") never += redundant.asInstanceOf[MPath]
                if (command == "later") changed=true //well not really, but it still could
              }
            }
          }
        } catch {
          case _: Error => {
            if (printErrors) Console.err.println("Error: while optimizing " + theoryPath + " (skipped)")
            replacements.put(theoryPath, HashMap[Path, HashSet[MPath]]())
          }
        }
        if (predictFuture)
          for (include <- getDependencies(theoryPath, replacements).intersect(theorySet)) {
            futureUse.get(include).get ++= futureUse.get(theoryPath).get
          }
      }
    }
    /*stop interactive window*/
    if (interactive) {
      dialogue.done()
    }
    replacements
  }

  /** Optimizes all known theories
    *
    * This method optimizes all theories inside the onthology
    * @return This is a map containing the suggested replacements for all analyzed theories
    */
  def findReplacements(interactive : Boolean = false) : HashMap[MPath, HashMap[Path, HashSet[MPath]]] = {
    val theories = HashSet[MPath]()

    controller.backend.getArchives.foreach(archive => archive.allContent.foreach(
      mpath => try {
        controller.get(mpath) match {
          case dt : DeclaredTheory => theories += mpath
          case _ =>
        }
      } catch {
        case e: Error =>
      }
    ))
    findReplacements(theories, interactive)
  }

  /** Converts map to xml
    *
    * This method converts a given mapping as generated by findReplacements to an XML representation
    * @param map This is a map containing inclusion replacements for each theory
    * @return XML-String
    */
  def toXML(map : HashMap[MPath, HashMap[Path, HashSet[MPath]]]) : String = {
    var sb : StringBuilder = new StringBuilder()
    for (theoryPath <- map.keySet) {
      sb ++= replaceTheoryToXML(theoryPath, map)
    }
    return sb.toString
  }

  /** Xml conversion subroutine
    *
    * This method is a subroutine of allToXML
    * @param theoryPath
    * @param map This is a map containing inclusion replacements for each theory
    * @return XML-String, or empty if mapping is
    */
  protected def replaceTheoryToXML(theoryPath : MPath, map : HashMap[MPath, HashMap[Path, HashSet[MPath]]]) : String = {
    var sb : StringBuilder = new StringBuilder()
    if (map.get(theoryPath).get.keySet.isEmpty) return ""
    sb ++= "<theory MPath=" ++= "\"" ++= theoryPath.toString ++= "\"" ++= ">\n"
    sb ++= replaceInclusionToXML(map.get(theoryPath).get)
    sb ++= "</theory>\n"
    return sb.toString
  }

  /** Converts replacement to xml
    *
    * This method converts a given mapping of replacements for a single theory to an XML representation
    * @param map This is a map containing inclusion replacements for one theory
    * @return XML-String
    */
  protected def replaceInclusionToXML(map : HashMap[Path, HashSet[MPath]]) : String = {
    var sb : StringBuilder = new StringBuilder()
    for (path <- map.keySet) {
      if (map.get(path).get.isEmpty)
        sb ++= "\t<removeInclusion MPath=" ++= "\"" ++= path.toString ++= "\"" ++= ">\n"
      else {
        sb ++= "\t<replaceInclusion MPath=" ++= "\"" ++= path.toString ++= "\"" ++= ">\n"
        for (theoryPath <- map.get(path).get) sb ++= "\t\t<replacement MPath=" ++= "\"" ++= theoryPath.toString ++= "\"/" ++= ">\n"
        sb ++= "\t</replaceInclusion>\n"
      }
    }
    return sb.toString
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
      def actionPerformed(e : ActionEvent){
        got.synchronized {
          got.command="yes"
          disableButtons()
          got.notify()
        }
      }
    })
    val noButton = new JButton("no")
    noButton.addActionListener(new ActionListener(){
      def actionPerformed(e : ActionEvent){
        got.synchronized {
          got.command="no"
          disableButtons()
          got.notify()
        }
      }
    })
    val laterButton = new JButton("later")
    laterButton.addActionListener(new ActionListener(){
      def actionPerformed(e : ActionEvent){
        got.synchronized {
          got.command="later"
          disableButtons()
          got.notify()
        }
      }
    })
    val neverButton = new JButton("never")
    neverButton.addActionListener(new ActionListener(){
      def actionPerformed(e : ActionEvent){
        got.synchronized {
          got.command="never"
          disableButtons()
          got.notify()
        }
      }
    })

    addComponentsToPane(getContentPane())
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

    def suggest(in : MPath, theoryPath : Path, replacements : HashSet[MPath]) : Unit = {
      /*display suggestion*/
      var sb = new StringBuilder()
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
}