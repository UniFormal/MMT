package info.kwarc.mmt.jedit.proofgui

import info.kwarc.mmt.api.gui.Swing
import info.kwarc.mmt.api.objects.Stack
import info.kwarc.mmt.api.proving.itp.{HasError, ProofUtil, WarningMsg}
import info.kwarc.mmt.jedit.proofgui.util.GenInteractiveProof
import info.kwarc.mmt.lf.itp.{InteractiveProof, Tactic, TacticError}
import org.gjt.sp.jedit.gui.{HistoryTextArea, HistoryTextField}
import org.gjt.sp.jedit.{Abbrevs, View}

import java.awt.event.{ActionEvent, ActionListener, KeyEvent}
import java.awt.{BorderLayout, Dimension}
import java.util
import java.util.Calendar
import javax.swing._

/**
  * the Java-swing that is used as a containter for concret gui implementations like the proof display
  * @param view the current jedit view
  * @param position the position of the dockable inside of jedit (not actively used but required by jedit)
  */
class MMTProofStateDockable (view: View, position: String) extends JTabbedPane {


  val tacticsproof = new TacticsProof(view)
  tacticsproof.setDividerLocation(0.5)
  tacticsproof.setResizeWeight(0.5)
  addTab("Tactics Proof", tacticsproof)


}

/**
  * TacticsProof implements a concrete gui for the MMTproofStateDockable.
  * It consists of three parts: the top display containing the proof state [[textarea0]] ,
  * the input bar at the bottom [[tacticsinput]] and the middle tabbed display for different kinds of outputs [[tabs]]
  * @param view the current jedit view
  */
class TacticsProof(view : View) extends JSplitPane(SwingConstants.HORIZONTAL){

  /** the concrete InteractiveProof instance used for proof management by the GUI
    *
    */
  var ip : Option[InteractiveProof] = None

  /** abbrevs contains all the ascii to unicode translations used for the input bar
    * this is used to translate an ascii sequence into an unicode using the MMT-abbreviations (f.ex. jrA for the big
    * unicode right arrow, in latex it would be \Rightarrow)
    */
  val abbrevs: util.Hashtable[String, String] = Abbrevs.getModeAbbrevs.get("mmt")


  val top = new JPanel()
  val bottom = new JPanel()
  /**
    * tabs used in the bottom panel
    */
  val tabs = new JTabbedPane()



  top.setLayout(new BorderLayout())
  bottom.setLayout(new BorderLayout())
  add(top)
  add(bottom)

  // not writeable textarea (in the top half of the proof gui) used to display the proof state
  val txtarea0= new JTextArea()
  txtarea0.setLineWrap(true)
  txtarea0.setPreferredSize(new Dimension(top.getWidth, top.getHeight))
  txtarea0.setEditable(false)

  // the bottom text area is used
  val btmtxt = new JTextArea()
//  btmtxt.setLineWrap(true)
//  btmtxt.setPreferredSize(new Dimension(bottom.getWidth, bottom.getHeight))



  val scrolltxt = new JScrollPane(btmtxt)
 // scrolltxt.setPreferredSize(new Dimension(bottom.getWidth, bottom.getHeight))
  scrolltxt.setVisible(true)

  // the bar at the very bottom where one enters the tactics
  val tacticsinput =  new HistoryTextField("tactics input") // new JTextField(30)
  tacticsinput.setEnterAddsToHistory(true)


  bottom.add(tacticsinput, BorderLayout.SOUTH)

  bottom.add(tabs ,BorderLayout.CENTER)
  tabs.addTab("output" , scrolltxt)

  val tacticsHistory = new JTextArea()


  tabs.addTab("input" , tacticsHistory)

  val history = new HistoryTextArea("internal mmt history")

  // tabs.addTab("history" , history)

  val proofTermTxt  = new JTextArea()
  proofTermTxt.setLineWrap(true)

  tabs.addTab("proof term", proofTermTxt )

  top.add(txtarea0, BorderLayout.CENTER)


  val jtb0 = new JToolBar()



  val proofbtn : JButton = Swing.Button("Proof", "Proof current term") {
    ip = GenInteractiveProof.genInteractiveProof(view)
    if (ip.isDefined){val tmp = ip.get.pr.proofStateToString(ip.get.slvr); txtarea0.setText(tmp) ; btmtxt.setText("")}
  }


  val undobtn : JButton = Swing.Button("Undo", "Undo the last step"){if (ip.isDefined) {ip.get.undo; val tmp = ip.get.pr.proofStateToString(ip.get.slvr); txtarea0.setText(tmp) ; ip.get.stepsS.remove(ip.get.stepsS.length - 1); tacticsHistory.setText(ip.get.stepsS.mkString(";\n"))} }
  val genProofTermBtn : JButton = Swing.Button("Proof Term (S)" , "Generate and display the proof term (simplified)"){
    if (ip.isDefined){
      val slvr = ip.get.slvr
      proofTermTxt.setText(slvr.presentObj(ip.get.slvr.simplify(ip.get.pr.proofTerm)(Stack(ip.get.slvr.checkingUnit.context), ip.get.hist)))
      //   val tmp = slvr.asInstanceOf[ExtendedSolver].presentBracketed(ip.get.slvr.simplify(ip.get.pr.proofTermAlt)(Stack(ip.get.slvr.checkingUnit.context), ip.get.hist))
    }
  }
  val genProofTermBtnAlt : JButton = Swing.Button("Proof Term" , "Generate and display the proof term (unsimplified)"){
    if (ip.isDefined){
      val slvr = ip.get.slvr
      proofTermTxt.setText(slvr.presentObj(ip.get.pr.proofTermAlt))
      //  val tmp = Presenter.bracket()
    }
  }

  val genProofTermBtnBrack : JButton = Swing.Button("Proof Term (SB)" , "Generate and display the proof term (simplified with brackets)"){
    if (ip.isDefined){
      val slvr = ip.get.slvr
      val tmp = ProofUtil.presentBracketed(ip.get.slvr.controller , ip.get.slvr.report  , ip.get.slvr.simplify(ip.get.pr.proofTerm)(Stack(ip.get.slvr.checkingUnit.context), ip.get.hist))
      proofTermTxt.setText(tmp)
    }
  }

  val genProofTermBtnBrackAlt : JButton = Swing.Button("Proof Term (B)" , "Generate and display the proof term (unsimplified with brackets)"){
    if (ip.isDefined){
      val slvr = ip.get.slvr
      // proofTermTxt.setText(slvr.presentObj(ip.get.slvr.simplify(ip.get.pr.proofTermAlt)(Stack(ip.get.slvr.checkingUnit.context), ip.get.hist)))
      val tmp = ProofUtil.presentBracketed(ip.get.slvr.controller,ip.get.slvr.report, (ip.get.pr.proofTermAlt))
      proofTermTxt.setText(tmp)
    }
  }


  val  testrunbutton : JButton = Swing.Button("Test" , "treats this proof as a test run (less checks, no proof term generation, useful for debugging and using experimental features)"){
    if (ip.isDefined){
      ip.get.testrun = !ip.get.testrun
      btmtxt.append("is testrun:  " + ip.get.testrun.toString + "\n")
      // proofTermTxt.setText(slvr.presentObj(ip.get.slvr.simplify(ip.get.pr.proofTermAlt)(Stack(ip.get.slvr.checkingUnit.context), ip.get.hist)))

    }
  }

  val saveProofTermBtn : JButton = Swing.Button("Save Proof" , "Save the proof (this will only save the proof steps)"){}
  val loadProofTermBtn : JButton = Swing.Button("Load Proof" , "Load the proof (this will only load the proof steps, if any are available)"){}
  val helpBtn : JButton = Swing.Button("Displays Help" , "Show help for several topics like \"which tactics are available\""){}
  val hist : JButton = Swing.Button("Print History" , "Print the internal mmt history"){
    if (ip.isDefined){
      val ipp = ip.get
      history.setText(ipp.hist.toString)
    }
  }
  jtb0.add(proofbtn)
  jtb0.add(undobtn)
  jtb0.add(genProofTermBtn)
  jtb0.add(genProofTermBtnAlt)
  jtb0.add(testrunbutton)


  jtb0.add(genProofTermBtnBrack)
  jtb0.add(genProofTermBtnBrackAlt)


  top.add(jtb0 , BorderLayout.NORTH)


  tacticsinput.addActionListener(new tacticsinputaction)


  /** prints an error to the output panel (including the date)
    *
    * @param s the string to be printed
    */
  def printError(s : String): Unit ={
    val time =  Calendar.getInstance().getTime
    val times = time.toString
    val errorstring = times + "\n-----------------\n\n" + s
    btmtxt.setText(errorstring)
  }

  /**
    * contains all input triggers, restricted to the input bar
    */
  val imap: InputMap = tacticsinput.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT)
  /**
    * contains all actions that can be called by the input action map. This includes the unicode completion for the input bar
    */
  val amap: ActionMap = tacticsinput.getActionMap
  // trigger the unicode completion when the space bar is pressed
  imap.put(KeyStroke.getKeyStroke(KeyEvent.VK_SPACE,0,false) , "unic")

  // add the unicode completion to the action map
  amap.put("unic" , new unicodecomplete)

  /**
    * class used for the unicode completion in the input bar (not the source code area)
    */
  class unicodecomplete extends AbstractAction {
    /**
      * handler that gets called when the space bar is pressed in the input bar
      * @param actionEvent ignored
      */
    override def actionPerformed(actionEvent: ActionEvent): Unit = {
      // get the text from the input bar
      val txt = tacticsinput.getText()
      // position of the caret in the input bar
      val i = tacticsinput.getCaretPosition
      // represents the start of the string that is to be transformed into an unicode character
      var start = i
      // find the start of the word by going back until the start of the input bar or a whitespace is found
      while(0 < start && !  txt(start - 1).isWhitespace){
        start -= 1
      }
      if (start != i){
        val sub = txt.substring(start , i)
        if(abbrevs.containsKey(sub)){
          val res = abbrevs.get(sub)
          val hd = txt.take(start)
          val tl = txt.drop(i)
          tacticsinput.setText(hd + res + tl)
          val newpos = start + res.length  //  + (if (start != 0)  -1 else 0 )
          tacticsinput.setCaretPosition(newpos)
        }
      }
    }
  }

  /**
    * class called when processing input from the input bar (by pressing enter)
    */
  class tacticsinputaction extends ActionListener{


    /**
      * processes the input bar text by first parsing it and if sucessfull, executing the parsed tactic
      * @param actionEvent ignored
      */
    override def actionPerformed(actionEvent: ActionEvent): Unit = {
      if (ip.isEmpty) return

      val tmp = tacticsinput.getText

      tacticsinput.setText("")
      //get the tactics parser
      val tp = ip.get.tp
      val ipp = ip.get
      // save the current proof state in the history
      ipp.saveCurrState




      val pr  = try { tp.parseAll(tp.multiTacticPA, tmp)} catch {
        case TacticError(s) =>     tp.Error(s , null)
        case e =>   tp.Error("error while parsing: " + e.toString , null)
      }
      pr match {
        case tp.Success(result, next) =>{
          ip.get.executeStep(result.asInstanceOf[Tactic]) match  {
            case HasError(s) => {
              printError(s)
            }
            case WarningMsg(s) => {printError(s) ; ip.get.stepsS.append(tmp)}
            case _ => ip.get.stepsS.append(tmp)
          }
          val st = ip.get.pr.proofStateToString(ip.get.slvr)
          txtarea0.setText(st)
          val tmp0 = ip.get.stepsS.map(_.toString).mkString(";\n")
          tacticsHistory.setText(tmp0)

        }
        case tp.Failure(msg,nxt) => printError(msg) ; ipp.undo
        case tp.Error(msg, nxt) => printError(msg) ; ipp.undo
      }
    }
  }

}
